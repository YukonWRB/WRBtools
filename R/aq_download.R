#' Get formatted timeseries data from Aquarius.
#'
#' Fetches and processes data downloaded from an Aquarius web-hosted server and returns it in a concise format. Note that any times returned by this function are in UTC.
#'
#' To store login credentials in your .renviron profile, call usethis::edit_r_environ() and enter your username and password as value pairs, as AQUSER="your username" and AQPASS = "your password".
#'
#'
#' @param loc_id The location ID, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `29EA001` or `YOWN-0804`.
#' @param ts_name The timeseries name, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `Wlevel_bgs.Calculated`.
#' @param start The first day for which you want information (local time) as a character vector. Whole days only. Times requested prior to the actual timeseries start will be adjusted to match available data.
#' @param end The last day for which you want information (local time) as a character vector. Whole days only. Times requested prior to the actual timeseries end will be adjusted to match available data.
#' @param login Your Aquarius login credentials as a character vector of two. Default pulls information from your .renviron profile; see details.
#' @param server The URL for your organization's Aquarius web server. Default is for the Yukon Water Resources Branch.
#' @return A list with four data.frames: station metadata; timeseries information consisting of timestamps, values, applicable grade and approval levels; approval level change summary; grade level change summary. Important: all times in this list are in UTC.
#'
#' @export

aq_download <- function(loc_id,
                        ts_name,
                        start = "1950-01-01",
                        end = as.character(Sys.Date()),
                        login = Sys.getenv(c("AQUSER", "AQPASS")),
                        server = "https://yukon.aquaticinformatics.net/AQUARIUS"
)
{

  source(system.file("scripts",  "timeseries_client.R", package = "WRBtools")) #This loads the code dependencies

  #Make a data.frame with grade numbers and meanings because AQ doesn't supply them
  grade_codes <- data.frame(code = c(-55,-50, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 10, 11, 12, 14, 15, 21, 30, 31, 99, 100, 101, 103, 105, 110, 115, 120, 124, 125, 130),
                            description = c("GW RECOVERY", "WL BLW", "HW-MISS", "MISSING DATA", "OBSTRUCT", "EST-WI", "Unusable", "Unspecified", "Undefined", "ICE", "E", "C", "B", "A", "do not use - Est. Poor", "do not use - Poor", "Qun(>15%)", "Qun(<15%)", "Qun(<7%)", "do not use - Fair", "do not use - Est. Good", "do not use - formerly Good", "do not use - HW-MISS", "MET MISSING", "MET FREEZE", "MET CUML-GAP", "MET POOR", "MET EST-EXTERNAL", "MET EST-GAP", "MET SNOW", "MET FILL-DUPL", "MET FAIR", "MET GOOD"))
  #Make the Aquarius configuration
  config = list(
    # Aquarius server credentials
    server=server, username=login[1], password=login[2],
    # time series name@location EX: Wlevel_btoc.Calculated@YOWN-XXXX
    timeSeriesName=paste0(ts_name, "@", loc_id),
    # Analysis time period
    eventPeriodStartDay = start,
    eventPeriodEndDay = end)

  # Connect to Aquarius server
  timeseries$connect(config$server, config$username, config$password)
  on.exit(timeseries$disconnect())

  # Get the location metadata
  locationData = timeseries$getLocationData(loc_id)

  # Deal with times
  utcOffset = timeseries$getUtcOffsetText(locationData$UtcOffset)
  startOfDay = "T00:00:00"
  endOfDay = "T23:59:59.9999999"

  # Prepare for downloading data points based on specified period start and end or for all data points
  fromPeriodStart = paste0(config$eventPeriodStartDay, startOfDay, utcOffset)
  toPeriodEnd = paste0(config$eventPeriodEndDay, endOfDay, utcOffset)
  periodLabel = sprintf("%s - %s", config$eventPeriodStartDay, config$eventPeriodEndDay)

  # Read corrected time-series data from Aquarius, format time series to POSIXct
  RawDL <- timeseries$getTimeSeriesCorrectedData(c(config$timeSeriesName), queryFrom = fromPeriodStart, queryTo = toPeriodEnd)

  metadata <- data.frame(attribute = c("Location Name", "TS name", "Identifier", "Location Type", "Latitude", "Longitude", "Elevation", "Elevation Units", "UTC Offset in Aquarius"),
                         value = c(locationData$LocationName, ts_name, locationData$Identifier, locationData$LocationType, locationData$Latitude, locationData$Longitude, locationData$Elevation, locationData$ElevationUnits, locationData$UtcOffset)
  )

  #Get the UTC offset so that times can be made to UTC
  offset <- as.numeric(substr(utcOffset, 1, 3))

  #Make the basic timeseries
  ts <- data.frame(timestamp_UTC = RawDL$Points$Timestamp,
                           value = RawDL$Points$Value$Numeric)

  # format times to POSIXct, fix offset
  ts$timestamp_UTC <- as.POSIXct(ts$timestamp_UTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  ts$timestamp_UTC <- ts$timestamp_UTC - (offset*60*60)

  #format approvals, grade times
  approvals <- RawDL$Approvals
  approvals$DateAppliedUtc <- as.POSIXct(approvals$DateAppliedUtc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  approvals$StartTime <- as.POSIXct(approvals$StartTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  approvals$StartTime <- approvals$StartTime - (offset*60*60)
  approvals$EndTime <- as.POSIXct(approvals$EndTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  approvals$EndTime <- approvals$EndTime - (offset*60*60)
  colnames(approvals) <- c("level", "datetime_applied_UTC", "user", "description", "comment", "start_time_UTC", "end_time_UTC")
  approvals <- approvals[,c(1,4,6,7,3,5,2)]

  grades <- RawDL$Grades
  grades$StartTime <- as.POSIXct(grades$StartTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  grades$StartTime <- grades$StartTime - (offset*60*60)
  grades$EndTime <- as.POSIXct(grades$EndTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  grades$EndTime <- grades$EndTime - (offset*60*60)
  grades <- merge(grades, grade_codes, by.x = "GradeCode", by.y = "code")
  grades <- grades[,c(1,4,2,3)]
  colnames(grades) <- c("level", "description", "start_time_UTC", "end_time_UTC")


  #Add in grades and approval columns
  ts$grade_level <- NA
  ts$grade_description <- NA
  for (i in 1:nrow(grades)){
    if (min(ts$timestamp_UTC) > grades$start_time_UTC[i]){ #if the grade start time is in the TS; it might be well before just to cover everything.
      ts[ts$timestamp_UTC == min(ts$timestamp_UTC),]$grade_level <- grades$level[i]
      ts[ts$timestamp_UTC == min(ts$timestamp_UTC),]$grade_description <- grades$description[i]
    } else if (nrow(ts[ts$timestamp_UTC == grades$start_time_UTC[i],]) !=0) { #if the times line up properly (are snapped to a point)
      ts[ts$timestamp_UTC == grades$start_time_UTC[i],]$grade_level <- grades$level[i]
      ts[ts$timestamp_UTC == grades$start_time_UTC[i],]$grade_description <- grades$description[i]
    } else { #if the times do not line up properly (not snapped)
      index <- which.min(abs(ts$timestamp_UTC - grades$start_time_UTC[i]))
      ts[index,]$grade_level <- grades$level[i]
      ts[index,]$grade_description <- grades$description[i]
    }
  }

  ts$approval_level <- NA
  ts$approval_description <- NA
  for (i in 1:nrow(approvals)){
    if (min(ts$timestamp_UTC) > approvals$start_time_UTC[i]){ #if the grade start time is in the TS; it might be well before just to cover everything.
      ts[ts$timestamp_UTC == min(ts$timestamp_UTC),]$approval_level <- approvals$level[i]
      ts[ts$timestamp_UTC == min(ts$timestamp_UTC),]$approval_description <- approvals$description[i]
    } else if (nrow(ts[ts$timestamp_UTC == grades$start_time_UTC[i],]) !=0) { #if the times line up properly (are snapped to a point)
      ts[ts$timestamp_UTC == approvals$start_time_UTC[i],]$approval_level <- approvals$level[i]
      ts[ts$timestamp_UTC == approvals$start_time_UTC[i],]$approval_description <- approvals$description[i]
    } else { #if the times do not line up properly (not snapped)
      index <- which.min(abs(ts$timestamp_UTC - approvals$start_time_UTC[i]))
      ts[index,]$approval_level <- approvals$level[i]
      ts[index,]$approval_description <- approvals$description[i]
    }
  }

  ts <- tidyr::fill(ts, c(grade_level, grade_description, approval_level, approval_description), .direction = "down")

  list <- list(metadata = metadata,
             timeseries = ts,
             approvals = approvals,
             grades = grades)

  return(list)

}
