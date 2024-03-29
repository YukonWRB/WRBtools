#' Get formatted timeseries data from Aquarius.
#'
#'@description
#' `r lifecycle::badge("stable")`
#'
#' Fetches and processes data downloaded from an Aquarius web-hosted server and returns it in a concise format. Note that any times returned by this function are in UTC.
#'
#'@details
#' To store login credentials in your .renviron file, call [usethis::edit_r_environ()] and enter your username and password as value pairs, as AQUSER="your username" and AQPASS="your password". The server should be entered at server="your_server_url".
#'
#'
#' @param loc_id The location ID, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `29EA001` or `YOWN-0804`.
#' @param ts_name The timeseries name, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `Wlevel_bgs.Calculated`.
#' @param start The first day or instant for which you want information, in UTC 0 timezone. You can specify a Date object, POSIXct object, or character vector of form yyyy-mm-dd or yyyy-mm-dd HH:mm:ss. If specifying date or POSIXct objects, the timezone attribute will be ignored. If only a date is specified it will be assigned the first moment of the day. Times requested prior to the actual timeseries start will be adjusted to match available data.
#' @param end The last day or instant for which you want information, in UTC 0. You can specify a Date object, POSIXct object, or character vector of form yyyy-mm-dd or yyyy-mm-dd HH:mm:ss. If specifying date or POSIXct objects, the timezone attribute will be ignored. If only a date is specified it will be assigned the last moment of the day. Times requested prior to the actual timeseries end will be adjusted to match available data.
#' @param login Your Aquarius login credentials as a character vector of two. Default pulls information from your .renviron file; see details.
#' @param server The URL for your organization's Aquarius web server. Default pulls from your .renviron file; see details.
#'
#' @return A list with four data.frames: station metadata; timeseries information consisting of datetimes, values, applicable grade and approval levels; approval level change summary; grade level change summary. IMPORTANT: all times in this returned list are in UTC.
#'
#' @export

aq_download <- function(loc_id,
                        ts_name,
                        start = "1950-01-01",
                        end = Sys.Date(),
                        login = Sys.getenv(c("AQUSER", "AQPASS")),
                        server = Sys.getenv("AQSERVER")
)
{

  source(system.file("scripts",  "timeseries_client.R", package = "WRBtools")) #This loads the code dependencies

  #Make a data.frame with grade numbers and meanings because AQ doesn't supply them
  grade_codes <- data.frame(code = c(-55,-50, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 10, 11, 12, 14, 15, 21, 30, 31, 99, 100, 101, 103, 105, 110, 115, 120, 124, 125, 130),
                            description = c("GW RECOVERY", "WL BLW", "HW-MISS", "MISSING DATA", "OBSTRUCT", "EST-WI", "Unusable", "Unspecified", "Undefined", "ICE", "E", "C", "B", "A", "do not use - Est. Poor", "do not use - Poor", "Qun(>15%)", "Qun(<15%)", "Qun(<7%)", "do not use - Fair", "do not use - Est. Good", "do not use - formerly Good", "do not use - HW-MISS", "MET MISSING", "MET FREEZE", "MET CUML-GAP", "MET POOR", "MET EST-EXTERNAL", "MET EST-GAP", "MET SNOW", "MET FILL-DUPL", "MET FAIR", "MET GOOD"))
  #Make the Aquarius configuration
  config = list(
    server = server,
    username=login[1],
    password=login[2],
    timeSeriesName=paste0(ts_name, "@", loc_id)
  )

  # Connect to Aquarius server
  timeseries$connect(config$server,
                     config$username,
                     config$password)
  on.exit(timeseries$disconnect())

  # Get the location metadata
  locationData = timeseries$getLocationData(loc_id)

  start <- as.character(start)
  if(nchar(start) == 10){
    start <- paste0(start, " 00:00:00")
  }
  start <- gsub(" ", "T", start)
  start <- paste0(start, "-00:00")

  end <- as.character(end)
  if (nchar(end) == 10){
    end <- paste0(end, " 23:59:59.9999999")
  }
  end <- gsub(" ", "T", end)
  end <- paste0(end, "-00:00")

  # Read corrected time-series data from Aquarius, format time series to POSIXct
  RawDL <- timeseries$getTimeSeriesCorrectedData(c(config$timeSeriesName), queryFrom = start, queryTo = end)

  metadata <- data.frame(attribute = c("Location Name", "TS name", "Identifier", "Location Type", "Latitude", "Longitude", "Elevation", "Elevation Units", "UTC Offset in Aquarius"),
                         value = c(locationData$LocationName, ts_name, locationData$Identifier, locationData$LocationType, locationData$Latitude, locationData$Longitude, locationData$Elevation, if (is.null(locationData$ElevationUnits)) "unspecified" else locationData$ElevationUnits, locationData$UtcOffset)
  )

  #Get the UTC offset so that times can be made to UTC
  offset <- as.numeric(substr(locationData$UtcOffset, 1, 3))

  #Make the basic timeseries
  ts <- data.frame(datetime = RawDL$Points$Timestamp,
                   value = RawDL$Points$Value$Numeric)

  # format times to POSIXct, fix offset
  ts$datetime <- as.POSIXct(ts$datetime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  ts$datetime <- ts$datetime - (offset*60*60)

  #format approvals, grade times
  approvals <- RawDL$Approvals
  approvals$DateAppliedUtc <- as.POSIXct(approvals$DateAppliedUtc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  approvals$StartTime <- as.POSIXct(approvals$StartTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  approvals$StartTime <- approvals$StartTime - (offset*60*60)
  approvals$EndTime <- as.POSIXct(approvals$EndTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  approvals$EndTime <- approvals$EndTime - (offset*60*60)
  colnames(approvals) <- c("level", "datetime_applied", "user", "description", "comment", "start_time", "end_time")
  approvals <- approvals[,c(1,4,6,7,3,5,2)]

  grades <- RawDL$Grades
  grades$StartTime <- as.POSIXct(grades$StartTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  grades$StartTime <- grades$StartTime - (offset*60*60)
  grades$EndTime <- as.POSIXct(grades$EndTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  grades$EndTime <- grades$EndTime - (offset*60*60)
  grades <- merge(grades, grade_codes, by.x = "GradeCode", by.y = "code")
  grades <- grades[,c(1,4,2,3)]
  colnames(grades) <- c("level", "description", "start_time", "end_time")


  #Add in grades and approval columns
  if (nrow(ts) > 0){
    ts <- ts[!duplicated(ts) , ] #In unknown circumstances, Aquarius spits out duplicate points.
    ts$grade_level <- NA
    ts$grade_description <- NA
    for (i in 1:nrow(grades)){
      if (min(ts$datetime) > grades$start_time[i]) { #if the grade is prior to the first ts point
        ts[ts$datetime == min(ts$datetime),]$grade_level <- grades$level[i]
        ts[ts$datetime == min(ts$datetime),]$grade_description <- grades$description[i]
      } else if (nrow(ts[ts$datetime == grades$start_time[i],]) !=0) { #if the times line up properly (are snapped to a point)
        ts[ts$datetime == grades$start_time[i],]$grade_level <- grades$level[i]
        ts[ts$datetime == grades$start_time[i],]$grade_description <- grades$description[i]
      } else if (which.min(abs(ts$datetime - grades$start_time[i])) != nrow(ts)){ #if the times do not line up with anything in ts (not snapped), but not after the ts end
        index <- which.min(abs(ts$datetime - grades$start_time[i])) + 1
        ts[index,]$grade_level <- grades$level[i]
        ts[index,]$grade_description <- grades$description[i]
      } # and if the last grade start is after then end of the ts, do nothing with it!
    }

    ts$approval_level <- NA
    ts$approval_description <- NA
    for (i in 1:nrow(approvals)){
      if (min(ts$datetime) > approvals$start_time[i]) { #if the approval is prior to the first ts point
        ts[ts$datetime == min(ts$datetime),]$approval_level <- approvals$level[i]
        ts[ts$datetime == min(ts$datetime),]$approval_description <- approvals$description[i]
      } else if (nrow(ts[ts$datetime == approvals$start_time[i],]) !=0) { #if the times line up properly (are snapped to a point)
        ts[ts$datetime == approvals$start_time[i],]$approval_level <- approvals$level[i]
        ts[ts$datetime == approvals$start_time[i],]$approval_description <- approvals$description[i]
      } else if (which.min(abs(ts$datetime - approvals$start_time[i])) != nrow(ts)){ #if the times do not line up with anything in ts (not snapped), but not after the ts end
        index <- which.min(abs(ts$datetime - approvals$start_time[i])) + 1
        ts[index,]$approval_level <- approvals$level[i]
        ts[index,]$approval_description <- approvals$description[i]
      } # and if the last approval start is after then end of the ts, do nothing with it!
    }
    ts <- tidyr::fill(ts, c(grade_level, grade_description, approval_level, approval_description), .direction = "down")
  }


  list <- list(metadata = metadata,
             timeseries = ts,
             approvals = approvals,
             grades = grades)

  return(list)

}
