#' YOWNraw
#'
#' Export of raw data from aquarius to csv, writes 3 data frames for raw data, compensated data, and manually corrected data
#'
#' @param AQID YOWN location
#' @param saveTo Directory in which the data will be saved. Can specify "desktop" to automatically create YOWN ID folder on desktop as save directory.
#' @param login Aquarius username and password, taken from Renviron files
#' @param AQTSServerID Aquarius server ID
#'
#' @return Writes three csv files containing YOWN data in the specified directory.
#' @export


YOWNraw <- function(AQID,
                    dateRange = "all",
                    saveTo = "desktop",
                    login = Sys.getenv(c("AQUSER", "AQPASS")),
                    AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS",
                    filename = "leveldata_RAW.csv"){

  # # Debug and development params. Leave as comments.
  # AQID = "YOWN-1925"
  # timeSeriesID = "Wlevel_Hgt.level_RAW"
  # saveTo = "desktop"
  # login = Sys.getenv(c("AQUSER", "AQPASS"))
  # AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS"

  #### Setup ####
  # Deal with file save location
  if(tolower(saveTo) == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop")
  }

  #### Download time series data from Aquarius, preliminary formatting ####
  # Download data from Aquarius
  for(i in c("Wlevel_Hgt.level_RAW", "Wlevel_Hgt.Compensated", "Wlevel_btoc.Calculated")){
    datalist <- suppressMessages(WRBtools::aq_download(loc_id = AQID,
                                                       ts_name = i))

    # Unlist time series data
    timeseries <- datalist$timeseries

    # Replace all grades below C with "Redacted"
    timeseries$grade_description[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "MISSING DATA"] <- "REDACTED"

    # Change timestamps from UTC to MST
    attr(timeseries$timestamp_UTC , "tzone") <- "MST"
    names(timeseries)[names(timeseries) == "timestamp_UTC"] <- "timestamp_MST"

    # final format, write to .csv
    fulldf <- timeseries %>%
      dplyr::select(c("timestamp_MST", "value", "grade_level", "grade_description"))
    write.csv(fulldf, paste0(saveTo, "/", AQID, "_", i, ".csv"), row.names = FALSE)
  }

}

