#' Upload data to Aquarius
#'
#' This function bypasses the web GUI and allows you to append data to Aquarius directly.
#'
#' The parameter `data` should consist of a data.frame with two named columns: Value and Time. Units for the column `Value` are set according the units already in use for the timeserie, so it should only contain numbers compatible with this. The `Time` column should be formatted as.POSIXct in timezone UTC, keeping in mind that Aquarius will apply the station UTC offset. Failure to ensure the correct timezone of input data will result in offset points.
#'
#' To store login credentials in your .renviron profile, call usethis::edit_r_environ() and enter your username and password as value pairs, as AQUSER="your username" and AQPASS = "your password".
#'
#' @param loc_id The location ID, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `29EA001` or `YOWN-0804`.
#' @param ts_name The timeseries name, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `Wlevel_bgs.Calculated`.
#' @param data The data you wish to append to an existing timeseries. Must contain columns named `Value` and `Time`. Time must be in UTC as Aquarius applies the station offset. See details for more information.
#' @param start Not required; specify only if you with to overwrite existing data, as a POSIXct object. Inclusive, must be >= to the first data point in data. Timezones should match that of data$Time.
#' @param end Not required; specify only if you with to overwrite existing data, as a POSIXct object. Inclusive, must be <= to the final data point in data. Timezones should match that of data$Time.
#' @param login Your Aquarius login credentials as a character vector of two. Default pulls information from your .renviron profile; see details.
#' @param server The URL for your organization's Aquarius web server. Default is for the Yukon Water Resources Branch.
#'
#' @return Appends points to the Aquarius server.
#' @export

aq_upload <- function(loc_id,
                      ts_name,
                      data,
                      start = NULL,
                      end = NULL,
                      login = Sys.getenv(c("AQUSER", "AQPASS")),
                      server = "https://yukon.aquaticinformatics.net/AQUARIUS")
{

  #Check that data has correct column names
  if (!(all(c("Value", "Time") %in% names(data)))){
    stop("Your data.frame must contain columns labelled Value and Time. Case sensitive.")
  }

  data$Value[data$Value == ""] <- NA #Set blank spaces to NA
  data$Value[data$Value == " "] <- NA
  data$Value[data$Value == "NA"] <- NA
  data$Value[data$Value == "<NA>"] <- NA

  data <- na.omit(data) #Very important! Any NA data actually gets appended to AQ as a point that is then a PITA to overwrite.

  #Start with server connection
  source(system.file("scripts",  "timeseries_client.R", package = "WRBtools"))
  #Make the Aquarius configuration and connect
  config = list(
    server = server,
    username=login[1],
    password=login[2],
    timeSeriesName=paste0(ts_name, "@", loc_id),
    eventPeriodStartDay = start,
    eventPeriodEndDay = end
    )

  timeseries$connect(config$server,
                     config$username,
                     config$password)
  on.exit(timeseries$disconnect())

  #Then append Points.
  #Notes about how AQ handles timestamps: it doesn't. The server will take the data fed to it as if it was UTC, without considering the tz attribute, and applies the station offset to that value. Therefore times must be converted to UTC prior to being uploaded, even if the TZ attribute does not matter. Time data can be fed in as.POSIXct or as dateTtime.

  result <- timeseries$waitForCompletedAppendRequest(timeseries$appendPoints(config$timeSeriesName, data, start, end), 120) #This makes it wait up to 120 seconds to show the append result - long enough for even giant datasets.
  points_in_file <- nrow(data)

  now <- Sys.time()
  attr(now, "tzone") <- "UTC"

  output <- list(appended = result$NumberOfPointsAppended,
                 input = points_in_file)

  if (result$AppendStatus == "Completed"){
    cat("\n", paste0(crayon::bold$green("Your request was completed:\n"), result$NumberOfPointsAppended, " points were appended out of the ", points_in_file, " that were in the provided dataset.\nThe points were appended to the timeseries ", crayon::bold(ts_name), " at location ", crayon::bold(loc_id), "\n", now, " UTC"))
  } else {
    cat("\n", paste0(crayon::bold$red("Your request was not completed or had an irregular status:\n"), "The status returned was ", crayon::bold(result$AppendStatus), "\n", result$NumberOfPointsAppended, " points were appended out of ", points_in_file, " requested.\nThe target timeseries was ", crayon::bold(ts_name), " at location ", crayon::bold(loc_id), "\n", now, " UTC"))
    return(output)
  }
}
