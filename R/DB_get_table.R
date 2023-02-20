#' Get data from the hydromet database
#'
#' @param path The path to the database, passed to hydroConnect. Default uses hydroConnect default path.
#' @param location The location code/id for the requested timeseries. Refer to DB_get_timeseries if unsure of location code.
#' @param parameter The parameter requested for the timeseries.  Refer to DB_get_timeseries if unsure of parameter spelling.
#' @param frequency One of "daily", "realtime", or "discrete".
#' @param start The start date or datetime of records requested (inclusive). Specify a Date or poSIXCT object, or a character vector of form "2022-01-01" or "2022-01-01 10:10:10". Set before timeseries start to get all records up to the end date/time.
#' @param end The end date or datetime of records requested (inclusive). Format as per 'start'. Set after timeseries end to get all records after the start date/time
#'
#' @return A data.frame containing the information requested.
#' @export
#'

DB_get_table <- function(path = "default", location, parameter, frequency, start, end) {

  if (length(location) != 1){
    stop("You can only request data from one location at a time.")
  }
  if (length(parameter) != 1){
    stop("You can only request data for one parameter at a time.")
  }
  if (length(frequency) != 1){
    stop("You can only request data for one frequency type at a time.")
  }

  DB <- hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(DB), add=TRUE)

  data <- DBI::dbGetQuery(DB, paste0("SELECT * FROM '", frequency, "' WHERE location = '", location, "' AND parameter = '", parameter, "' AND ", if(frequency == "daily") "date" else if (frequency == "datetime_UTC") "realtime" else if (frequency == "discrete") "sample_date", " BETWEEN '", start, "' AND '", end, "'"))

  if (nrow(data) > 0){
    return(data)
  } else {
    print("No records matched your inputs.")
  }
}
