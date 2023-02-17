#' Get timeseries information from the hydromet database
#'
#' Wondering what's in the database? This function helps you see what's under the hood, with an eye to helping you create a query for function get_DB_data. Leaving all NULL defaults will show you every timeseries in the database.
#'
#' @param path The path to the database, passed to hydroConnect. Default uses hydroConnect default path.
#' @param operator Narrow by location operator if you wish. Exact spelling only!
#' @param location Narrow by location if you wish. Exact spelling only!
#' @param type Narrow by type if you wish. Exact spelling only!
#' @param parameter Narrow by parameter if you wish. Exact spelling only!
#'
#' @return A data.frame containing the database timeseries matching the function parameters.
#' @export
#'

DB_get_timeseries <- function(path = "default", operator = NULL, location = NULL, type = NULL, parameter = NULL) {

  DB <- hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(DB), add=TRUE)

  timeseries <- DBI::dbGetQuery(DB, "SELECT * FROM timeseries")

  if (!is.null(operator)){
    timeseries <- timeseries[timeseries$operator == operator , ]
  }
  if (!is.null(location)){
    timeseries <- timeseries[timeseries$location == location , ]
  }
  if (!is.null(type)){
    timeseries <- timeseries[timeseries$type == type , ]
  }
  if(!is.null(parameter)){
    timeseries <- timeseries[timeseries$parameter == parameter , ]
  }

  return(timeseries)
}
