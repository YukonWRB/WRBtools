#' Get information on timeseries contained in the database
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Wondering what's in the database? This function helps you see what's under the hood, with an eye to helping you create a query for function [DB_get_ts()]. Leaving all NULL defaults will show you every timeseries in the database.
#'
#' @param path The path to the database, passed to [hydroConnect()]. Default uses hydroConnect default path.
#' @param operator Narrow by location operator if you wish (one or more). Exact spelling only!
#' @param location Narrow by location if you wish (one or more). Exact spelling only!
#' @param type Narrow by type if you wish (one or more). Exact spelling only!
#' @param parameter Narrow by parameter if you wish (one or more). Exact spelling only!
#'
#' @seealso [DB_get_ts()] for extracting timeseries data from the database, or, for spatial data, [DB_browse_spatial()] and [DB_get_spatial()] to browse and extract spatial data. For location metadata use [DB_get_meta()].
#'
#' @return A data.frame containing the database timeseries matching the function parameters.
#' @export
#'

DB_browse_ts <- function(path = "default", operator = NULL, location = NULL, type = NULL, parameter = NULL) {

  #TODO: Make this function work with multiple selection criteria, so 2+ locations.
  DB <- hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(DB), add=TRUE)

  timeseries <- DBI::dbGetQuery(DB, "SELECT * FROM timeseries")

  if (!is.null(operator)){
    timeseries <- timeseries[timeseries$operator %in% operator , ]
  }
  if (!is.null(location)){
    timeseries <- timeseries[timeseries$location %in% location , ]
  }
  if (!is.null(type)){
    timeseries <- timeseries[timeseries$type %in% type , ]
  }
  if(!is.null(parameter)){
    timeseries <- timeseries[timeseries$parameter %in% parameter , ]
  }

  if (nrow(timeseries) > 0){
    return(timeseries)
  } else {
    print("No records matched your inputs.")
  }
}
