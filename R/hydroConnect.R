#' Connect to hydrology database
#'
#' This function exists to facilitate connecting to the hydrology database, especially if the database type and connection method changes in the future. Cannot be used to create a new database, use DBI::dbConnect with the appropriate driver (e.g. RSQlite::SQLite()) for that purpose.
#'
#' @param path The path to the database. Currently supports only .sqlite databases. 'default' points to //env-fs/env-data/corp/water/Common_GW_SW/Data/database/hydro.sqlite.
#' @param timeout For sqlite DBs, the duration in which to retry an operation in milliseconds. Valid for the duration of the connection.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#' @export
#'

hydroConnect <- function(path = "default", timeout = 100000, silent = FALSE){

  if (path == "default"){
    path <- "//env-fs/env-data/corp/water/Common_GW_SW/Data/database/hydro.sqlite"
  }

  if(!file.exists(path)){
    stop("The path you specified either does not exist or this computer does not have access to that drive. Read the function help file if you're trying to create a new DB.")
  }

  tryCatch({
    hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(hydro, "PRAGMA busy_timeout=100000")
    if (!silent){
      print("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(hydro)
  }, error = function(e){
    stop("Connection failed.")
  })
}
