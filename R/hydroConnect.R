#' Connect to hydrology database
#'
#' This function exists to facilitate connecting to the hydrology database, especially if the database type and connection method changes in the future.
#'
#' @param path The path to the database. Currently supports only .sqlite databases.
#' @param timeout For sqlite DBs, the duration in which to retry an operation in milliseconds. Valid for the duration of the connection.
#'
#' @return A connection to the database.
#' @export
#'

hydroConnect <- function(path, timeout = 100000){

  if(!file.exists(path)){
    stop("The path you specified either does not exist or this computer does not have access to that drive.")
  }

  tryCatch({
    hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(hydro, "PRAGMA busy_timeout=100000")
    return(hydro)
  }, error = function(e){
    print("Connection failed.")
  })
}
