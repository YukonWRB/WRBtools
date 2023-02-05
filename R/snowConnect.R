#' Connect to snow survey database
#'
#' If the Snow Survey database is migrated to an SQLite database, beware that the tables and table columns might no longer be what you're expecting!
#'
#' @param path The path to the database. Currently supports either a Microsoft Access database or an .sqlite database.
#'
#' @return A connection to the database.
#' @export
#'

snowConnect <- function(path){

  #Check the paths and make a connection
  if(!file.exists(path)){
    stop("The path you specified either does not exist or this computer does not have access to that drive.")
  }
  if(stringr::str_detect(path, ".mdb")){
    snowCon <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", path))
  } else if (stringr::str_detect(path, ".sqlite")){
    snowCon <- DBI::dbConnect(RSQLite::SQLite(), path)
    print("Be careful! The database is now an SQLite database, and the tables and their columns may have changed.")
  } else {
    stop("This script is not designed to work with a database having that file extension. Currently supporting .mdb and .sqlite databases.")
  }
}