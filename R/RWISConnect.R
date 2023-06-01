#' Connect to the RWIS database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Establishes a connection to the RWIS (Road Weather Information System) database. Only works from within YG networks.
#'
#' @param name Database name.
#' @param host Database host address.
#' @param port Connection port.
#' @param username Username. Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password.
#' @importFrom RPostgreSQL PostgreSQL
#' @return A connection to the database
#'
#' @seealso [hydroConnect()] for establishing a connection to the WRB's hydrometric database.
#'
#' @export
#'

RWISConnect <- function(name = "rwdm", host = "rwis.gov.yk.ca", port = "5432", username = "rwdmread", password = "rwdmread"){

  #initial checks
  rlang::check_installed("RPostgreSQL", reason = "Package RPostgreSQL is required to use function RWISConnect") #This is here because RPostgreSQL is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"

  library(RPostgreSQL) #library calls should not usually be in a package... but this doesn't work without it!
  on.exit(detach("package:RPostgreSQL", unload = TRUE))

  tryCatch({
    RWIS <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                           dbname = name,
                           host = host,
                           port = port,
                           user = username,
                           password = password)
    return(RWIS)
  }, error=function(e) {
    print("Connection failed.")
  })

}

