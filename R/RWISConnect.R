#' Connect to the RWIS database
#'
#' @param name Database name.
#' @param host Database host address.
#' @param port Connection port.
#' @param username Username. Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password.
#'
#' @return A connection to the database
#' @export
#'

RWISConnect <- function(name = "rwdm", host = "rwis.gov.yk.ca", port = "5432", username = "rwdmread", password = "rwdmread"){
  tryCatch({
    drv <- DBI::dbDriver("PostgreSQL")
    RWIS <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                           dbname = dsn_database,
                           host = dsn_hostname,
                           port = dsn_port,
                           user = dsn_uid,
                           password = dsn_pwd)
    return(RWIS)
  }, error=function(e) {
    print("Connection failed.")
  })

}

