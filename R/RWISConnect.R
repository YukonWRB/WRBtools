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

