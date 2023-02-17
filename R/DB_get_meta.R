#' Get location metadata from the hydromet database
#'
#' Retrieves metadata for any valid location ID in the database. Returns the name of the location, the lat/long and (if possible) vertical datum information.
#'
#' @param path The path to the database, passed to hydroConnect. Default uses hydroConnect default path.
#' @param location A character vector of 1 or more location IDs, or default "all" to retrieval all locations.
#'
#' @return A list of two data.frames: one with the location code, name, latitude, and longitude, and another with vertical datum information.
#' @export
#'

DB_get_meta <- function(path = "default", location = "all") {

  DB <- hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(DB), add=TRUE)

  if (location == "all"){
    location <- DBI::dbGetQuery(DB, "SELECT location FROM locations")[,1]
  }

  meta <- data.frame()
  datum <- data.frame()
  for (i in location){
    latlong <- DBI::dbGetQuery(DB, paste0("SELECT latitude, longitude FROM locations WHERE location = '", i, "'"))
    datums <- DBI::dbGetQuery(DB, paste0("SELECT datum_id_from, datum_id_to, conversion_m, current FROM datum_conversions WHERE location = '", i, "'"))
    datum_codes <- unique(c(datums$datum_id_from, datums$datum_id_to))
    for (j in 1:nrow(datums)){
      datums$datum_id_from[j] <- DBI::dbGetQuery(DB, paste0("SELECT datum_name_en FROM datum_list WHERE datum_id = '", datums$datum_id_from[j], "'"))
      datums$datum_id_to[j] <- DBI::dbGetQuery(DB, paste0("SELECT datum_name_en FROM datum_list WHERE datum_id = '", datums$datum_id_to[j], "'"))
    }
    datums$location <- i
    datums <- datums[, c(5, 1,2,3,4)]
    names(datums) <- c("location", "origin datum", "end datum", "conversion (m)", "current")

    name <- DBI::dbGetQuery(DB, paste0("SELECT name FROM locations WHERE location = '", i, "'"))

    meta <- rbind(meta, data.frame("location_ID" = i,
                                   "name" = name[1,1],
                                   "latitude" = latlong[1,1],
                                   "longitude" = latlong[1,2]))
    datum <- rbind(datum, datums)
  }

  ls <- list("locations" = meta, "vertical_datums" = datums)

  return(ls)
}
