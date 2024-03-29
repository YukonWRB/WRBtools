#' Polygons of provinces with 300km buffers
#'
#' Storing and loading package spatial data is tricky! For shapefiles, the package sf is required but loading internal data can't be done with a specific package (loading a .rda file doesn't allow that). Requiring  this package to be loaded just to load internal data doesn't make much sense. Instead, delayedAssign below waits until "prov_buff" is called and then uses the code, including an sf call, to load the shapefile.
#'
#' @format
#' An ESRI shapefile.
#' @noRd
#' @keywords internal

delayedAssign("prov_buff", local({
  try(
    sf::read_sf(
      system.file("extdata/prov_buffers/Provinces_buffered_300km.shp", package = "WRBtools")  #This lives in the /inst folder, but because of how /inst works the path does not include /inst. Read up on this behavior in the R packages e-book.
    ),
    silent = TRUE
  )
}))
