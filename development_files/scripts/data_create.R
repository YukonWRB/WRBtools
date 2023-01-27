#Shortcut script for (re)creating sysdata.rda

prov_buff <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/basins/Clip_polygons", layer = "Provinces_buffered_300km")
#WSC_polygons <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/WSC_basins", layer = "WSC_watersheds_polygons")
#WSC_points <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/WSC_basins", layer = "WSC_watersheds_points")
ECCC_stations <- utils::read.csv("inst/extdata/ECCCstations.csv", skip=2)

data <- list(prov_buff = prov_buff,
             #,
             #WSC_polygons = WSC_polygons,
             #WSC_points = WSC_points,
             ECCC_stations = ECCC_stations
             )

usethis::use_data(data, internal=TRUE, overwrite=TRUE)

#polygons and points are in WRBfloods for now.
