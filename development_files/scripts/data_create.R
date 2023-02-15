#Shortcut script for (re)creating sysdata.rda

prov_buff <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/basins/Clip_polygons", layer = "Provinces_buffered_300km")

data <- list(prov_buff = prov_buff)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
