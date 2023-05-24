#Shortcut script for (re)creating sysdata.rda

prov_buff <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/basins/Clip_polygons", layer = "Provinces_buffered_300km")
level_returns_max <- read.csv("data-raw/level_returns_max.csv")

#NOTE: the name of *_returns tables be formulated so that the first part of the name matches the parameter as it's spelled in the local database, followed by _returns. If in doubt, check the parameter names using DB_browse_ts.

data <- list(prov_buff = prov_buff,
             level_returns_max = level_returns_max)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
