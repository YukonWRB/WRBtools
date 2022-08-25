prov_buff <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/Clip_polygons", layer = "Provinces_buffered_300km")
WSC_polygons <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/WSC_basins", layer = "WSC_watersheds_polygons")
WSC_points <- sf::read_sf(dsn = "G:/water/Common_GW_SW/Data/WSC_basins", layer = "WSC_watersheds_points")

data <- list(prov_buff = prov_buff, WSC_polygons = WSC_polygons, WSC_points = WSC_points)
