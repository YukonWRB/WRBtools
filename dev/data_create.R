#Shortcut script for (re)creating sysdata.rda

prov_buff <- sf::read_sf(dsn = "dev/prov_buffers", layer = "Provinces_buffered_300km")

data <- list(prov_buff = prov_buff)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
