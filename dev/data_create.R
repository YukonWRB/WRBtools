#Shortcut script for (re)creating sysdata.rda
#

#IMPORTANT NOTE: spatial data doesn't behave well as internal package data. See the file data_load in the /R folder for a better way to do this. Non-spatial data can almost all be incorporated using  internal data though.

# prov_buff <- sf::read_sf(dsn = "dev/prov_buffers", layer = "Provinces_buffered_300km")
#
#
# data <- list() #data must be a single object
#
# usethis::use_data(data, internal=TRUE, overwrite=TRUE)
