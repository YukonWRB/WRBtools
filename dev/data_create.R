#Shortcut script for (re)creating sysdata.rda

prov_buff <- sf::read_sf(dsn = "dev/prov_buffers", layer = "Provinces_buffered_300km")   #Important!!! The sf package is being used here to create an sf object which is later saved as internal data. You might have noticed that the rest of the package does NOT use the sf package but instead uses terra... terra is faster and easier to work with, but terra objects are "pointers"; the files themselves are not loaded to the R environment but are instead worked on in-place. This is *very* handy for working with large spatial files, but saving a pointer object as package data does not work. sf package creates an actual object in the R environment and can be loaded as a terra object later on.

data <- list(prov_buff = prov_buff)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
