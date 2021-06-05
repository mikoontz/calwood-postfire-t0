# make photo points

library(dplyr)
library(sf)
library(pbapply)

cw <- sf::st_read("data/out/calwood-property-boundary.gpkg")

metadata_files <- list.files(path = "data/drone/L0/metadata-lite/", full.names = TRUE)

metadata <- 
  metadata_files %>% 
  pblapply(read.csv) %>% 
  dplyr::bind_rows() 

photo_points <-
  metadata %>% 
  sf::st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), remove = FALSE, crs = 4326) %>% 
  sf::st_transform(sf::st_crs(cw))

sf::st_write(obj = photo_points, dsn = "data/drone/L0/photo-points.gpkg")
