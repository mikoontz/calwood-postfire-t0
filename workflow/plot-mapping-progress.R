# mapping progress

library(dplyr)
library(sf)
library(pbapply)
library(data.table)

dir.create("figs", showWarnings = FALSE)

cw <- sf::st_read("data/out/calwood_property_boundary.gpkg")

metadata <- 
  list.files("data/drone/L0/metadata", full.names = TRUE) %>% 
  pblapply(FUN = fread) %>% 
  data.table::rbindlist()

photo_points <- 
  metadata %>% 
  dplyr::select(FileName, DateTimeOriginal, GPSLongitude, GPSLatitude) %>% 
  st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = 4326) %>% 
  sf::st_transform(sf::st_crs(cw))

sampling_progress_gg <-
  ggplot() +
  geom_sf(data = cw, alpha = 0.5, fill = "blue") +
  geom_sf(data = photo_points) +
  theme_bw()

ggsave(filename = "figs/sampling-progress_2021-05-24.png", plot = sampling_progress_gg)

