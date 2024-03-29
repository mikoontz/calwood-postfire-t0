# mapping progress

library(dplyr)
library(sf)
library(pbapply)
library(data.table)
library(ggplot2)
library(lubridate)

dir.create("figs", showWarnings = FALSE)

cw <- sf::st_read("data/out/calwood-property-boundary.gpkg")
cw_fire <- sf::st_read("data/out/calwood-fire-perimeter.gpkg")

metadata <- 
  list.files("data/drone/L0/metadata-lite/", full.names = TRUE) %>% 
  pblapply(FUN = fread) %>% 
  data.table::rbindlist()

photo_points <- 
  metadata %>% 
  st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = 4326) %>% 
  sf::st_transform(sf::st_crs(cw)) %>% 
  dplyr::mutate(DateTimeOriginal = lubridate::ymd_hms(DateTimeOriginal),
                sampling_day = as.factor(1 + as.numeric(difftime(trunc(DateTimeOriginal, units = "days"), min(trunc(DateTimeOriginal, units = "days")), units = "days"))))

photo_points

sampling_progress_gg <-
  ggplot() +
  geom_sf(data = cw, alpha = 0.5, fill = "blue") +
  geom_sf(data = cw_fire, color = "red", lwd = 2, fill = NA) +
  geom_sf(data = photo_points, aes(color = sampling_day)) +
  theme_bw() +
  coord_sf(xlim = c(st_bbox(cw)[c(1, 3)]), ylim = c(st_bbox(cw)[c(2, 4)]))

sampling_progress_gg

ggsave(filename = "figs/sampling-progress_2021-06-07.png", plot = sampling_progress_gg)

