# Get metadata
library(dplyr)
library(exifr)
library(sf)
library(pbapply)
library(ggplot2)

dir.create("data/drone/L0/metadata", showWarnings = FALSE)

flight_days <- list.files("data/drone/L0/photos", full.names = TRUE)

pblapply(flight_days, FUN = function(x) {
  photo_list <- list.files(x, full.names = TRUE, recursive = TRUE)
  flight_day_basename <- basename(x)
  
  metadata <- exifr::read_exif(path = photo_list)

  write.csv(x = metadata, file = paste0("data/drone/L0/metadata/", flight_day_basename, "_metadata.csv"), row.names = FALSE)
  })
