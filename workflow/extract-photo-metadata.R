# Get metadata
library(dplyr)
library(exifr)
library(sf)
library(pbapply)
library(ggplot2)

dir.create("data/drone/L0/metadata", showWarnings = FALSE)
dir.create("data/drone/L0/metadata-lite", showWarnings = FALSE)

flight_days <- list.files("data/drone/L0/photos", full.names = TRUE)

pblapply(flight_days, FUN = function(x) {
  photo_list <- list.files(x, full.names = TRUE, recursive = TRUE)
  flight_day_basename <- basename(x)
  
  if(!file.exists(paste0("data/drone/L0/metadata/", flight_day_basename, "_metadata.csv"))) {
    metadata <- exifr::read_exif(path = photo_list)
    
    data.table::fwrite(x = metadata, file = paste0("data/drone/L0/metadata/", flight_day_basename, "_metadata.csv"))
  }
  
})

pblapply(flight_days, FUN = function(x) {
  flight_day_basename <- basename(x)
  
  metadata <- data.table::fread(paste0("data/drone/L0/metadata/", flight_day_basename, "_metadata.csv"))
  
  metadata_lite <- 
    metadata %>% 
    dplyr::select(SourceFile, FileName, FileSize, DateTimeOriginal, FlightXSpeed, FlightYSpeed, FlightZSpeed, FlightPitchDegree, FlightYawDegree, FlightRollDegree, GimbalPitchDegree, GimbalYawDegree, GimbalRollDegree, GPSLongitude, GPSLatitude, GPSAltitude, RelativeAltitude)
  
  data.table::fwrite(x = metadata_lite, file = paste0("data/drone/L0/metadata-lite/", flight_day_basename, "_metadata-lite.csv"))
  
})
