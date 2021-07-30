# Crop the dense point cloud, dsm, and orthomosaic from raw Metashape outputs then save them
# in appropriate L1 folders

# From Metashape, these three data products in their raw output form were saved as:
# data/out/calwood-postfire-t0_dense-point-cloud.las
# data/out/calwood-postfire-t0_dsm.tif
# data/out/calwood-postfire-t0_ortho.tif

# To do this, we need to define our area of interest
# The data products need to be cropped to remove some of the extraneous edge artifacts
# When we set up the flights, we buffered our flight path to 50m beyond the
# Cal-Wood Education Center property, anticipating that we could then use the
# data within exact boundary of the property (i.e., unbuffered) for analyses.

# We'll combine a few different data sources in order to do this well.
# 1) The actual locations of the photo points that were taken
# 2) The Cal-Wood Property boundary 

# For 1), we'll reference code written here: https://github.com/mikoontz/local-structure-wpb-severity/blob/master/workflow/04_convert-flight-path-to-site-boundary.R
# For 2), we'll work with the cleaned, buffered property boundary

# dependencies
library(dplyr)
library(sf)
library(terra)
library(lidR)
library(glue)
library(readr)
library(pbapply)

# Create needed L0 directories if necessary
dir.create(file.path("data", "drone", "L0", "mission-footprint"), showWarnings = FALSE)
dir.create(file.path("data", "drone", "L0", "mission-footprint", "photo-points"), showWarnings = FALSE)

# create needed L1 directories
dir.create(file.path("data", "drone", "L1", "dense-point-cloud"), showWarnings = FALSE)
dir.create(file.path("data", "drone", "L1", "dsm"), showWarnings = FALSE)
dir.create(file.path("data", "drone", "L1", "ortho"), showWarnings = FALSE)

# We'll want a local coordinate reference system for some later parts of this script,
# so we'll read in the original Cal-Wood property bounds
raw_calwood_property <- sf::st_read(dsn = file.path("data", "raw", "property", "property.shp"))
cw_crs <- sf::st_crs(x = raw_calwood_property)

# Download 30-m SRTM elevation data which is what Map Pilot uses to adjust the drone waypoints
# to account for the terrain

# Tile N40W106 covers the area of interest.

# One option to download is https://dwtkns.com/srtm30m/ and you'll need NASA Earthdata login credentials 

# Another option is to use the command line. To avoid putting your NASA Earthdata username/password in plaintext
# inside your script, instead set them to be environment variables using the Sys.setenv() function like the
# code snippet below. DON'T save your code with the "myuser" and "mypassword" filled in! Instead, run this line
# in the console (not in a script) so that it disappears and there's no record of it.

# Sys.setenv("EARTHDATA_USER" = "myuser",
#            "EARTHDATA_PASSWORD" = "mypassword")

tile_name <- "N40W106"

system2(command = 'wget', args= glue::glue('--user={Sys.getenv("EARTHDATA_USER")} --password={Sys.getenv("EARTHDATA_PASSWORD")} http://e4ftl01.cr.usgs.gov/MEASURES/SRTMGL1.003/2000.02.11/{tile_name}.SRTMGL1.hgt.zip -P data/raw'))

unzip(zipfile = file.path("data", "raw", paste0(tile_name, ".SRTMGL1.hgt.zip")), exdir = file.path("data/raw/"))
unlink(file.path("data", "raw", paste0(tile_name, ".SRTMGL1.hgt.zip")))

dem <- terra::rast(x = file.path("data", "raw", paste0(tile_name, ".hgt")))

# Flight dates to process (organized in different folders)
# character vector of all study sites that don't have the mission footprint figured out yet
dirs_to_process <- list.files(path = file.path("data", "drone", "L0", "photos"))

# Iterate through all the available dirs
# For loop is much more inuitive to use here (IMO)
all_photo_points <- 
  pblapply(X = seq_along(dirs_to_process), 
           FUN = function(i) {
             # get the character string representing the ith dir
             current_dir <- dirs_to_process[i]
             
             # read all the individual raw flight logs from the specified directory
             # turn the csv files into shape files by assigning the appropriate columns
             # to be the coordinates
             
             flight_logs_list <- 
               list.files(paste0("data/drone/L0/flight-logs/", current_dir), pattern = "[0-9].csv", full.names = TRUE) %>% 
               purrr::map(read_csv, col_types = cols()) %>% 
               purrr::map(.f = function(x) {
                 x %>% 
                   sf::st_as_sf(coords = c("Longitude", "Latitude")) %>%
                   sf::st_set_crs(4326)
               })
             
             # assume the first row of each log file is the location of the takeoff point
             # All altitude calculations are relative to this point, so getting the
             # elevation of this point from the DEM tells us the offset
             takeoff_points <-
               purrr::map(flight_logs_list, .f = function(x) {
                 x %>% 
                   dplyr::slice(1)
               })
             
             # filter the spatial flight log to just the rows where images incremented
             # and the drone is in a "flying" condition
             photo_points_list <-
               purrr::map(flight_logs_list, .f = function(x) {
                 x %>% 
                   dplyr::filter(isFlying == 1) %>% 
                   dplyr::filter(flyState == 14) %>% 
                   dplyr::filter(Images > lag(Images))
               })
             
             # Any bad flights that might have recorded a takeoff point, but some failure prevented the
             # aircraft from flying and taking photos?
             bad_flights <- sapply(photo_points_list, FUN = function(x) return(nrow(x) == 0))
             
             photo_points_list <- photo_points_list[!bad_flights]
             takeoff_points <- takeoff_points[!bad_flights]
             
             # Iterate over the list elements representing the photo points for each flight
             # and the take off point for each flight to calculate the elevation for each
             # photo point (on the ground) and the agl (above ground level) measure for
             # each photo point (by taking the ground elevation, the takeoff elevation, and
             # the relative altitude offset from the takeoff elevation into account)
             # Note: the agl should be fairly consistent across the whole mission.
             photo_points <- 
               purrr::map2(.x = photo_points_list, .y = takeoff_points, .f = function(x, y, ...) {
                 x %>% 
                   dplyr::mutate(photo_elev = terra::extract(dem, terra::vect(.), method = "bilinear")[, 2]) %>% 
                   dplyr::mutate(agl = `Altitude (m)` + terra::extract(dem, terra::vect(y))[, 2] - photo_elev) %>% 
                   dplyr::filter(agl > 105) %>% 
                   dplyr::mutate(Time = Time + as.POSIXct(current_dir, tz = "MST7MDT"))
               }) %>% 
               do.call(rbind, .)
             
             return(photo_points)
           })

# Combine all the photo points
all_photo_points <-
  all_photo_points %>% 
  bind_rows()

# Write photo points to disk
sf::st_write(obj = all_photo_points, dsn = file.path("data", "drone", "L0", "mission-footprint", "calwood-postfire-t0_photo-points.gpkg"), delete_dsn = TRUE)

# create a vector object representing the convex hull of the photopoints
# This will be a little bit too large, but will appropriately capture the west
# side of the flight area where the fire perimeter intersection is
# By combining the Cal-Wood Property boundary with this convex hull, we'll
# get the best of both worlds: the right boundary for the east side of
# the property from the property bounds itself, and the right boundary for
# the west side of the area flown which is bounded by the fire perimeter
# We subtract 50 meters to match the 50 meters we subtract
photo_bounds_ch <- 
  all_photo_points %>%
  sf::st_transform(cw_crs) %>% 
  st_union() %>% 
  st_convex_hull() %>% 
  sf::st_buffer(-50)

# For the Cal-Wood Property Boundary, note that the 50m buffered 
# version was first buffered, then cleaned up a bit to
# make it a more blocky shape instead of having a bunch of odd protrusions in the
# northwest. So we'll take that cleaned 50m buffered shape and subtract 50m to 
# get a clean version of the original property boundary

# Note after doing this: it turns out that the wonky parts of the property weren't near
# the fire, so we didn't fly those areas! So we could have just used the original
# property boundary. Oh well.

# read in the cleaned, buffered version, subtract away the buffer, and 
calwood_property <- 
  sf::st_read(dsn = file.path("data", "out", "calwood_property_boundary_50m_buffer_clean.kml")) %>% 
  sf::st_transform(crs = cw_crs) %>% 
  sf::st_buffer(dist = -50, joinStyle = "MITRE", mitreLimit = 3) %>% 
  sf::st_geometry()

# Intersection of the photo points convex hull geometry with the Cal-Wood property boundary
# produces the survey area that we want
fire_plus_cw <- 
  sf::st_intersection(x = photo_bounds_ch, y = calwood_property) %>% 
  sf::st_transform(4326)

# Write survey area to disk
sf::st_write(obj = fire_plus_cw, dsn = file.path("data", "drone", "L0", "mission-footprint", "calwood-postfire-t0_site-bounds.gpkg"), delete_dsn = TRUE)

# Original Metashape products to be cropped
dpc <- lidR::readLAScatalog(folder = file.path("data", "out", "calwood-postfire-t0_dense-point-cloud.las"))
dsm <- terra::rast(x = file.path("data", "out", "calwood-postfire-t0_dsm.tif"))
ortho <- terra::rast(x = file.path("data", "out", "calwood-postfire-t0_ortho.tif"))

# Cropped SRTM 30m data
dem_crop <- terra::crop(x = dem, y = terra::vect(fire_plus_cw))
dem_crop <- terra::mask(x = dem_crop, mask = terra::vect(fire_plus_cw))
terra::writeRaster(x = dem_crop, filename = file.path("data", "drone", "L0", "mission-footprint", "calwood-postfire-t0_srtm30m.tif"), overwrite = TRUE)

# Cropped Digital Surface Model (DSM)
dsm_crop <- terra::crop(x = dsm, y = terra::vect(fire_plus_cw))
dsm_crop <- terra::mask(x = dsm_crop, mask = terra::vect(fire_plus_cw))
terra::writeRaster(x = dsm_crop, filename = file.path("data", "drone", "L1", "dsm", "calwood-postfire-t0_dsm.tif"), overwrite = TRUE)

# Cropped orthomosaic
ortho_crop <- terra::crop(x = ortho, y = terra::vect(fire_plus_cw))
ortho_crop <- terra::mask(x = ortho_crop, mask = terra::vect(fire_plus_cw))
terra::writeRaster(x = ortho_crop, filename = file.path("data", "drone", "L1", "ortho", "calwood-postfire-t0_ortho.tif"), overwrite = TRUE)

# Cropped dense point cloud
# 64 GB of RAM not enough! 
# dpc_crop <- lidR::clip_roi(las = dpc, geometry = sf::st_as_sf(fire_plus_cw), progress = TRUE)
# lidR::writeLAS(las = dpc_crop, file = file.path("data", "drone", "L1", "dense-point-cloud", "calwood-postfire-t0_dense-point-cloud.las"))
