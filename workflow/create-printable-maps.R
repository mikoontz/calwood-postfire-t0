# Print a map of the orthomosaic

library(dplyr)
library(tmap)
library(terra)
library(grid)

ortho <- terra::rast(file.path("data", "drone", "L1", "ortho", "calwood-postfire-t0_ortho.tif"))
fire <- sf::st_read(file.path("data", "out", "calwood-fire-perimeter.gpkg"))

# Use terra::draw() to get the spatial extent of an interesting part of the landscape
# terra::plotRGB(ortho)
# terra::draw()
# SpatExtent : -105.380227754775, -105.378466858011, 40.1525670762914, 40.1534969166429 (xmin, xmax, ymin, ymax)

aoi <- terra::ext(c(-105.380227754775, -105.378466858011, 40.1525670762914, 40.1534969166429))
aoi_sf <- as.vector(aoi)[c(1,3,2,4)] %>% st_bbox() %>% st_as_sfc() %>% st_as_sf(crs = 4326)

ortho_crop <- terra::crop(x = ortho, y = aoi)

calwood_map <-
  tmap::tm_shape(ortho) +
  tm_rgb() +
  tm_shape(fire) +
  tm_borders(col = "red", lwd = 2) +
  tm_shape(aoi_sf) + 
  tm_borders(col = "blue", lwd = 2) +
  tm_scale_bar(width = 1/3) + 
  tm_compass(position = c("right", "top"), type = "rose") +
  tm_add_legend(type = "line", col = c("red", "blue"), lwd = 2, labels = c("Cal-Wood Fire boundary", "Zoomed view"))

calwood_map

tmap::tmap_save(tm = calwood_map, filename = file.path("figs", "calwood-ortho-map.pdf"), width = 10, height = 7.5)

# Use terra::draw() to get an even smaller interesting part of the landscape
# terra::plotRGB(ortho_crop)
# terra::draw()
# SpatExtent : -105.379801787341, -105.379642029262, 40.1529334844875, 40.1530437747485 (xmin, xmax, ymin, ymax)

aoi2 <- terra::ext(c(-105.379801787341, -105.379642029262, 40.1529334844875, 40.1530437747485))
aoi2_sf <- as.vector(aoi2)[c(1,3,2,4)] %>% st_bbox() %>% st_as_sfc() %>% st_as_sf(crs = 4326)

ortho_supercrop <- terra::crop(x = ortho_crop, y = aoi2)

calwood_map_zoom <-
  tmap::tm_shape(ortho_crop) +
  tm_rgb() +
  tm_shape(aoi2_sf) +
  tm_borders(col = "white", lwd = 2) + 
  tm_scale_bar(bg.color = "white") +
  tm_compass(position = c("right", "top"), type = "rose", bg.color = "white")

calwood_map_zoom

tmap::tmap_save(tm = calwood_map_zoom, filename = file.path("figs", "calwood-ortho-map_zoom.pdf"), width = 10, height = 7.5)

supercrop_inset <- 
  tm_shape(ortho_supercrop, unit = "m", ) +
  tm_rgb() +
  tm_scale_bar(bg.color = "white", position = c("center", "bottom"), width = 1/2) +
  tm_layout(frame = "white", frame.lwd = 10)

vp <- viewport(x = 0.15, y = 0.22, width = 0.3, height = 0.3)

tmap_save(calwood_map_zoom, filename = file.path("figs", "calwood-ortho-map_zoom-with-inset.pdf"),
          insets_tm = supercrop_inset, insets_vp = vp,
          width = 10, height = 7.5)


# Cropped dense point cloud
# 64 GB of RAM not enough! 
dpc <- lidR::readLAScatalog(folder = file.path("data", "out", "calwood-postfire-t0_dense-point-cloud.las"))
dpc_crop <- lidR::clip_roi(las = dpc, geometry = aoi_sf, progress = TRUE)
# lidR::writeLAS(las = dpc_crop, file = file.path("data", "drone", "L1", "dense-point-cloud", "calwood-postfire-t0_dense-point-cloud_small.las"))
