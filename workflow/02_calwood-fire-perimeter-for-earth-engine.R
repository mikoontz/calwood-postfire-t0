calwood <- 
  sf::read_sf("data/out/calwood-fire-perimeter.gpkg") |>
  dplyr::mutate(Fire_ID = "calwood",
                Fire_Year = 2020,
                Start_Day = 152,
                End_Day = 258) |>
  dplyr::select(-Id) |>
  sf::st_transform(4326)

sf::st_write(obj = calwood, dsn = "data/out/2020_calwood-fire_colorado.shp")

mapview::mapview(calwood)
# Needed attributes; for Colorado, use Wyoming's image season (152 to 258)
# NAME           DESCRIPTION
# Fire_ID        unique identifier for each fire 
# Fire_Year      year of fire
# Start_Day      start day of fire season in julian days, e.g. June 15 = 166
# End_Day        end day of fire season in julian days, e.g. June 15 = 166

r <- terra::rast("data/out/calwood_CBI_bc.tif")
terra::plot(r)
terra::plot(terra::vect(calwood), add = TRUE)

cw_prop <- sf::read_sf("data/out/calwood_fire_perim_calwood_property_intersect.kml")
plot(cw_prop$geometry)

r_crop <- terra::crop(x = r, y = cw_prop)
terra::plot(r_crop, col = viridis::inferno(100))
terra::writeRaster(x = r_crop, filename = "data/out/calwood-fire-severity-on-calwood-property.tif")

ortho <- terra::rast("/Volumes/dev/sUAS/sUAS_data/calwood-postfire-t0/data/drone/L1/ortho/calwood-postfire-t0_ortho.tif")
terra::plotRGB(ortho)
