# Visualize Calwood
library(dplyr)
library(sf)

dir.create("data/out", showWarnings = FALSE)

cw_property <- 
  sf::st_read("data/raw/property/property.shp") %>% 
  sf::st_transform(4326) %>% 
  dplyr::mutate(Name = "Cal-Wood", 
                Description = "Cal-Wood property boundary") %>% 
  dplyr::select(-Id)

plot(cw_property)

sf::st_write(cw_property, dsn = "data/out/calwood_property_boundary.kml", delete_dsn = TRUE)

sf::st_read("data/raw/property/property.shp") %>% 
  dplyr::mutate(Name = "Cal-Wood", 
                Description = "Cal-Wood property boundary") %>% 
  dplyr::select(-Id) %>% 
  sf::st_write(dsn = "data/out/calwood-property-boundary.gpkg", delete_dsn = TRUE)

cw_buff_50 <- 
  sf::st_read("data/raw/property/property.shp") %>% 
  sf::st_buffer(dist = 50, 
                endCapStyle = "SQUARE", 
                joinStyle = "MITRE", mitreLimit = 3) %>% 
  sf::st_transform(4326) %>% 
  dplyr::mutate(Name = "Cal-Wood_50m-buffer", 
                Description = "Cal-Wood property boundary buffered by 50 m") %>% 
  dplyr::select(-Id)

plot(cw_buff_50)

sf::st_write(cw_buff_50, dsn = "data/out/calwood_property_boundary_50m_buffer.kml", delete_dsn = TRUE)

cw_fire <- 
  sf::st_read(dsn = "data/raw/Calwood_Lefthand/Calwood_Lefthand.shp") %>% 
  dplyr::filter(st_area(.) == max(st_area(.)))

sf::st_write(obj = cw_fire, dsn = "data/out/calwood-fire-perimeter.gpkg", delete_dsn = TRUE)

cw_fire_buff_50 <- 
  cw_fire %>% 
  dplyr::filter(st_area(.) == max(st_area(.))) %>% 
  sf::st_buffer(dist = 50, 
                endCapStyle = "SQUARE", 
                joinStyle = "MITRE", mitreLimit = 3) %>% 
  sf::st_transform(4326) %>% 
  dplyr::mutate(Name = "Calwood-Fire_50m-buffer", 
                Description = "Calwood fire perimeter buffered by 50 m") %>% 
  dplyr::select(-Id)
  
sf::st_write(obj = cw_fire_buff_50, dsn = "data/out/calwood_fire_perimeter_50m_buffer.kml", delete_dsn = TRUE)

cw_buff_50_clean <- sf::st_read("data/out/calwood_property_boundary_50m_buffer_clean.kml")

cw_intersect <- sf::st_intersection(x = cw_fire_buff_50, 
                                    y = cw_buff_50_clean)

plot(cw_intersect %>% st_geometry())

sf::st_write(obj = cw_intersect, dsn = "data/out/calwood_fire_perim_calwood_property_intersect.kml")
