# Visualize Calwood

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
