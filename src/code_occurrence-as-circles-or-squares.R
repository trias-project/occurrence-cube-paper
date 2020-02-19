library(leaflet)
library(tidyverse)
library(sf)

lat_lon <- tibble(lat = 50.810596,	lon = 3.582600)

# lat-lon of a point in space
sf_lat_lon <-
  lat_lon %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(CRS("+init=epsg:3035"))

# make a circle around it
sf_lat_lon_circles <-
  sf_lat_lon %>%
  st_buffer(nQuadSegs = 50, dist = 300)

# get coordinates to make a square around point
sf_lat_lon_coords <-
  st_coordinates(sf_lat_lon) %>%
  as_vector()

# make square
sf_lat_lon_squares <-
  tibble(x = c(sf_lat_lon_coords[1] - 300,
               sf_lat_lon_coords[1] - 300,
               sf_lat_lon_coords[1] + 300,
               sf_lat_lon_coords[1] + 300,
               sf_lat_lon_coords[1] - 300),
         y = c(sf_lat_lon_coords[2] - 300,
               sf_lat_lon_coords[2] + 300,
               sf_lat_lon_coords[2] + 300,
               sf_lat_lon_coords[2] - 300,
               sf_lat_lon_coords[2] - 300)) %>%
  as.matrix()
sf_lat_lon_squares <- list(sf_lat_lon_squares)
sf_lat_lon_squares <-
  st_polygon(sf_lat_lon_squares, dim = 2) %>%
  st_sfc(crs = 3035)
# rotate function
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
# rotate a little to adjust appearance in leaflet due to distortion as
# converting to 4326
sf_lat_lon_squares <-
  (st_geometry(sf_lat_lon_squares)  - st_centroid(sf_lat_lon_squares)) *
  rot(pi/33)  + st_centroid(sf_lat_lon_squares)
sf_lat_lon_squares <- st_sfc(sf_lat_lon_squares, crs = 3035)
# Fig. 2 occurrence as square
sf_lat_lon_squares %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  setView(lng = lat_lon$lon, lat = lat_lon$lat, zoom = 15) %>%
  addTiles() %>%
  addPolygons(opacity = 1.0, fillOpacity = 0.5, color = "red")


# Fig. 1 occurrence as circle
sf_lat_lon_circles %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  setView(lng = lat_lon$lon, lat = lat_lon$lat, zoom = 15) %>%
  addTiles() %>%
  addPolygons(opacity = 1.0, fillOpacity = 0.5)

# Show how aggregated data can result in misleading low occupancy


