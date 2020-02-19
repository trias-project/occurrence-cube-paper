library(sf)
library(sp)
library(tidyverse)
library(leaflet)

n_pts <- 10
lat_lon <-
  tibble(
    lat = rep(50.810596, n_pts),
    lon = rep(3.583700, n_pts),
    other = rep(NA, n_pts)
)

# lat-lon of a point in space
sf_lat_lon <-
  lat_lon %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(CRS("+init=epsg:3035"))

# get needed cellcodes
x_y <- st_coordinates(sf_lat_lon)[1,]
# cell the centroid belongs to
centroid_cell <- paste0("1km",
                        "E",
                        floor(x_y[1]/1000),
                        "N",
                        floor(x_y[2]/1000))
x_y_cells_surroundings <- crossing(
  x = seq(floor(x_y[1]/1000) - 2, floor(x_y[1]/1000) + 2),
  y = seq(floor(x_y[2]/1000) - 2, floor(x_y[2]/1000) + 2))

x_y_cells_surroundings <-
  paste0(
    "1km",
    "E",
    x_y_cells_surroundings$x,
    "N",
    x_y_cells_surroundings$y
  )

#read EEA grid of Belgium
be_grid <- st_read("../pipeline/data/external/utm1_bel")
# select cells in the surroundings of circle
cells_surroundings <-
  be_grid %>%
  filter(CELLCODE %in% x_y_cells_surroundings)

basic_grid <-
  cells_surroundings %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  setView(lng = lat_lon$lon[1], lat = lat_lon$lat[1], zoom = 13) %>%
  addTiles() %>%
  addPolygons(opacity = 1.0, fillOpacity = 0.2, weight = 0.5)

small_radius <- 250
large_radius <- 1000
extralarge_radius <- 2000

# Small circle contained in one cell with centroid
map_small_circle <-
  basic_grid %>%
  addCircles(lng = lat_lon$lon[1], lat = lat_lon$lat[1], weight = 1,
             radius = small_radius, color = "red")
map_small_circle

# Large circle extending on multiple cells. Cell with centroid has the highest
# portion of area
map_large_circle <-
  basic_grid %>%
  addCircles(lng = lat_lon$lon[1], lat = lat_lon$lat[1], weight = 1,
             radius = large_radius, color = "red")
map_large_circle

# Large circle extending on multiple cells. More than one cell have the highest
# portion of area
map_extralarge_circle <-
  basic_grid %>%
  addCircles(lng = lat_lon$lon[1], lat = lat_lon$lat[1], weight = 1,
             radius = extralarge_radius, color = "red")
map_extralarge_circle


# function to pick 10 random points in a circle
#' @param sf_df A sf_object with one point geometry (center)
#' @param radius numeric. Radius of the circle
#' @param n_pts Number of random points to get from circle
random_pt_in_circle <- function(sf_df, radius, n_pts = 10) {
  random_pts <- as(sf_df, "Spatial")
  random_pts@data <-
  random_pts@data %>%
  mutate(random_angle = runif(n_pts, 0, 2*pi))

random_pts@data <-
  random_pts@data %>%
  mutate(random_r = sqrt(runif(
    n_pts, 0, 1)) * radius)

random_pts@data <-
  random_pts@data %>%
  mutate(x = random_pts@coords[, "coords.x1"],
         y = random_pts@coords[, "coords.x2"])
random_pts@data <-
  random_pts@data %>%
  mutate(x = x + random_r * cos(random_angle),
         y = y + random_r * sin(random_angle)) %>%
  select(-c(random_angle, random_r))

random_pts <-
  random_pts@data %>%
  st_as_sf(coords = c("x", "y"), crs = 3035)

random_pts <-
  random_pts %>%
  st_transform(4326) %>%
  st_coordinates
}

random_pts_small <- random_pt_in_circle(sf_lat_lon,
                                        radius = small_radius)

random_pts_large <- random_pt_in_circle(sf_lat_lon,
                                        radius = large_radius)

random_pts_extralarge <- random_pt_in_circle(sf_lat_lon,
                                        radius = extralarge_radius)

map_random_pt_small <-
  map_small_circle %>%
  addCircleMarkers(lng = random_pts_small[, "X"],
                   lat = random_pts_small[, "Y"],
                   radius = 1,
                   fillOpacity = 1)
map_random_pt_small

map_random_pt_large <-
  map_large_circle %>%
  addCircleMarkers(lng = random_pts_large[, "X"],
                   lat = random_pts_large[, "Y"],
                   radius = 1,
                   fillOpacity = 1)
map_random_pt_large

map_random_pt_extralarge <-
  map_extralarge_circle %>%
  addCircleMarkers(lng = random_pts_extralarge[, "X"],
                   lat = random_pts_extralarge[, "Y"],
                   radius = 1,
                   fillOpacity = 1)
map_random_pt_extralarge
