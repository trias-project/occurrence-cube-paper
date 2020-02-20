library(sf)
library(sp)
library(tidyverse)
library(leaflet)
library(here)
library(mapview)
# to combine pngs together
library(grid)
library(png)

#read EEA grid of Belgium
be_grid <- st_read("../pipeline/data/external/utm1_bel")

key <- "0011257-200127171203522"
zip_filename <- paste0(key, ".zip")
if (!file.exists(here::here("data", "raw", zip_filename))) {
  occ <- occ_download_get(
    key = key,
    path = here::here("data", "raw")
  )
}

occ_file <- paste(key, "occurrence.txt", sep = "_")
occ_path <- here::here("data", "raw", occ_file)
if (!file.exists(here::here("data", "raw", occ_file))) {
  unzip(zipfile = occ,
        files = "occurrence.txt",
        exdir = here::here("data", "raw"))
  file.rename(from = here::here("data", "raw", "occurrence.txt"),
              to = occ_path
  )
}
cols_occ_file <- read_delim(
  occ_path, "\t", n_max = 1,
  quote = ""
)
cols_occ_file <- names(cols_occ_file)

length(cols_occ_file)


occ_reynoutria <- read_tsv(
  here::here("data", "raw", occ_file),
  na = "",
  quote = "",
  guess_max = 50000)
nrow(occ_reynoutria)

# lat-lon of a point in space with small uncertainty (all within a cell)
occ_id_small <- 2235280677
small_radius_occ_reynoutria <-
  occ_reynoutria %>%
  # use this filter for getting one occ point randomly
  # filter(coordinateUncertaintyInMeters < 150 &
  #          coordinateUncertaintyInMeters > 60 &
  #          coordinateUncertaintyInMeters != 100) %>%
  # sample_n(size = 1) %>%
  filter(gbifID == occ_id_small) %>%
  select(gbifID,
         decimalLongitude,
         decimalLatitude,
         coordinateUncertaintyInMeters)

small_radius <- small_radius_occ_reynoutria$coordinateUncertaintyInMeters
small_radius

occ_id_large <- 1569856810
large_radius_occ_reynoutria <-
  occ_reynoutria %>%
  # use this filter for getting one occ point randomly
  # filter(coordinateUncertaintyInMeters < 2500 &
  #          coordinateUncertaintyInMeters > 850) %>%
  # sample_n(size = 1) %>%
  filter(gbifID == occ_id_large) %>%
  select(gbifID,
         decimalLongitude,
         decimalLatitude,
         coordinateUncertaintyInMeters)

large_radius <- large_radius_occ_reynoutria$coordinateUncertaintyInMeters
large_radius

# lat-lon of a point in space with small uncertainty (spread over two cells)
occ_id_small_over_two_cells <- 2235279067
small_radius_occ_reynoutria_spread <-
  occ_reynoutria %>%
  filter(gbifID == occ_id_small_over_two_cells) %>%
  select(gbifID,
         decimalLongitude,
         decimalLatitude,
         coordinateUncertaintyInMeters)

small_radius_spread <- small_radius_occ_reynoutria_spread$coordinateUncertaintyInMeters
small_radius_spread

# make sf objects
sf_lat_lon_small <-
  small_radius_occ_reynoutria %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_transform(crs = 3035)

sf_lat_lon_large <-
  large_radius_occ_reynoutria %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_transform(crs = 3035)

sf_lat_lon_small_spread <-
  small_radius_occ_reynoutria_spread %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_transform(crs = 3035)

# get needed cellcodes
x_y_small <- st_coordinates(sf_lat_lon_small)
x_y_large <- st_coordinates(sf_lat_lon_large)
x_y_small_spread <- st_coordinates(sf_lat_lon_small_spread)

# cell the centroid belongs to
centroid_cell_small <- paste0("1km",
                        "E",
                        floor(x_y_small[1]/1000),
                        "N",
                        floor(x_y_small[2]/1000))
centroid_cell_large <- paste0("1km",
                              "E",
                              floor(x_y_large[1]/1000),
                              "N",
                              floor(x_y_large[2]/1000))
centroid_cell_small_spread <- paste0("1km",
                              "E",
                              floor(x_y_small_spread[1]/1000),
                              "N",
                              floor(x_y_small_spread[2]/1000))

x_y_cells_surroundings_small <- crossing(
  x = seq(floor(x_y_small[1]/1000) - 3, floor(x_y_small[1]/1000) + 3),
  y = seq(floor(x_y_small[2]/1000) - 3, floor(x_y_small[2]/1000) + 3))
x_y_cells_surroundings_small <-
  paste0(
    "1km",
    "E",
    x_y_cells_surroundings_small$x,
    "N",
    x_y_cells_surroundings_small$y
  )

x_y_cells_surroundings_large <- crossing(
  x = seq(floor(x_y_large[1]/1000) - 4, floor(x_y_large[1]/1000) + 4),
  y = seq(floor(x_y_large[2]/1000) - 4, floor(x_y_large[2]/1000) + 4))
x_y_cells_surroundings_large <-
  paste0(
    "1km",
    "E",
    x_y_cells_surroundings_large$x,
    "N",
    x_y_cells_surroundings_large$y
  )

x_y_cells_surroundings_small_spread <- crossing(
  x = seq(floor(x_y_small_spread[1]/1000) - 3, floor(x_y_small_spread[1]/1000) + 3),
  y = seq(floor(x_y_small_spread[2]/1000) - 3, floor(x_y_small_spread[2]/1000) + 3))
x_y_cells_surroundings_small_spread <-
  paste0(
    "1km",
    "E",
    x_y_cells_surroundings_small_spread$x,
    "N",
    x_y_cells_surroundings_small_spread$y
  )

# select cells in the surroundings of circle
cells_surroundings_small <-
  be_grid %>%
  filter(CELLCODE %in% x_y_cells_surroundings_small)

cells_surroundings_large <-
  be_grid %>%
  filter(CELLCODE %in% x_y_cells_surroundings_large)

cells_surroundings_small_spread <-
  be_grid %>%
  filter(CELLCODE %in% x_y_cells_surroundings_small_spread)

# create basic map with grid
basic_grid_small <-
  cells_surroundings_small %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  setView(lng = small_radius_occ_reynoutria$decimalLongitude,
          lat = small_radius_occ_reynoutria$decimalLatitude,
          zoom = 14) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addPolygons(opacity = 0.2,
              fillOpacity = 0.0,
              weight = 1.0,
              color = "black")

basic_grid_large <-
  cells_surroundings_large %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = large_radius_occ_reynoutria$decimalLongitude,
          lat = large_radius_occ_reynoutria$decimalLatitude,
          zoom = 14) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addPolygons(opacity = 0.2,
              fillOpacity = 0.0,
              weight = 1.0,
              color = "black")

basic_grid_small_spread <-
  cells_surroundings_small_spread %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  setView(lng = small_radius_occ_reynoutria_spread$decimalLongitude,
          lat = small_radius_occ_reynoutria_spread$decimalLatitude,
          zoom = 14) %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addPolygons(opacity = 0.2,
              fillOpacity = 0.0,
              weight = 1.0,
              color = "black")

# add circles to grid map
map_circle_in_cell_small <-
  basic_grid_small %>%
  addCircles(lng = small_radius_occ_reynoutria$decimalLongitude,
             lat = small_radius_occ_reynoutria$decimalLatitude,
             weight = 3,
             radius = small_radius, color = "black")
map_circle_in_cell_small

map_circle_in_cell_large <-
  basic_grid_large %>%
  addCircles(lng = large_radius_occ_reynoutria$decimalLongitude,
             lat = large_radius_occ_reynoutria$decimalLatitude,
             weight = 3,
             radius = large_radius, color = "black")
map_circle_in_cell_large

map_circle_in_cell_small_spread <-
  basic_grid_small_spread %>%
  addCircles(lng = small_radius_occ_reynoutria_spread$decimalLongitude,
             lat = small_radius_occ_reynoutria_spread$decimalLatitude,
             weight = 3,
             radius = small_radius_spread, color = "black")
map_circle_in_cell_small_spread

# pick random point in small circle
# Run code below 251 - 310 to produce a random assignment
random_pt_small_circle <- as(sf_lat_lon_small, "Spatial")
nrow_df <- nrow(random_pt_small_circle)
random_pt_small_circle@data <-
  random_pt_small_circle@data %>%
  mutate(random_angle = runif(nrow_df, 0, 2*pi))

random_pt_small_circle@data <-
  random_pt_small_circle@data %>%
  mutate(random_r = sqrt(runif(
    nrow_df, 0, 1)) * small_radius)

random_pt_small_circle@data <-
  random_pt_small_circle@data %>%
  mutate(x = random_pt_small_circle@coords[, "coords.x1"],
         y = random_pt_small_circle@coords[, "coords.x2"])
random_pt_small_circle@data <-
  random_pt_small_circle@data %>%
  mutate(x = x + random_r * cos(random_angle),
         y = y + random_r * sin(random_angle)) %>%
  select(-c(random_angle, random_r))

random_pt_small_circle <-
  random_pt_small_circle@data %>%
  st_as_sf(coords = c("x", "y"), crs = 3035)

x_y_random_pt_small_circle <- st_coordinates(random_pt_small_circle)
# cell the random point belongs to
cellcode_random_pt_small_circle <- paste0(
  "1km",
  "E",
  floor(x_y_random_pt_small_circle[1]/1000),
  "N",
  floor(x_y_random_pt_small_circle[2]/1000)
)

cell_random_pt_small_circle_wgs84 <-
  be_grid %>%
  filter(CELLCODE == cellcode_random_pt_small_circle) %>%
  st_transform(4326)

random_pt_small_circle <-
  random_pt_small_circle %>%
  st_transform(4326) %>%
  st_coordinates

map_random_pt_in_cell_small <-
  map_circle_in_cell_small %>%
  addCircleMarkers(lng = random_pt_small_circle[1, "X"],
                   lat = random_pt_small_circle[1, "Y"],
                   radius = 2,
                   fillOpacity = 1.0,
                   opacity = 1.0,
                   color = "red") %>%
  # stress the cell the random point falls in
  addPolygons(data = cell_random_pt_small_circle_wgs84,
              opacity = 1.0,
              fillOpacity = 0.0,
              weight = 3,
              color = "red")
map_random_pt_in_cell_small

# pick random point in large circle
# Run code below 314 - 373 to produce a random assignment
random_pt_large_circle <- as(sf_lat_lon_large, "Spatial")
random_pt_large_circle@data <-
  random_pt_large_circle@data %>%
  mutate(random_angle = runif(nrow_df, 0, 2*pi))

random_pt_large_circle@data <-
  random_pt_large_circle@data %>%
  mutate(random_r = sqrt(runif(
    nrow_df, 0, 1)) * large_radius)

random_pt_large_circle@data <-
  random_pt_large_circle@data %>%
  mutate(x = random_pt_large_circle@coords[, "coords.x1"],
         y = random_pt_large_circle@coords[, "coords.x2"])
random_pt_large_circle@data <-
  random_pt_large_circle@data %>%
  mutate(x = x + random_r * cos(random_angle),
         y = y + random_r * sin(random_angle)) %>%
  select(-c(random_angle, random_r))

random_pt_large_circle <-
  random_pt_large_circle@data %>%
  st_as_sf(coords = c("x", "y"), crs = 3035)

x_y_random_pt_large_circle <- st_coordinates(random_pt_large_circle)

# cell the random point belongs to
cellcode_random_pt_large_circle <- paste0(
  "1km",
  "E",
  floor(x_y_random_pt_large_circle[1]/1000),
  "N",
  floor(x_y_random_pt_large_circle[2]/1000)
)

cell_random_pt_large_circle_wgs84 <-
  be_grid %>%
  filter(CELLCODE == cellcode_random_pt_large_circle) %>%
  st_transform(4326)

random_pt_large_circle <-
  random_pt_large_circle %>%
  st_transform(4326) %>%
  st_coordinates

map_random_pt_over_cells <-
  map_circle_in_cell_large %>%
  addCircleMarkers(lng = random_pt_large_circle[1, "X"],
                   lat = random_pt_large_circle[1, "Y"],
                   radius = 2,
                   fillOpacity = 1.0,
                   opacity = 1.0,
                   color = "red") %>%
  # stress the cell the random point falls in
  addPolygons(data = cell_random_pt_large_circle_wgs84,
              opacity = 1.0,
              fillOpacity = 0.0,
              weight = 3,
              color = "red")
map_random_pt_over_cells

# pick random point in small circle overlapping two cells
# Run code below 377 - 460 to produce a random assignment
random_pt_small_circle_spread <- as(sf_lat_lon_small_spread, "Spatial")
nrow_df <- nrow(random_pt_small_circle_spread)
random_pt_small_circle_spread@data <-
  random_pt_small_circle_spread@data %>%
  mutate(random_angle = runif(nrow_df, 0, 2*pi))

random_pt_small_circle_spread@data <-
  random_pt_small_circle_spread@data %>%
  mutate(random_r = sqrt(runif(
    nrow_df, 0, 1)) * small_radius_spread)

random_pt_small_circle_spread@data <-
  random_pt_small_circle_spread@data %>%
  mutate(x = random_pt_small_circle_spread@coords[, "coords.x1"],
         y = random_pt_small_circle_spread@coords[, "coords.x2"])
random_pt_small_circle_spread@data <-
  random_pt_small_circle_spread@data %>%
  mutate(x = x + random_r * cos(random_angle),
         y = y + random_r * sin(random_angle)) %>%
  select(-c(random_angle, random_r))

random_pt_small_circle_spread <-
  random_pt_small_circle_spread@data %>%
  st_as_sf(coords = c("x", "y"), crs = 3035)

x_y_random_pt_small_circle_spread <-
  st_coordinates(random_pt_small_circle_spread)
# cell the random point belongs to
cellcode_random_pt_small_circle_spread <- paste0(
  "1km",
  "E",
  floor(x_y_random_pt_small_circle_spread[1]/1000),
  "N",
  floor(x_y_random_pt_small_circle_spread[2]/1000)
)

cell_random_pt_small_circle_wgs84_spread <-
  be_grid %>%
  filter(CELLCODE == cellcode_random_pt_small_circle_spread) %>%
  st_transform(4326)

random_pt_small_circle_spread <-
  random_pt_small_circle_spread %>%
  st_transform(4326) %>%
  st_coordinates

map_random_pt_in_cell_small_spread <-
  map_circle_in_cell_small_spread %>%
  addCircleMarkers(lng = random_pt_small_circle_spread[1, "X"],
                   lat = random_pt_small_circle_spread[1, "Y"],
                   radius = 2,
                   fillOpacity = 1.0,
                   opacity = 1.0,
                   color = "red") %>%
  # stress the cell the random point falls in
  addPolygons(data = cell_random_pt_small_circle_wgs84_spread,
              opacity = 1.0,
              fillOpacity = 0.0,
              weight = 3,
              color = "red")
map_random_pt_in_cell_small_spread

# save maps as png
filename_small <- here::here(
  "figures",
  paste0("random_pt_in_small_circle_and_grids_",
         occ_id_small,
         ".png"))
mapshot(map_random_pt_in_cell_small, file = filename_small)

filename_large <- here::here(
  "figures",
  paste0("random_pt_in_big_circle_and_grids_",
         occ_id_large,
         ".png"))
mapshot(map_random_pt_over_cells, file = filename_large)

filename_small_spread <- here::here(
  "figures",
  paste0("random_pt_in_small_circle_and_grids_overlap_",
         occ_id_small,
         ".png"))
mapshot(map_random_pt_in_cell_small_spread,
        file = filename_small_spread)
