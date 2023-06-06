# Function to simulate points
sim_points <- function(n, xlim = c(62000, 63000), ylim = c(182000, 183000),
                       add_points = NULL, seed = 123) {
  set.seed(seed)
  if (!is.null(add_points)) {
    out <- tibble(
      lat = runif(n, ylim[1], ylim[2]),
      long = runif(n, xlim[1], xlim[2])) %>%
      bind_rows(add_points[,1:2]) %>%
      st_as_sf(coords = c("long", "lat"), crs = st_crs(31370)) %>%
      mutate(coordinateUncertaintyInMeters =
               c(abs(rnorm(10, 100, 500)),
                 add_points$coordinateUncertaintyInMeters))
  } else {
    out <- tibble(
      lat = runif(n, ylim[1], ylim[2]),
      long = runif(n, xlim[1], xlim[2])) %>%
      st_as_sf(coords = c("long", "lat"), crs = st_crs(31370)) %>%
      mutate(coordinateUncertaintyInMeters = abs(rnorm(10, 100, 500)))
  }

  out %>%
    cbind(st_coordinates(out)) %>%
    rename(x = X, y = Y)
}

# Function to create grid around points with uncertainties
create_grid <- function(spacing, circles) {
  grid_spacing <- spacing  # size of squares, in units of the CRS
  grid <- st_make_grid(circles, square = T,
                       cellsize = c(grid_spacing, grid_spacing)) %>%
    st_as_sf() %>%
    mutate(id = row_number())

  return(grid)
}
