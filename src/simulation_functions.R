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
               c(abs(rnorm(n, 100, 500)),
                 add_points$coordinateUncertaintyInMeters))
  } else {
    out <- tibble(
      lat = runif(n, ylim[1], ylim[2]),
      long = runif(n, xlim[1], xlim[2])) %>%
      st_as_sf(coords = c("long", "lat"), crs = st_crs(31370)) %>%
      mutate(coordinateUncertaintyInMeters = abs(rnorm(n, 100, 500)))
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

# Function to simulate occurrence cubes
sim_cubes <- function(n_sim, points, grid, seed = 123, print = 100) {
  out <- vector(mode = "list", length = n_sim)

  set.seed(seed)
  for (i in 1:n_sim) {
    if (i %% print == 0) {
      message(paste0("* Generating cube ", i, "/", n_sim))
    }
    # Get random point
    test_points <-
      points %>%
      st_drop_geometry() %>%
      mutate(random_angle = runif(nrow(points), 0, 2*pi),
             random_r = sqrt(runif(nrow(points), 0, 1)) *
               coordinateUncertaintyInMeters)

    test_points2 <-
      test_points %>%
      mutate(x_new = x + random_r * cos(random_angle),
             y_new = y + random_r * sin(random_angle)) %>%
      st_as_sf(coords = c("x_new", "y_new"), crs = st_crs(31370))

    # We assign each occurrence to a grid cell
    intersect_grid <- st_intersection(test_points2, grid)

    # Aggregate to get the cube
    occ_cube <- intersect_grid %>%
      st_drop_geometry() %>%
      group_by(id) %>%
      summarize(n = n(),
                min_coord_uncertainty = min(coordinateUncertaintyInMeters)) %>%
      ungroup() %>%
      mutate(sim = i)

    out[[i]] <- occ_cube
  }

  final_occs <- do.call(rbind.data.frame, out)

  # Add zeroes
  design <- expand_grid(id = grid$id, sim = seq_len(n_sim))

  out_df <- final_occs %>%
    full_join(design, by = join_by(id, sim)) %>%
    mutate(n = ifelse(is.na(n), 0, n))

  return(out_df)
}

# Function to iterate over expected number of occurrences
iterate_occurrences <- function(iter_df, total_var, grouping_var) {
  if ("sf" %in% class(iter_df)) {
    merge_df <- iter_df
    iter_df <- iter_df %>%
      st_drop_geometry()
  } else {
    merge_df <- iter_df
    iter_df <- iter_df %>%
      st_drop_geometry()
  }
  out <- vector(mode = "list", length = nrow(distinct(iter_df[grouping_var])))
  # Loop over grouping variable
  for (i in seq_along(pull(distinct(test[grouping_var])))) {
    # Filter data
    y <- pull(distinct(test[grouping_var]))[i]
    df <- iter_df %>%
      filter(year == y,
             !is.na(expected_n)) %>%
      mutate(counting = floor(expected_n + 1e-6)) %>%
      mutate(iter = expected_n - counting)
    # Set minimum
    count <- sum(df$counting)
    iter <- 1
    # Set maximum
    total <- df[total_var] %>%
      distinct() %>%
      pull()

    print(paste0("Iteration ", iter, ", year ", y, ": count ", count))
    while (count != total) {
      id_to_count <- df$id[df$iter == max(df$iter)]
      df <- df %>%
        mutate(counting = ifelse(id == id_to_count, counting + 1, counting),
               iter = expected_n - counting)
      count <- sum(df$counting)
      iter <- iter + 1
      print(paste0("Iteration ", iter, ", year ", y, ": count ", count))
    }

    out[[i]] <- merge_df %>%
      filter(year == y) %>%
      full_join(df) %>%
      mutate(counting = ifelse(is.na(counting), 0, counting))
  }

  return(do.call(rbind.data.frame, out))
}
