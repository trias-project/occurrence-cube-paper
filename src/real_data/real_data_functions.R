#' Function to select grid cells crossing the occurrences as circles
#'
#' Making a leaflet map or a plot with the entire grid is not very practical.
#' By this function we can select the grid cells (polygons) which intersect the
#' circles.
#'
get_cells_around_circles <- function(grid, circles) {
  cells_circles <- sf::st_intersects(
    x = grid,
    y = circles
  )
  cells_index <- purrr::map2_dbl(
    cells_circles, seq_along(cells_circles), function(x, y) {
      if (length(x) > 0) {
        y
      } else {
        NA
      }
    })
  cells_index <- cells_index[!is.na(cells_index)]

  return(grid[cells_index, ])
}
