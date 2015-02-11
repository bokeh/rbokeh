
#' Display glyph types available for ly_points()
#' @param size size of the glyph
#' @param color color to use for line and fill properties
#' @param width,height dimensions of output plot
#' @export
point_types <- function(size = 25, color = "blue", width = 800, height = 450) {

  types <- c(as.list(marker_pch_types), as.list(marker_names))

  f <- figure(ylim = as.character(5:1), width = width, height = height, xlab = NULL, ylab = NULL)
  grid <- expand.grid(1:8, 1:5)
  for(ii in seq_along(types)) {
    cur_grid <- as.character(grid[ii,])
    f <- f %>%
      ly_points(cur_grid[1], cur_grid[2],
       glyph = types[[ii]], color = color, size = size)
  }

  f %>%
    ly_text(as.character(grid[,1]),
      paste(as.character(grid[,2]), ":0.1", sep = ""),
      unlist(types), font_size = "12px",
      align = "center", baseline = "bottom")
}
