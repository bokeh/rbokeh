
#' Display glyph types available for ly_point()
#' @param size size of the glyph
#' @param color color to use for line and fill properties
#' @export
point_types <- function(size = 25, color = "blue") {

  width <- 1100; height <- 600

  types <- c(as.list(marker_pch_types), as.list(marker_names))

  f <- figure(ylim = as.character(5:1), width = width, height = height, xlab = NULL, ylab = NULL)
  grid <- expand.grid(1:8, 1:5)
  for(ii in seq_along(types)) {
    cur_grid <- as.character(grid[ii,])
    f <- f %>%
      ly_point(cur_grid[1], cur_grid[2],
       glyph = types[[ii]], color = color, size = size)
  }

  f %>%
    ly_text(as.character(grid[,1]),
      paste(as.character(grid[,2]), ":0.15", sep = ""),
      unlist(types),
      text_align = "center", text_baseline = "bottom")
}
