
#' @export
point_types <- function(width = 1100, height = 600, size = 25, col = "blue") {

  types <- c(as.list(markerPchTypes), as.list(markerNames))

  f <- figure(ylim = as.character(5:1), width = width, height = height, xlab = NULL, ylab = NULL)
  grid <- expand.grid(1:8, 1:5)
  for(ii in seq_along(types)) {
    curGrid <- as.character(grid[ii,])
    f <- f %>% 
      lay_points(curGrid[1], curGrid[2], 
       type = types[[ii]], line_color = col, 
       fill_color = reduceSaturation(col), size = size)
  }
  f %>%
    lay_text(as.character(grid[,1]), 
      paste(as.character(grid[,2]), ":0.15", sep = ""), 
      unlist(types),
      text_align = "center", text_baseline = "bottom")
}

