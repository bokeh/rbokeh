\donttest{
idx <- split(1:150, iris$Species)
figs <- lapply(idx, function(x) {
  figure(tools = tools, width = 300, height = 300) %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris[x,],
      hover = list(Sepal.Length, Sepal.Width))
})

# 1 row, 3 columns
grid_plot(figs)
# unnamed list will remove labels
grid_plot(unname(figs))
# 2 rows, 2 columns
grid_plot(figs, nrow = 2)
# x and y axis with same (and linked) limits
grid_plot(figs, same_axes = TRUE)
# x axis with same (and linked) limits
grid_plot(figs, same_axes = c(TRUE, FALSE))
# send lists instead of specifying nrow and ncol
grid_plot(list(
  c(list(figs[[1]]), list(figs[[2]])),
  list(figs[[3]])
))
# a null entry will be skipped in the grid
figs[1] <- list(NULL)
grid_plot(figs, nrow = 2)

# link data across plots in the grid (try box_select tool)
# (data sources must be the same)
tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "resize", "reset")
p1 <- figure(tools = tools) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris, color = Species)
p2 <- figure(tools = tools) %>%
  ly_points(Petal.Length, Petal.Width, data = iris, color = Species)
grid_plot(list(p1, p2), same_axes = TRUE, link_data = TRUE)
}

