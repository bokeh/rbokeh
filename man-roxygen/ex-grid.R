\donttest{
idx <- split(1:150, iris$Species)
figs <- lapply(idx, function(x) {
  figure(width = 300, height = 300) %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris[x, ],
      hover = list(Sepal.Length, Sepal.Width))
})

# 1 row, 3 columns
grid_plot(figs)
# specify xlim and ylim to be applied to all panels
grid_plot(figs, xlim = c(4, 8), ylim = c(1.5, 4.5))
# unnamed list will remove labels
grid_plot(unname(figs))
# 2 rows, 2 columns
grid_plot(figs, nrow = 2)
# x and y axis with same (and linked) limits
grid_plot(figs, same_axes = TRUE)
# x axis with same (and linked) limits
grid_plot(figs, same_axes = c(TRUE, FALSE), nrow = 2)
# x axis with same (and linked) limits and custom xlim
grid_plot(figs, same_axes = c(TRUE, FALSE), xlim = c(5, 7), nrow = 2)
# send lists instead of specifying nrow and ncol
grid_plot(list(
  c(list(figs[[1]]), list(figs[[3]])),
  c(list(NULL), list(figs[[2]]))
))
# a null entry will be skipped in the grid
figs2 <- figs
figs2[1] <- list(NULL)
grid_plot(figs2, nrow = 2)
# with themes
grid_plot(figs) %>%
  theme_title(text_color = "red") %>%
  theme_plot(background_fill_color = "#E6E6E6",
    outline_line_color = "white") %>%
  theme_grid(c("x", "y"), grid_line_color = "white",
    minor_grid_line_color = "white",
    minor_grid_line_alpha = 0.4) %>%
  theme_axis(c("x", "y"), axis_line_color = "white",
    major_label_text_color = "#7F7F7F",
    major_tick_line_color = "#7F7F7F",
    minor_tick_line_alpha = 0, num_minor_ticks = 2)
# themes again
grid_plot(figs) %>%
  set_theme(bk_ggplot_theme)

# link data across plots in the grid (try box_select tool)
# (data sources must be the same)
tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "resize", "reset")
p1 <- figure(tools = tools, width = 500, height = 500) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris, color = Species)
p2 <- figure(tools = tools, width = 500, height = 500) %>%
  ly_points(Petal.Length, Petal.Width, data = iris, color = Species)
grid_plot(list(p1, p2), same_axes = TRUE, link_data = TRUE)
}
