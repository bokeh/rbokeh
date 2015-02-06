tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "resize", "reset")

p1 <- figure(tools = tools) %>%
  ly_point(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

p2 <- figure(tools = tools) %>%
  ly_point(Petal.Length, Petal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

# 1 row, 2 columns
grid_plot(list(p1, p2))
# x and y axis with same (and linked) limits
grid_plot(list(p1, p2), same_axes = TRUE)
# x axis has same (and linked) limits
grid_plot(list(p1, p2), same_axes = c(TRUE, FALSE))
# same axes and data is linked (try box_select tool)
grid_plot(list(p1, p2), same_axes = TRUE, link_data = TRUE)
# 1 column, 2 rows
grid_plot(list(p1, p2), ncol = 1)
# send lists instead of specifying nrow and ncol
grid_plot(list(c(list(p1), list(p2))))
grid_plot(list(list(p1), list(p2)))
