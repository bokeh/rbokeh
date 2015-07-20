\donttest{
tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "resize", "reset")

p1 <- figure(tools = tools) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

p2 <- figure(tools = tools) %>%
  ly_points(Petal.Length, Petal.Width, data = iris,
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



p1 <- figure(tools = tools) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

p2 <- figure(tools = tools) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

p3 <- figure(tools = tools) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

p4 <- figure(tools = tools) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

p5 <- figure(tools = tools) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

p6 <- figure(tools = tools) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = list(Sepal.Length, Sepal.Width))

grid_plot(list(p1, p2, p3, p4, p5, p6), nrow = 2)
grid_plot(list(p1, p2, p3, p4, p5, p6), nrow = 2, same_axes = TRUE)
grid_plot(list(c(list(p1), list(p2), list(p3)), c(list(p4), list(p5), list(p6))), same_axes = TRUE)

grid_plot(list(c(list(p1), list(NULL), list(p3)), c(list(p4), list(p5), list(NULL))), same_axes = TRUE)

grid_plot(list(p1, NULL, p3, p4, p5, p6), nrow = 2, same_axes = TRUE)

grid_plot(list(p1, NULL, p3, p4, p5, p6), nrow = 2, same_axes = TRUE, width = 300)

grid_plot(list(a = p1, b = NULL, c = p3, d = p4, e = p5, f = p6), nrow = 2, same_axes = TRUE, width = 800, height = 600)

}
