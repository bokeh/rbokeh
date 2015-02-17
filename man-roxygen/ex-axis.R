\donttest{
figure(min_border = 2, background_color = "blue") %>%
  ly_points(rexp(1000), rexp(1000)) %>%
  x_axis(label = "x", log = TRUE) %>%
  y_axis(label = "y", log = TRUE, visible = FALSE)
}
