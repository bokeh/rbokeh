\donttest{
figure() %>%
  ly_points(rexp(1000), rexp(1000)) %>%
  x_axis(label = "x", log = TRUE) %>%
  y_axis(label = "y", log = TRUE)

figure() %>%
  ly_points(2^(1:10)) %>%
  y_axis(log = 2)
}
