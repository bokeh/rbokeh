\donttest{
z <- lm(dist ~ speed, data = cars)
p <- figure() %>%
  ly_points(cars, hover = cars) %>%
  ly_lines(lowess(cars), legend = "lowess") %>%
  ly_abline(z, type = 2, legend = "lm", width = 2)
p
}
