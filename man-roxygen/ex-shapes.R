theta <- seq(0, 4 * pi, length = 40)
r <- seq(0.5, 10, length = 40)

d <- data.frame(
  x1 = r * cos(theta),
  y1 = r * sin(theta),
  x2 = 2 * r * cos(theta),
  y2 = 2 * r * sin(theta))

figure() %>% ly_rect(x1, y1, x2, y2, data = d)
