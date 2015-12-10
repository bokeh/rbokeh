\donttest{
url <- c("http://bokeh.pydata.org/en/latest/_static/bokeh-transparent.png",
  "http://developer.r-project.org/Logo/Rlogo-4.png")

ss <- seq(0, 2*pi, length = 13)[-1]
ws <- runif(12, 2.5, 5) * rep(c(1, 0.8), 6)

imgdat <- data.frame(
  x = sin(ss) * 10, y = cos(ss) * 10,
  w = ws, h = ws * rep(c(1, 0.76), 6),
  url = rep(url, 6)
)

p <- figure(xlab = "x", ylab = "y") %>%
  ly_image_url(x, y, w = w, h = h, image_url = url, data = imgdat,
    anchor = "center") %>%
  ly_lines(sin(c(ss, ss[1])) * 10, cos(c(ss, ss[1])) * 10,
    width = 15, alpha = 0.1)
p
}
