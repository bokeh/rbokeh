
test_that("markers", {
  alpha <- c(6:2) / 10
  colors <- c("pink", "orange", "red", "blue", "green")
  xr <- yr <- c(0, 6)

  ### asterisk
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "asterisk",
    line_color = colors, size = seq(12, 40, length = 5),
    line_width = c(2:6) / 2)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### circle (no radius)
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "circle",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### circle_cross
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "circle_cross",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### circle_x
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "circle_x",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### cross
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "cross",
    size = seq(12, 40, length = 5),
    line_width = 3, line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### diamond
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "diamond",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### diamond_cross
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "diamond_cross",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### inverted_triangle
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "inverted_triangle",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### square
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "square",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### square_cross
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "square_cross",
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### square_x
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "square_x",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### triangle
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "triangle",
    size = seq(12, 40, length = 5),
    line_color = colors, fill_color = colors, fill_alpha = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### x
  a <- figure(xlim = xr, ylim = yr) %>%
    lay_points(x = 1:5, y = 5:1, type = "x",
    size = seq(12, 40, length = 5),
    line_color = colors, line_width = c(2:6) / 2)
  a

  a$xlim <- a$ylim <- NULL
  a
})

test_that("glyphs", {
  alpha <- c(6:2) / 10
  colors <- c("pink", "orange", "red", "blue", "green")
  xr <- yr <- c(0, 6)

  ### annular_wedge
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_annular_wedge(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1:5) / 2,
    inner_radius = 0.3, outer_radius = 0.7,
    fill_color = colors, fill_alpha = alpha, 
    line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### annulus
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_annulus(x = 1:5, y = 5:1, 
    inner_radius = c(2:6) / 10, 
    outer_radius = 0.8,
    fill_color = colors, line_color = colors, 
    fill_alpha = alpha)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### arc
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_arc(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1.5, 2, 2.5, 3.0, 4.5),
    line_color = colors, line_width = 1:5, radius = 0.5)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### bezier
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_bezier(x0 = 1:5, x1 = 2:6, y0 = 5:1, y1 = 5:1, 
    cx0 = 1:5, cy0 = rep(6, 5), cx1 = 4, cy1 = rep(6, 5), 
    line_width = 2, line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### image
  N <- 300
  d1 <- rep(1:N, each = N)
  d2 <- rep(1:N, times = N)
  d <- sin(10*d1/N)*cos(20*d2/N)
  a <- figure(xlim = c(0, 10), ylim = c(0, 10)) %>%
    lay_image(x = 0, y = 0, dw = 10, dh = 10,
      image = d, rows = N, cols = N,
      palette = "Spectral-10")
  a

  a$xlim <- a$ylim <- NULL
  a

  ### line
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_line(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
    line_color = "#43A2CA",
    line_dash = c(5, 2), line_width = 2)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### multi_line
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_multi_line(xs = list(c(1, 2, 3), c(2, 3, 4), 
      c(0, 1, 0), c(2, 5, 4), c(5, 3, 2)),
    ys = list(c(5, 2, 3), c(1, 1, 4), 
      c(5, 5, 2), c(4, 1, 5), c(3, 5, 5)),
    line_width = c(1, 1.5, 2, 2.5, 3),
    line_color = colors,
    line_dash = c(5, 2))
  a

  a$xlim <- a$ylim <- NULL
  a

  ### oval
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_oval(x = 1:5, y = 5:1,
    angle = c(1:5) / 5, width = c(1:5) / 5,
    height = c(5:1) / 5, line_color = colors,
    fill_color = colors, fill_alpha = alpha)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### patch
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_polygon(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
    line_width = 2, line_dash = c(5, 2, 5, 6),
    line_color = "#2C7FB8", fill_color = "#7FCDBB")
  a

  a$xlim <- a$ylim <- NULL
  a

  ### patches
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_polygons(xs = list(c(1, 2, 3), c(2, 3, 4), 
      c(0, 1, 0), c(2, 5, 4), c(5, 3, 2)),
    ys = list(c(5, 2, 3), c(1, 1, 4), c(5, 5, 2), 
      c(4, 1, 5), c(3, 5, 4)),
    line_width = 2, line_dash = c(5, 2),
    fill_alpha = alpha, fill_color = colors,
    line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### quad
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_rect(ytop = c(5.1, 4.2, 3.7, 2.4, 1.5),
    ybottom = c(4.9, 3.8, 2.3, 1.6, 0.5),
    xleft = c(0.9, 1.6, 2.7, 3.6, 4.4),
    xright = c(1.1, 2.4, 3.3, 4.4, 5.7),
    fill_alpha = alpha, fill_color = colors,
    line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### quadratic
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_quadratic(x0 = 1:5, x1 = 2:6, y0 = 5:1, y1 = 5:1, 
    cx = 1:5, cy = rep(6, 5), 
    line_width = 2, line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### ray
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_ray(x = 1:5, y = 5:1,
    angle = c(1:5) / 2, length = 30,
    line_width = 2, line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### rect
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_crect(x = 1:5, y = 5:1,
    angle = c(1:5) / 5, 
    width = c(1:5) / 5, height = c(5:1) / 5,
    fill_alpha = alpha, 
    line_color = colors, fill_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### segment
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_segments(x0 = 1:5, x1 = 2:6, y0 = 5:1, 
    y1 = c(6, 4.8, 3.6, 2.4, 1.2), 
    line_width = 2, line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### text
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_text(x = 1:5, y = 5:1, 
    text = c("foo", "bar", "baz", "hello", "world"),
    angle = c(1:5) / 10,
    text_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a

  ### wedge
  a <- figure(xlim = xr, ylim = yr) %>%
  lay_wedge(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1:5) / 2,
    radius = 0.7,
    fill_color = colors, fill_alpha = alpha, 
    line_color = colors)
  a

  a$xlim <- a$ylim <- NULL
  a
})


test_that("lay_points with plotting characters", {
  p <- figure() %>%
    lay_points(rnorm(10), rnorm(10), type = 0) %>%
    lay_points(rnorm(10), rnorm(10), type = 1) %>%
    lay_points(rnorm(10), rnorm(10), type = 2) %>%
    lay_points(rnorm(10), rnorm(10), type = 3) %>%
    lay_points(rnorm(10), rnorm(10), type = 4)
  p

  p <- figure() %>%
    lay_points(rnorm(10), rnorm(10), type = 21) %>%
    lay_points(rnorm(10), rnorm(10), type = 22) %>%
    lay_points(rnorm(10), rnorm(10), type = 23) %>%
    lay_points(rnorm(10), rnorm(10), type = 24) %>%
    lay_points(rnorm(10), rnorm(10), type = 25)
  p
})

test_that("lay_points with other options", {
  p <- figure(height = 600, width = 600) %>%
    lay_points(rnorm(50), rnorm(50), type = 21, fill_alpha = 0.3, line_alpha = 0.3) %>%
    lay_points(rnorm(50), rnorm(50), type = 22, fill_alpha = 0.3, line_alpha = 0.3) %>%
    lay_points(rnorm(50), rnorm(50), type = 23, fill_alpha = 0.3, line_alpha = 0.3) %>%
    lay_points(rnorm(50), rnorm(50), type = 24, fill_alpha = 0.3, line_alpha = 0.3) %>%
    lay_points(rnorm(50), rnorm(50), type = 25, fill_alpha = 0.3, line_alpha = 0.3)
  p
})

test_that("lay_points with bg colors", {
  p <- figure(height = 600, width = 600) %>%
    lay_points(rnorm(50), rnorm(50), type = 21, fill_color = "blue", size = 20) %>%
    lay_points(rnorm(50), rnorm(50), type = 22, fill_color = "green", size = 30)
  p

  expect_equal(p$glyphSpecs[[1]]$fill_color, "blue")
  expect_equal(p$glyphSpecs[[2]]$fill_color, "green")

  thm <- getOption("bokeh_theme")$glyph
  expect_equal(p$glyphSpecs[[1]]$line_color, thm[1])
  expect_equal(p$glyphSpecs[[2]]$line_color, thm[2])
})

test_that("lay_points with line colors", {
  p <- figure(height = 600, width = 600) %>%
    lay_points(rnorm(50), rnorm(50), type = 21, line_color = "blue", size = 20) %>%
    lay_points(rnorm(50), rnorm(50), type = 22, line_color = "green", size = 30)
  p

  # check outline color (should match what was specified)
  expect_equal(p$glyphSpecs[[1]]$line_color, "blue")
  expect_equal(p$glyphSpecs[[2]]$line_color, "green")

  # fill color should be saturation-adjusted value of outline color
  expect_equal(p$glyphSpecs[[1]]$fill_color, rbokeh:::reduceSaturation(p$glyphSpecs[[1]]$line_color))
  expect_equal(p$glyphSpecs[[2]]$fill_color, rbokeh:::reduceSaturation(p$glyphSpecs[[2]]$line_color))
})

test_that("lay_points with pch having no outline", {
  # should be solid colors
  p <- figure(height = 600, width = 600) %>%
    lay_points(rnorm(50), rnorm(50), type = 15, fill_color = "blue") %>%
    lay_points(rnorm(50), rnorm(50), type = 16, fill_color = "green")
  p

  expect_equal(p$glyphSpecs[[1]]$line_color, NULL)
  expect_equal(p$glyphSpecs[[2]]$line_color, NULL)
})

test_that("lay_points with pch as text", {
  p <- figure(height = 600, width = 600) %>%
    lay_points(rnorm(50), rnorm(50), type = "a")
  p
  thm <- getOption("bokeh_theme")$glyph
  expect_equal(p$glyphSpecs[[1]]$text_color, thm[1])

  p <- figure(height = 600, width = 600) %>%
    lay_points(rnorm(50), rnorm(50), type = "a", text_color = "red")
  p
  expect_equal(p$glyphSpecs[[1]]$text_color, "red")
})

test_that("lay_points and lines together", {
  p <- figure() %>%
    lay_points(runif(10), runif(10)) %>%
    lay_lines(c(0, 1), c(0, 1), line_color = "black", line_dash = 2)
  p
})

test_that("lines with single vector input", {
  p <- figure() %>%
    lay_lines(rnorm(100))
  p
})

test_that("lines with different lty", {
  set.seed(1234)
  p <- figure() %>%
    lay_lines(rnorm(10), line_dash = 1, line_width = 2, line_alpha = 0.6) %>%
    lay_lines(rnorm(10), line_dash = 2, line_width = 2, line_alpha = 0.6) %>%
    lay_lines(rnorm(10), line_dash = 3, line_width = 2, line_alpha = 0.6) %>%
    lay_lines(rnorm(10), line_dash = 4, line_width = 2, line_alpha = 0.6) %>%
    lay_lines(rnorm(10), line_dash = 5, line_width = 2, line_alpha = 0.6) %>%
    lay_lines(rnorm(10), line_dash = 6, line_width = 2, line_alpha = 0.6)
  p
})

test_that("lowess example", {
  p <- figure() %>%
    lay_points(cars) %>%
    lay_lines(lowess(cars), line_color = "black")
  p
})

test_that("histogram", {
  h <- figure() %>%
    lay_hist(faithful$eruptions, breaks = 40)
  h

  ## add another one
  h <- h %>% lay_hist(runif(100, 1, 5), breaks = 30, line_alpha = 0.3, fill_alpha = 0.3)
  h
})

test_that("segments", {
  p <- figure() %>%
    lay_segments(rnorm(100), rnorm(100), rnorm(100), rnorm(100), line_alpha = 0.5, line_width = 2, line_color = rainbow(100))
  p
})

test_that("abline", {
  p <- figure() %>%
    lay_points(rnorm(100), rnorm(100)) %>%
    lay_abline(0, 1, line_color = "black") %>%
    lay_abline(h = 1, line_color = "red") %>%
    lay_abline(v = 1, line_color = "blue")
  p
})

test_that("multiple abline hv", {
  p <- figure() %>%
    lay_points(rnorm(100), rnorm(100)) %>%
    lay_abline(h = seq(-3, 3, length = 25)) %>%
    lay_abline(v = seq(-3, 3, length = 25))
  p
})

test_that("multiple abline ab", {
  p <- figure() %>%
    lay_points(rnorm(100), rnorm(100)) %>%
    lay_abline(0, seq(-10, 10, length = 100))
  p
})

test_that("abline coef", {
  z <- lm(dist ~ speed, data = cars)
  p <- figure() %>%
    lay_points(cars) %>%
    lay_abline(z)
  p
})

test_that("curve", {
  ff <- function(x) x^2
  xx <- c(-10:10)
  yy <- ff(xx)
  p <- figure() %>%
    lay_points(xx, yy) %>%
    lay_curve(ff, -10, 10)
  p
})

# test_that("curve with no limits", {
#   ff <- function(x) x^2
#   xx <- c(-10:10)
#   yy <- ff(xx)
#   p <- figure() %>%
#     lay_points(xx, yy) %>%
#     lay_curve(ff)
#   p
# })

test_that("rect", {
  p <- figure() %>%
    lay_rect(1:10, 1:10, 2:11, 2:11)
  p
})

test_that("rect char", {
  p <- figure() %>%
    lay_rect(letters[1:10], letters[1:10], letters[2:11], letters[2:11])
  p
})

test_that("crect", {
  p <- figure() %>%
    lay_crect(1:10, 1:10, 0.9, 0.9)
  p
})

test_that("crect char", {
  p <- figure() %>%
    lay_crect(letters[1:10], letters[1:10], 1, 1, fill_alpha = 0.8)
  p
})

test_that("polygon", {
  require(maps)
  mdat <- map("state", "washington", fill = TRUE, plot = FALSE)
  n <- length(mdat$x)
  p <- figure() %>%
    lay_polygon(mdat$x[102:n], mdat$y[102:n])
  p
})

test_that("polygons", {
  map2df <- function(a) {
    dd <- data.frame(lon = a$x, lat = a$y, 
      group = cumsum(is.na(a$x) & is.na(a$y)) + 1)
    dd[complete.cases(dd$lon, dd$lat), ]
  }
  require(maps)
  mdat <- map("county", "washington", fill = TRUE, plot = FALSE)
  mdat <- map2df(mdat)

  p <- figure() %>%
    lay_polygons(lon, lat, group = group, data = mdat)
  p
})

test_that("polygons list", {
  map2df <- function(a) {
    dd <- data.frame(lon = a$x, lat = a$y, 
      group = cumsum(is.na(a$x) & is.na(a$y)) + 1)
    dd[complete.cases(dd$lon, dd$lat), ]
  }
  require(maps)
  mdat <- map("county", "washington", fill = TRUE, plot = FALSE)
  mdat <- map2df(mdat)
  xs <- split(mdat$lon, mdat$group)
  ys <- split(mdat$lat, mdat$group)
  p <- figure() %>%
    lay_polygons(xs, ys)
  p
})

test_that("contour", {
  p <- figure() %>%
    lay_image(volcano) %>%
    lay_contour(volcano, line_color = "black", line_alpha = 0.5)
  p

  p <- figure() %>%
    lay_contour(volcano)
  p
})

test_that("quantile", {
  p <- figure() %>%
    lay_quantile(Sepal.Length, group = Species, data = iris)
  p

  p <- figure() %>%
    lay_quantile(Sepal.Length, group = Species, data = iris, probs = c(0.25, 0.5, 0.75))
  p

  p <- figure() %>%
    lay_quantile(rnorm(1000), distn = qnorm) %>%
    lay_abline(0, 1, line_color = "black", line_width = 2)
  p

  p <- figure() %>%
    lay_quantile(rnorm(1000), group = sample(1:3, 1000, replace = TRUE), distn = qnorm) %>%
    lay_abline(0, 1, line_color = "black", line_width = 2)
  p
})

test_that("density", {
  h <- figure() %>%
    lay_density(faithful$eruptions, bw = 0.08)
  h

  h <- figure() %>%
    lay_hist(faithful$eruptions, breaks = 20, freq = FALSE) %>%
    lay_density(faithful$eruptions)
  h
})


test_that("misc", {
  a <- figure() %>%
    lay_points(letters, 1:26)
  a

  a <- figure() %>%
    lay_points("a", 1)
  a
})

test_that("map", {
  require(maps)
  data(us.cities)
  cities <- subset(us.cities, long > -130)

  p <- figure(width = 800, height = 550, padding_factor = 0) %>% 
    lay_map("county", fill_color = "#1F77B4", 
      line_color = "white", line_alpha = 0.2, fill_alpha = 0.5) %>%
    lay_map("state", line_color = "white") %>%
    lay_points(long, lat, data = cities, type = 19, size = 4, 
      fill_color = "black", fill_alpha = 0.75) %>%
    lay_text(long, lat, name, data = cities, text_font_size = "4pt")
})

test_that("boxplot", {
  figure() %>% lay_boxplot(rnorm(100))
  figure() %>% lay_boxplot(Sepal.Length, Species, data = iris)
})

# ### line with categorical x-axis
# a <- figure(ylim = yr)
# a$line(x = letters[1:5], y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA",
#   line_dash = c(5, 2), line_width = 2)
# a

# # now try to add numeric x (should error)
#   lay_points(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA")

# # now add new categorical lay_points
#   lay_points(x = letters[2:6], y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA")
# a