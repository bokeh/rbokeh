
## add more tests for categorical axes
## add more tests for adding glyphs with different axis types

SHOW_PLOTS <- FALSE
# SHOW_PLOTS <- TRUE

getResult <- function(x) {
  if(SHOW_PLOTS) {
    x
  } else {
    invisible(x)
  }
}

test_that("markers", {
  alpha <- c(6:2) / 10
  colors <- c("pink", "orange", "red", "blue", "green")
  xr <- yr <- c(0, 6)

  ### asterisk
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "asterisk",
    col = colors, size = seq(12, 40, length = 5),
    lwd = c(2:6) / 2)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### circle (no radius)
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "circle",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### circle_cross
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "circle_cross",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### circle_x
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "circle_x",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### cross
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "cross",
    size = seq(12, 40, length = 5),
    lwd = 3, col = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### diamond
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "diamond",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### diamond_cross
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "diamond_cross",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### inverted_triangle
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "inverted_triangle",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### square
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "square",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### square_cross
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "square_cross",
    size = seq(12, 40, length = 5),
    col = colors,
    bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### square_x
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "square_x",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### triangle
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "triangle",
    size = seq(12, 40, length = 5),
    col = colors, bg = colors, fill_alpha = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### x
  a <- figure(xlim = xr, ylim = yr)
  a$points(x = 1:5, y = 5:1, type = "x",
    size = seq(12, 40, length = 5),
    col = colors, lwd = c(2:6) / 2)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())
})


test_that("glyphs", {
  alpha <- c(6:2) / 10
  colors <- c("pink", "orange", "red", "blue", "green")
  xr <- yr <- c(0, 6)

  ### annular_wedge
  a <- figure(xlim = xr, ylim = yr)
  a$annular_wedge(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1:5) / 2,
    inner_radius = 0.3, outer_radius = 0.7,
    fill_color = colors, fill_alpha = alpha, 
    line_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### annulus
  a <- figure(xlim = xr, ylim = yr)
  a$annulus(x = 1:5, y = 5:1, 
    inner_radius = c(2:6) / 10, 
    outer_radius = 0.8,
    fill_color = colors, line_color = colors, 
    fill_alpha = alpha)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### arc
  a <- figure(xlim = xr, ylim = yr)
  a$arc(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1.5, 2, 2.5, 3.0, 4.5),
    line_color = colors, line_width = 1:5, radius = 0.5)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### bezier
  a <- figure(xlim = xr, ylim = yr)
  a$bezier(x0 = 1:5, x1 = 2:6, y0 = 5:1, y1 = 5:1, 
    cx0 = 1:5, cy0 = rep(6, 5), cx1 = 4, cy1 = rep(6, 5), 
    line_width = 2, line_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### image
  N <- 300
  d1 <- rep(1:N, each = N)
  d2 <- rep(1:N, times = N)
  d <- sin(10*d1/N)*cos(20*d2/N)
  a <- figure(xlim = c(0, 10), ylim = c(0, 10))
  a$image(x = 0, y = 0, dw = 10, dh = 10,
    image = d, rows = N, cols = N,
    palette = "Spectral-10")
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### line
  a <- figure(xlim = xr, ylim = yr)
  a$line(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
    line_color = "#43A2CA",
    line_dash = c(5, 2), line_width = 2)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### multi_line
  a <- figure(xlim = xr, ylim = yr)
  a$multi_line(xs = list(c(1, 2, 3), c(2, 3, 4), 
      c(0, 1, 0), c(2, 5, 4), c(5, 3, 2)),
    ys = list(c(5, 2, 3), c(1, 1, 4), 
      c(5, 5, 2), c(4, 1, 5), c(3, 5, 5)),
    line_width = c(1, 1.5, 2, 2.5, 3),
    line_color = colors,
    line_dash = c(5, 2))
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### oval
  a <- figure(xlim = xr, ylim = yr)
  a$oval(x = 1:5, y = 5:1,
    angle = c(1:5) / 5, width = c(1:5) / 5,
    height = c(5:1) / 5, line_color = colors,
    fill_color = colors, fill_alpha = alpha)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### patch
  a <- figure(xlim = xr, ylim = yr)
  a$patch(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
    line_width = 2, line_dash = c(5, 2, 5, 6),
    line_color = "#2C7FB8", fill_color = "#7FCDBB")
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### patches
  a <- figure(xlim = xr, ylim = yr)
  a$patches(xs = list(c(1, 2, 3), c(2, 3, 4), 
      c(0, 1, 0), c(2, 5, 4), c(5, 3, 2)),
    ys = list(c(5, 2, 3), c(1, 1, 4), c(5, 5, 2), 
      c(4, 1, 5), c(3, 5, 4)),
    line_width = 2, line_dash = c(5, 2),
    fill_alpha = alpha, fill_color = colors,
    line_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### quad
  a <- figure(xlim = xr, ylim = yr)
  a$quad(top = c(5.1, 4.2, 3.7, 2.4, 1.5),
    bottom = c(4.9, 3.8, 2.3, 1.6, 0.5),
    left = c(0.9, 1.6, 2.7, 3.6, 4.4),
    right = c(1.1, 2.4, 3.3, 4.4, 5.7),
    fill_alpha = alpha, fill_color = colors,
    line_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### quadratic
  a <- figure(xlim = xr, ylim = yr)
  a$quadratic(x0 = 1:5, x1 = 2:6, y0 = 5:1, y1 = 5:1, 
    cx = 1:5, cy = rep(6, 5), 
    line_width = 2, line_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### ray
  a <- figure(xlim = xr, ylim = yr)
  a$ray(x = 1:5, y = 5:1,
    angle = c(1:5) / 2, length = 30,
    line_width = 2, line_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### rect
  a <- figure(xlim = xr, ylim = yr)
  a$rect(x = 1:5, y = 5:1,
    angle = c(1:5) / 5, 
    width = c(1:5) / 5, height = c(5:1) / 5,
    fill_alpha = alpha, 
    line_color = colors, fill_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### segment
  a <- figure(xlim = xr, ylim = yr)
  a$segment(x0 = 1:5, x1 = 2:6, y0 = 5:1, 
    y1 = c(6, 4.8, 3.6, 2.4, 1.2), 
    line_width = 2, line_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### text
  a <- figure(xlim = xr, ylim = yr)
  a$text(x = 1:5, y = 5:1, 
    text = c("foo", "bar", "baz", "hello", "world"),
    angle = c(1:5) / 10,
    text_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())

  ### wedge
  a <- figure(xlim = xr, ylim = yr)
  a$wedge(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1:5) / 2,
    radius = 0.7,
    fill_color = colors, fill_alpha = alpha, 
    line_color = colors)
  getResult(a$show())

  a$xlim <- a$ylim <- NULL
  getResult(a$show())
})


test_that("points with plotting characters", {
  p <- figure()
  p$points(rnorm(10), rnorm(10), pch = 0)
  p$points(rnorm(10), rnorm(10), pch = 1)
  p$points(rnorm(10), rnorm(10), pch = 2)
  p$points(rnorm(10), rnorm(10), pch = 3)
  p$points(rnorm(10), rnorm(10), pch = 4)
  getResult(p$show())

  p <- figure()
  p$points(rnorm(10), rnorm(10), pch = 21)
  p$points(rnorm(10), rnorm(10), pch = 22)
  p$points(rnorm(10), rnorm(10), pch = 23)
  p$points(rnorm(10), rnorm(10), pch = 24)
  p$points(rnorm(10), rnorm(10), pch = 25)
  getResult(p$show())
})

test_that("points with other options", {
  p <- figure(height = 600, width = 600)
  p$points(rnorm(50), rnorm(50), pch = 21, alpha = 0.3)
  p$points(rnorm(50), rnorm(50), pch = 22, alpha = 0.3)
  p$points(rnorm(50), rnorm(50), pch = 23, alpha = 0.3)
  p$points(rnorm(50), rnorm(50), pch = 24, alpha = 0.3)
  p$points(rnorm(50), rnorm(50), pch = 25, alpha = 0.3)
  getResult(p$show())
})

test_that("points with bg colors", {
  p <- figure(height = 600, width = 600)
  p$points(rnorm(50), rnorm(50), pch = 21, bg = "blue", cex = 1.5)
  p$points(rnorm(50), rnorm(50), pch = 22, bg = "green", cex = 2)
  getResult(p$show())

  expect_equal(p$.glyphSpecs[[1]]$fill_color, "blue")
  expect_equal(p$.glyphSpecs[[2]]$fill_color, "green")

  thm <- getOption("bokeh_theme")$glyph
  expect_equal(p$.glyphSpecs[[1]]$line_color, thm[1])
  expect_equal(p$.glyphSpecs[[2]]$line_color, thm[2])
})

test_that("points with line colors", {
  p <- figure(height = 600, width = 600)
  p$points(rnorm(50), rnorm(50), pch = 21, col = "blue", cex = 1.5)
  p$points(rnorm(50), rnorm(50), pch = 22, col = "green", cex = 2)
  getResult(p$show())

  # check outline color (should match what was specified)
  expect_equal(p$.glyphSpecs[[1]]$line_color, "blue")
  expect_equal(p$.glyphSpecs[[2]]$line_color, "green")

  # fill color should be saturation-adjusted value of outline color
  expect_equal(p$.glyphSpecs[[1]]$fill_color, rbokeh:::reduceSaturation(p$.glyphSpecs[[1]]$line_color))
  expect_equal(p$.glyphSpecs[[2]]$fill_color, rbokeh:::reduceSaturation(p$.glyphSpecs[[2]]$line_color))
})

test_that("points with pch having no outline", {
  # should be solid colors
  p <- figure(height = 600, width = 600)
  p$points(rnorm(50), rnorm(50), pch = 15, bg = "blue")
  p$points(rnorm(50), rnorm(50), pch = 16, bg = "green")
  getResult(p$show())

  expect_equal(p$.glyphSpecs[[1]]$line_color, NULL)
  expect_equal(p$.glyphSpecs[[2]]$line_color, NULL)
})

test_that("points with pch as text", {
  p <- figure(height = 600, width = 600)
  p$points(rnorm(50), rnorm(50), pch = "a")
  getResult(p$show())
})

test_that("points and lines together", {
  p <- figure()
  p$points(runif(10), runif(10))
  p$lines(c(0, 1), c(0, 1), col = "black", lty = 2)
  getResult(p$show())
})

test_that("lines with single vector input", {
  p <- figure()
  p$lines(rnorm(100))
  getResult(p$show())
})

test_that("lines with different lty", {
  set.seed(1234)
  p <- figure()
  p$lines(rnorm(10), lty = 1, lwd = 2, alpha = 0.6)
  p$lines(rnorm(10), lty = 2, lwd = 2, alpha = 0.6)
  p$lines(rnorm(10), lty = 3, lwd = 2, alpha = 0.6)
  p$lines(rnorm(10), lty = 4, lwd = 2, alpha = 0.6)
  p$lines(rnorm(10), lty = 5, lwd = 2, alpha = 0.6)
  p$lines(rnorm(10), lty = 6, lwd = 2, alpha = 0.6)
  getResult(p$show())
})

test_that("lowess example", {
  cars.lo <- lowess(cars)
  p <- figure()
  p$points(cars)
  p$lines(cars.lo, col = "black")
  getResult(p$show())

  h <- figure()
  h$hist(faithful$eruptions, breaks = 40)
  getResult(h$show())
})


# ### line with categorical x-axis
# a <- figure(ylim = yr)
# a$line(x = letters[1:5], y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA",
#   line_dash = c(5, 2), line_width = 2)
# getResult(a$show())

# # now try to add numeric x (should error)
# a$points(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA")

# # now add new categorical points
# a$points(x = letters[2:6], y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA")
# getResult(a$show())