
## add more tests for categorical axes
## add more tests for adding glyphs with different axis types

test_that("everything (clean up later)", {
  alpha <- c(6:2) / 10
  colors <- c("pink", "orange", "red", "blue", "green")
  xr <- yr <- c(0, 6)

  ### annular_wedge
  a <- figure(xrange = xr, yrange = yr)
  a$annular_wedge(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1:5) / 2,
    inner_radius = 0.3, outer_radius = 0.7,
    fill_color = colors, fill_alpha = alpha, 
    line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### annulus
  a <- figure(xrange = xr, yrange = yr)
  a$annulus(x = 1:5, y = 5:1, 
    inner_radius = c(2:6) / 10, 
    outer_radius = 0.8,
    fill_color = colors, line_color = colors, 
    fill_alpha = alpha)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### arc
  a <- figure(xrange = xr, yrange = yr)
  a$arc(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1.5, 2, 2.5, 3.0, 4.5),
    line_color = colors, line_width = 1:5, radius = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### asterisk
  a <- figure(xrange = xr, yrange = yr)
  a$asterisk(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_width = c(2:6) / 2, line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### bezier
  a <- figure(xrange = xr, yrange = yr)
  a$bezier(x0 = 1:5, x1 = 2:6, y0 = 5:1, y1 = 5:1, 
    cx0 = 1:5, cy0 = rep(6, 5), cx1 = 4, cy1 = rep(6, 5), 
    line_width = 2, line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### circle (no radius)
  a <- figure(xrange = xr, yrange = yr)
  a$circle(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### circle_cross
  a <- figure(xrange = xr, yrange = yr)
  a$circle_cross(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### circle_x
  a <- figure(xrange = xr, yrange = yr)
  a$circle_x(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### cross
  a <- figure(xrange = xr, yrange = yr)
  a$cross(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_width = 3, line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### diamond
  a <- figure(xrange = xr, yrange = yr)
  a$diamond(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### image
  N <- 300
  d1 <- rep(1:N, each = N)
  d2 <- rep(1:N, times = N)
  d <- sin(10*d1/N)*cos(20*d2/N)
  a <- figure(xrange = c(0, 10), yrange = c(0, 10))
  a$image(x = 0, y = 0, dw = 10, dh = 10,
    image = d, rows = N, cols = N,
    palette = "Spectral-10")
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### inverted_triangle
  a <- figure(xrange = xr, yrange = yr)
  a$inverted_triangle(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### line
  a <- figure(xrange = xr, yrange = yr)
  a$line(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
    line_color = "#43A2CA",
    line_dash = c(5, 2), line_width = 2)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### multi_line
  a <- figure(xrange = xr, yrange = yr)
  a$multi_line(xs = list(c(1, 2, 3), c(2, 3, 4), 
      c(0, 1, 0), c(2, 5, 4), c(5, 3, 2)),
    ys = list(c(5, 2, 3), c(1, 1, 4), 
      c(5, 5, 2), c(4, 1, 5), c(3, 5, 5)),
    line_width = c(1, 1.5, 2, 2.5, 3),
    line_color = colors,
    line_dash = c(5, 2))
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### oval
  a <- figure(xrange = xr, yrange = yr)
  a$oval(x = 1:5, y = 5:1,
    angle = c(1:5) / 5, width = c(1:5) / 5,
    height = c(5:1) / 5, line_color = colors,
    fill_color = colors, fill_alpha = alpha)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### patch
  a <- figure(xrange = xr, yrange = yr)
  a$patch(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
    line_width = 2, line_dash = c(5, 2, 5, 6),
    line_color = "#2C7FB8", fill_color = "#7FCDBB")
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### patches
  a <- figure(xrange = xr, yrange = yr)
  a$patches(xs = list(c(1, 2, 3), c(2, 3, 4), 
      c(0, 1, 0), c(2, 5, 4), c(5, 3, 2)),
    ys = list(c(5, 2, 3), c(1, 1, 4), c(5, 5, 2), 
      c(4, 1, 5), c(3, 5, 4)),
    line_width = 2, line_dash = c(5, 2),
    fill_alpha = alpha, fill_color = colors,
    line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### quad
  a <- figure(xrange = xr, yrange = yr)
  a$quad(top = c(5.1, 4.2, 3.7, 2.4, 1.5),
    bottom = c(4.9, 3.8, 2.3, 1.6, 0.5),
    left = c(0.9, 1.6, 2.7, 3.6, 4.4),
    right = c(1.1, 2.4, 3.3, 4.4, 5.7),
    fill_alpha = alpha, fill_color = colors,
    line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### quadratic
  a <- figure(xrange = xr, yrange = yr)
  a$quadratic(x0 = 1:5, x1 = 2:6, y0 = 5:1, y1 = 5:1, 
    cx = 1:5, cy = rep(6, 5), 
    line_width = 2, line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### ray
  a <- figure(xrange = xr, yrange = yr)
  a$ray(x = 1:5, y = 5:1,
    angle = c(1:5) / 2, length = 30,
    line_width = 2, line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### rect
  a <- figure(xrange = xr, yrange = yr)
  a$rect(x = 1:5, y = 5:1,
    angle = c(1:5) / 5, 
    width = c(1:5) / 5, height = c(5:1) / 5,
    fill_alpha = alpha, 
    line_color = colors, fill_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### segment
  a <- figure(xrange = xr, yrange = yr)
  a$segment(x0 = 1:5, x1 = 2:6, y0 = 5:1, 
    y1 = c(6, 4.8, 3.6, 2.4, 1.2), 
    line_width = 2, line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### square
  a <- figure(xrange = xr, yrange = yr)
  a$square(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### square_cross
  a <- figure(xrange = xr, yrange = yr)
  a$square_cross(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### square_x
  a <- figure(xrange = xr, yrange = yr)
  a$square_x(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### text
  a <- figure(xrange = xr, yrange = yr)
  a$text(x = 1:5, y = 5:1, 
    text = c("foo", "bar", "baz", "hello", "world"),
    angle = c(1:5) / 10,
    text_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### triangle
  a <- figure(xrange = xr, yrange = yr)
  a$triangle(x = 1:5, y = 5:1, 
    size = seq(12, 40, length = 5),
    line_color = colors,
    fill_color = colors, fill_alpha = 0.5)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())

  ### wedge
  a <- figure(xrange = xr, yrange = yr)
  a$wedge(x = 1:5, y = 5:1, 
    start_angle = 0, end_angle = c(1:5) / 2,
    radius = 0.7,
    fill_color = colors, fill_alpha = alpha, 
    line_color = colors)
  invisible(a$show())

  a$xrange <- a$yrange <- NULL
  invisible(a$show())
})

# ### line with categorical x-axis
# a <- figure(yrange = yr)
# a$line(x = letters[1:5], y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA",
#   line_dash = c(5, 2), line_width = 2)
# invisible(a$show())

# # now try to add numeric x (should error)
# a$points(x = 1:5, y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA")

# # now add new categorical points
# a$points(x = letters[2:6], y = c(4, 5, 3, 5.5, 1), 
#   line_color = "#43A2CA")
# invisible(a$show())