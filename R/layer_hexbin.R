#' Add a "hexbin" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x values or field name of center x coordinates to be binned
#' @param y values or field name of center y coordinates to be binned
#' @param data an optional data frame, providing the source for x and y
#' @param xbins,shape,xbnds,ybnds parameters passed to \code{\link[hexbin]{hexbin}}
#' @param style type of plotting for hexbins (see \code{\link[hexbin]{grid.hexagons}}) - "colorramp" and "lattice" are currently supported
#' @param trans,inv transformation and inverse transformation function for the bin counts
#' @param lname layer name
#' @param palette name of color palette to use for color ramp (see \href{http://bokeh.pydata.org/en/latest/docs/reference/palettes.html}{here} for acceptable values)
#' @param line logical - should hexagons have an outline?
#' @param alpha the alpha transparency of the hexagons between 0 (transparent) and 1 (opaque)
#' @param hover logical - should a hover tool be added to show the count in each hexagon?
#' @param visible should the layer be visible?
#' @examples
#' \donttest{
#' figure() %>% ly_hexbin(rnorm(10000), rnorm(10000))
#' }
#' @export
ly_hexbin <- function(
  fig, x, y = NULL, data = figure_data(fig),
  xbins = 30, shape = 1, xbnds = NULL, ybnds = NULL,
  style = "colorscale",
  trans = NULL, inv = NULL, lname = NULL,
  palette = "RdYlGn11", line = FALSE, alpha = 1,
  hover = TRUE, visible = TRUE
) {

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      xbins,
      shape,
      # style,
      # trans,
      # inv,
      # palette,
      hover,
      line,
      alpha,
      visible,
      dots = lazy_dots()
    ),
    process_data_and_names = FALSE
  )

  minarea <- 0.04; maxarea <- 0.8; mincnt <- 1; maxcnt <- NULL

  if(!inherits(args$data$x, "hexbin")) {
    xy_names <- get_xy_names(args$data$x, args$data$y, deparse(substitute(x)), deparse(substitute(y)), NULL)
    xy <- get_xy_data(args$data$x, args$data$y)
    args$data$x <- xy$x
    args$data$y <- xy$y
    args$info$x_name <- xy_names$x
    args$info$y_name <- xy_names$y

    hbd <- get_hexbin_data(x = xy$x, y = xy$y, xbins = xbins,
      shape = shape, xbnds = xbnds, ybnds = ybnds)
  } else {
    args$info$x_name <- "x"
    args$info$y_name <- "y"

    hbd <- args$data$x
  }

  hbd <- get_from_hexbin(hbd, maxcnt = maxcnt,
    mincnt =mincnt, trans = trans, inv = inv, style = style,
    minarea = minarea, maxarea = maxarea)

  if(is.character(palette)) {
    if(valid_color(palette)) {
      col <- palette
    } else {
      if(!palette %in% bk_gradient_palette_names)
        stop("'palette' specified in ly_hexbin is not a valid color name or palette - see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html", call. = FALSE)
      palette <- colorRampPalette(bk_gradient_palettes[[palette]])
    }
  }

  if(is.function(palette)) {
    colorcut <- seq(0, 1, length = 100)
    nc <- length(colorcut)
    colgrp <- cut(hbd$rcnt, colorcut, labels = FALSE, include.lowest = TRUE)
    clrs <- palette(length(colorcut) - 1)
    col <- clrs[colgrp]
  }

  if(args$info$x_name == args$info$y_name) {
    args$info$x_name <- paste(args$info$x_name, "(x)")
    args$info$y_name <- paste(args$info$y_name, "(y)")
  }
  names(hbd$data)[1:2] <- c(args$info$x_name, args$info$y_name)

  if(!line) {
    line_color <- NA
  } else {
    # TODO
    # this could be reached and never have been set
    line_color <- col
  }

  if(is.logical(hover) && !hover)
    hbd$data <- NULL

  fig %>% ly_polygons(
    xs = hbd$xs, ys = hbd$ys, color = NULL,
    fill_color = col, alpha = NULL,
    fill_alpha = args$params$alpha, line_color = line_color,
    hover = hbd$data, xlab = args$info$x_name, ylab = args$info$y_name,
    lname = lname
  )
}


#' @importFrom hexbin hexbin
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexcoords
get_hexbin_data <- function(x, y, xbins = 30, shape = 1,
  xbnds = range(x, na.rm = TRUE),
  ybnds = range(y, na.rm = TRUE)) {

  if(is.null(xbnds))
    xbnds <- range(x, na.rm = TRUE)

  if(is.null(ybnds))
    ybnds <- range(y, na.rm = TRUE)

  ind <- stats::complete.cases(x, y)
  hexbin(x[ind], y[ind], shape = shape, xbins = xbins, xbnds = xbnds, ybnds = ybnds)
}


get_from_hexbin <- function(dat, maxcnt = NULL, mincnt = 1, trans = identity, inv = identity, maxarea = 0.8, minarea = 0.04, style = style) {

  cnt <- dat@count
  xbins <- dat@xbins
  shape <- dat@shape
  tmp <- hcell2xy(dat)
  if(is.null(maxcnt))
    maxcnt <- max(dat@count)

  ok <- cnt >= mincnt & cnt <= maxcnt

  xnew <- tmp$x[ok]
  ynew <- tmp$y[ok]
  cnt <- cnt[ok]

  sx <- xbins / diff(dat@xbnds)
  sy <- (xbins * shape) / diff(dat@ybnds)

  if (is.null(trans)) {
     if (min(cnt, na.rm = TRUE) < 0) {
        pcnt <- cnt + min(cnt)
        rcnt <- {
           if (maxcnt == mincnt) rep.int(1, length(cnt)) else (pcnt - mincnt) / (maxcnt - mincnt)
        }
     } else rcnt <- {
        if (maxcnt == mincnt) rep.int(1, length(cnt)) else (cnt - mincnt) / (maxcnt - mincnt)
     }
  } else {
     rcnt <- (trans(cnt) - trans(mincnt)) / (trans(maxcnt) - trans(mincnt))
     if (any(is.na(rcnt))) stop("bad count transformation")
  }

  if(style == "lattice") {
    area <- minarea + rcnt * (maxarea - minarea)
    area <- pmin(area, maxarea)
    radius <- sqrt(area)
  } else {
    radius <- rep(1, length(xnew))
  }

  inner <- 0.5
  outer <- (2 * inner) / sqrt(3)
  dx <- inner / sx
  dy <- outer / (2 * sy)
  # rad <- sqrt(dx^2 + dy^2)
  hex_c <- hexcoords(dx, dy, sep = NULL)

  xs <- lapply(seq_along(xnew), function(i)
    hex_c$x * radius[i] + xnew[i])
  ys <- lapply(seq_along(xnew), function(i)
    hex_c$y * radius[i] + ynew[i])

  list(xs = xs, ys = ys, data = data.frame(x = xnew, y = ynew, count = cnt), rcnt = rcnt)
}
