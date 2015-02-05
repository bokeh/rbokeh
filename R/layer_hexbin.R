#' @export
ly_hexbin <- function(fig, x, y, data = NULL, xbins = 30, shape = 1,
  style = "colorramp",
  minarea = 0.04, maxarea = 0.8, mincnt = 1, maxcnt = NULL,
  trans = NULL, inv = NULL,
  palette = "RdYlGn11", fill = TRUE, line = FALSE, hover = TRUE) {

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
  }

  ind <- complete.cases(x, y)

  hbd <- get_hexbin_data(x = x[ind], y = y[ind], xbins = xbins, shape = shape,
    style = style, minarea = minarea, maxarea = maxarea, mincnt = mincnt,
    maxcnt = maxcnt, trans = trans, inv = inv)

  if(is.character(palette)) {
    if(valid_color(palette)) {
      col <- palette
    } else {
      if(!palette %in% bk_palette_names)
        stop("'palette' specified in ly_hexbin is not a valid color name or palette - see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html", call. = FALSE)
      palette <- colorRampPalette(bk_palettes[[palette]])
    }
    if(is.function(palette)) {
      colorcut <- seq(0, 1, length = 10)
      nc <- length(colorcut)
      colgrp <- cut(hbd$rcnt, colorcut, labels = FALSE, include.lowest = TRUE)
      clrs <- palette(length(colorcut) - 1)
      col <- clrs[colgrp]
    }
  }

  if(xname == yname) {
    xname <- paste(xname, "(x)")
    yname <- paste(yname, "(y)")
  }
  names(hbd$data)[1:2] <- c(xname, yname)

  fig %>% ly_polygon(xs = hbd$xs, ys = hbd$ys, color = col, hover = hbd$data,
    xlab = xname, ylab = yname)
}
# figure() %>% ly_hexbin(rnorm(1000), rnorm(1000), style = "lattice")



get_hexbin_data <- function(x, y, xbins = 30, shape = 1, xbnds = range(x), ybnds = range(y),
  style = "lattice", minarea = 0.04, maxarea = 0.8, mincnt = 1, maxcnt = NULL, trans = NULL, inv = NULL) {

  if(is.null(xbnds))
    xbnds <- range(x, na.rm = TRUE)

  if(is.null(ybnds))
    ybnds <- range(y, na.rm = TRUE)

  dat <- hexbin(x, y, shape = shape, xbins = xbins, xbnds = xbnds, ybnds = ybnds)

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
