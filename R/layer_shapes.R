


#' @export
ly_polygon <- function(fig, xs, ys, group = NULL, data = NULL,
  color = NULL, alpha = NULL,
  fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1,
  hover = NULL, # legend = NULL,
  lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_polygon")

  xname <- deparse(substitute(xs))
  yname <- deparse(substitute(ys))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    xs         <- v_eval(substitute(xs), data)
    ys         <- v_eval(substitute(ys), data)
    group      <- v_eval(substitute(group), data)
    color      <- v_eval(substitute(color), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_color <- v_eval(substitute(fill_color), data)
  }

  args <- list(color = color, alpha = alpha,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha, ...)

  if(!is.null(group)) {
    idx <- unname(split(seq_along(group), group))
    xs <- lapply(idx, function(x) xs[x])
    ys <- lapply(idx, function(x) ys[x])

    ns <- lapply(args, length)
    badInd <- which(!ns %in% c(0, 1, length(idx), length(group)))
    if(length(badInd) > 0) {
      message("The following arguments do not have length the same as the number of groups or the total number of observations for ly_polygon() and will be ignored: ", paste(names(args[badInd], collapse = ", ")))
      args[badInd] <- NULL
    }

    fullLength <- which(ns == length(group))
    for(ii in fullLength) {
      args[[ii]] <- sapply(idx, function(x) args[[i]][x[1]])
    }
  }

  # hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(xs, ys, xname, yname, list(...))
  ## translate different x, y types to vectors
  lgroup <- getLgroup(lgroup, fig)

  if(!(is.list(xs) && is.list(ys)))
    stop("For ly_polygon, xs and ys must be lists or specified through a data frame through 'data' argument.")

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  if(is.null(args$fill_alpha))
    args$fill_alpha <- 0.5

  hover <- getHover(hover)

  axisTypeRange <- getGlyphAxisTypeRange(unlist(xs), unlist(ys))
  make_glyph(fig, type = "patches", lname = lname, lgroup = lgroup, hover = hover,
    data = list(xs = unname(xs), ys = unname(ys)), args = args, axisTypeRange = axisTypeRange)
}

#' @export
ly_rect <- function(fig, xleft, ybottom, xright, ytop, data = NULL,
  color = NULL, alpha = NULL,
  fill_color = NULL, line_color = NULL, fill_alpha = 1, line_alpha = 1,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_rect")

  xname <- deparse(substitute(xleft))
  yname <- deparse(substitute(ybottom))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    xleft      <- v_eval(substitute(xleft), data)
    yright     <- v_eval(substitute(yright), data)
    ybottom    <- v_eval(substitute(ybottom), data)
    ytop       <- v_eval(substitute(ytop), data)
    color      <- v_eval(substitute(color), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_color <- v_eval(substitute(fill_color), data)
  }

  hover <- getHover(substitute(hover), data)

  xyNames <- getXYNames(xleft, ybottom, xname, yname, list(...))

  lgroup <- getLgroup(lgroup, fig)

  args <- list(color = color, alpha = alpha,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  if(is.null(args$fill_alpha))
    args$fill_alpha <- 0.5

  axisTypeRange <- getGlyphAxisTypeRange(c(xleft, xright), c(ybottom, ytop))
  make_glyph(fig, type = "quad", lname = lname, lgroup = lgroup,
    xname = xyNames$x, yname = xyNames$y,
    legend = legend, hover = hover,
    data = list(left = xleft, right = xright, top = ytop, bottom = ybottom),
    args = args, axisTypeRange = axisTypeRange)
}

#' @export
ly_crect <- function(fig, x, y = NULL, data = NULL,
  width = 1, height = 1, angle = 0, color = NULL, alpha = NULL,
  fill_color = NULL, fill_alpha = 1, line_color = NULL, line_alpha = 1,
  hover = NULL, legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_crect")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x          <- v_eval(substitute(x), data)
    y          <- v_eval(substitute(y), data)
    width      <- v_eval(substitute(width), data)
    height     <- v_eval(substitute(height), data)
    angle      <- v_eval(substitute(angle), data)
    color      <- v_eval(substitute(color), data)
    line_color <- v_eval(substitute(line_color), data)
    fill_color <- v_eval(substitute(fill_color), data)
  }

  hover <- getHover(substitute(hover), data)
  xyNames <- getXYNames(x, y, xname, yname, list(...))
  ## translate different x, y types to vectors
  xy <- getXYData(x, y)
  lgroup <- getLgroup(lgroup, fig)

  args <- list(color = color, alpha = alpha,
    width = width, height = height, angle = angle,
    fill_color = fill_color, fill_alpha = fill_alpha,
    line_color = line_color, line_alpha = line_alpha, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  if(is.null(args$fill_alpha))
    args$fill_alpha <- 0.5

  xr <- xy$x
  if(is.numeric(xy$x)) {
    xr <- c(xy$x - width / 2, xy$x + width / 2)
  }
  yr <- xy$y
  if(is.numeric(xy$y)) {
    yr <- c(xy$y - height / 2, xy$y + height / 2)
  }

  axisTypeRange <- getGlyphAxisTypeRange(xr, yr)

  make_glyph(fig, type = "rect", lname = lname, lgroup = lgroup,
    xname = xyNames$x, yname = xyNames$y,
    legend = legend, hover = hover,
    data = xy, args = args, axisTypeRange = axisTypeRange)
}

