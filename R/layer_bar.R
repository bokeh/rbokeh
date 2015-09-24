#' Add a "barchart" layer to a Bokeh figure
#'
#' Draws a bar chart
#' @param fig figure to modify
#' @param x values or field name for x variable
#' @param y values or field name for y variable, or if NULL, y-axis will be counts of x
#' @param data an optional data frame, providing the source for inputs x, y, and color properties
#' @template par-coloralpha
#' @param position either "stack", "fill", or "dodge" (see details)
#' @param width with of each bar, a value between 0 (no width) and 1 (full width)
#' @param origin,breaks,right,binwidth parameters to be used for binning x when it is continuous (not yet implemented)
#' @template par-lnamegroup
#' @template par-legend
#' @template dots-fillline
#' @details
#' The y variable is summed for each x variable and bars are plotted.  If no y variable is supplied, the unique values of x will be tabulated.  Within each x variable, if color maps to another grouping variable then the bars are split up.  In this case, there are three ways to display the bars with the \code{position} argument.  The default, "stack" will stack the bars.  The "fill" choice will show the relative proportion for each group within each x, stacking the bars.  The "dodge" choice will plot the bars for each x side by side.
#'
#' Note that currently x cannot be numeric but support will soon be added for numeric x by first binning the x values.
#'
#' @family layer functions
#' @example man-roxygen/ex-bar.R
#' @export
ly_bar <- function(fig, x, y = NULL, data = NULL,
  color = NULL, alpha = 1,
  position = c("stack", "fill", "dodge"), width = 0.9,
  origin = NULL, breaks = NULL, right = FALSE, binwidth = NULL,
  lname = NULL, lgroup = NULL, legend = NULL, ...) {

  position <- match.arg(position)

  validate_fig(fig, "ly_bar")

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))
  colorname <- deparse(substitute(color))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    dots  <- substitute(list(...))[-1]
    args  <- lapply(dots, function(x) v_eval(x, data))
    x     <- v_eval(substitute(x), data)
    y     <- v_eval(substitute(y), data)
    color <- v_eval(substitute(color), data)
  } else {
    args <- list(...)
  }

  if(is.null(y)) {
    y <- rep(1, length(x))
    yname <- "count"
  }

  args$alpha <- alpha

  xy_names <- get_xy_names(x, y, xname, yname, args)
  ## translate different x, y types to vectors
  xy <- get_xy_data(x, y)
  lgroup <- get_lgroup(lgroup, fig)

  if(is.numeric(xy$x))
    stop("numeric values for x in ly_bar are not yet supported", call. = FALSE)

  if(is.null(color) || length(color) == 1) {
    res <- aggregate(y ~ x, data = xy, sum)
  } else {
    xy$color <- color
    color <- colorname
    res <- aggregate(y ~ x + color, data = xy, sum)
    if(missing(legend))
      legend <- TRUE
  }

  ## handle y values
  ##---------------------------------------------------------

  if(position == "stack") {
    res <- do.call(rbind, by(res, res$x, function(a) {
      a$ytop <- cumsum(a$y)
      a$ybottom <- a$ytop - a$y
      a
    }))
  }

  if(position == "fill") {
    res <- do.call(rbind, by(res, res$x, function(a) {
      tmp <- a$y / sum(a$y)
      a$ytop <- cumsum(tmp)
      a$ybottom <- a$ytop - tmp
      a
    }))
  }

  if(position == "dodge") {
    res$ytop <- res$y
    res$ybottom <- 0
  }

  ## handle x values
  ##---------------------------------------------------------

  if(position %in% c("stack", "fill")) {
    res$xleft <- paste0(res$x, ":", 1 - width)
    res$xright <- paste0(res$x, ":", width)
  } else {
    res <- do.call(rbind, by(res, res$x, function(a) {
      nn <- nrow(a)
      pts <- seq(1 - width, width, length = nn + 1)
      a$xleft <- paste0(a$x, ":", head(pts, nn))
      a$xright <- paste0(a$x, ":", tail(pts, nn))
      a
    }))
  }

  ind <- which(names(res) == "color")
  if(length(ind) > 0)
    names(res)[ind] <- colorname

  # get rid of x and y as they are no longer needed
  # and may conflict with xname, yname
  res$x <- NULL
  res$y <- NULL

  names(res)[which(names(res) == "xleft")] <- xname
  names(res)[which(names(res) == "ybottom")] <- yname

  do.call(ly_rect, c(list(fig = fig,
    xleft = xname, ybottom = yname, xright = "xright", ytop = "ytop",
    color = color, data = res,
    lname = lname, lgroup = lgroup, legend = legend), args))
}
