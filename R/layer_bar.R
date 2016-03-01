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
ly_bar <- function(
  fig, x, y = NULL, data = figure_data(fig),
  color = NULL, alpha = 1,
  position = c("stack", "fill", "dodge"), width = 0.9,
  origin = NULL, breaks = NULL, right = FALSE, binwidth = NULL,
  lname = NULL, lgroup = NULL, legend = NULL, visible = TRUE, ...
) {

  position <- match.arg(position)

  validate_fig(fig, "ly_bar")

  args <- sub_names(fig, data,
    grab(
      x,
      y,
      color,
      alpha,
      position,
      width,
      origin,
      breaks,
      right,
      binwidth,
      # hover, # no hover
      # url, # no url
      legend,
      lname,
      lgroup,
      visible,
      dots = lazy_dots(...)
    )
  )

  # will give NULL if it was not a variable
  colorname <- attr(args$params$color, "stringName")

  if(missing(y)) {
    args$data$x <- args$data$y
    args$data$y <- rep(1, length(args$data$x))
    args$info$x_name <- attr(args$data$x, "stringName")
    args$info$y_name <- "count"
  }

  if(is.numeric(args$data$x)) {
    stop("numeric values for x in ly_bar are not yet supported", call. = FALSE)
  }

  if(is.null(args$params$color) || length(args$params$color) == 1) {
    res <- stats::aggregate(y ~ x, data = args$data, sum)
  } else {
    data_and_color <- args$data
    data_and_color$color <- args$params$color
    res <- stats::aggregate(y ~ x + color, data = data_and_color, sum)
    if(missing(legend)) {
      args$info$legend <- TRUE
    }
  }

  ## handle y values
  ##---------------------------------------------------------

  if(position == "stack") {
    res <- do.call(rbind, by(res, res$x, function(a) {
      a$ytop <- cumsum(a$y)
      a$ybottom <- a$ytop - a$y
      a
    }))

  } else if(position == "fill") {
    res <- do.call(rbind, by(res, res$x, function(a) {
      tmp <- a$y / sum(a$y)
      a$ytop <- cumsum(tmp)
      a$ybottom <- a$ytop - tmp
      a
    }))

  } else if(position == "dodge") {
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
      a$xleft <- paste0(a$x, ":", utils::head(pts, nn))
      a$xright <- paste0(a$x, ":", utils::tail(pts, nn))
      a
    }))
  }

  ind <- which(names(res) == "color")
  if(length(ind) > 0) {
    names(res)[ind] <- colorname
  }

  bad_param_names = c("color", "origin","breaks","right","binwidth", "position")
  remaining_args = args$params
  remaining_args = remaining_args[! (names(remaining_args) %in% bad_param_names)]

  # get rid of x and y as they are no longer needed
  # and may conflict with xname, yname
  res$x <- NULL
  res$y <- NULL

  remaining_args$width <- NULL

  color_value <- if(is.null(colorname)) args$params$color else colorname
  do.call(ly_rect, append(list(fig = fig,
    xleft = "xleft", ybottom = "ybottom", xright = "xright", ytop = "ytop",
    xlab = args$info$x_name, ylab = args$info$y_name,
    data = res,
    color = color_value,
    lname = args$info$lname, lgroup = args$info$lgroup, legend = args$info$legend), remaining_args))
}
