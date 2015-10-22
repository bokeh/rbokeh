#' Add a "barchart" layer to a Bokeh figure
#'
#' Draws a bar chart
#' @param fig figure to modify
#' @param x values or field name for x variable
#' @param y values or field name for y variable
#' @param data an optional data frame, providing the source for inputs x, y, and color properties
#' @template par-coloralpha
#' @param position either "stack", "fill", or "dodge" (see details)
#' @param width with of each bar, a value between 0 (no width) and 1 (full width)
#' @param origin,breaks,right,binwidth parameters to be used for binning x when it is continuous (not yet implemented)
#' @template par-lnamegroup
#' @template par-legend
#' @template dots-fillline
#' @details
#' The y variable is summed for each x variable and bars are plotted.  Within each x variable, if color maps to another grouping variable then the bars are split up.  In this case, there are three ways to display the bars with the \code{position} argument.  The default, "stack" will stack the bars.  The "fill" choice will show the relative proportion for each group within each x, stacking the bars.  The "dodge" choice will plot the bars for each x side by side.
#'
#' Note that currently x cannot be numeric but support will soon be added for numeric x by first binning the x values.
#'
#' @family layer functions
#' @export
ly_bar <- function(
  fig, x, y, data = NULL,
  color = NULL, alpha = 1,
  position = c("stack", "fill", "dodge"), width = 0.9,
  origin = NULL, breaks = NULL, right = FALSE, binwidth = NULL,
  lname = NULL, lgroup = NULL, legend = NULL, ...
) {

  position <- match.arg(position)

  validate_fig(fig, "ly_bar")

  args <- sub_names2(fig, data,
    grab2(
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
      dots = lazy_dots(...)
    )
  )

  if (is.null(color)) {
    colorname <- NULL
  } else {
    colorname <- deparse(substitute(color))
  }

  if(is.numeric(args$data$x)) {
    stop("numeric values for x in ly_bar are not yet supported", call. = FALSE)
  }

  if(is.null(args$params$color)) {
    res <- aggregate(y ~ x, data = args$data, sum)
  } else {
    dataAndColor <- args$data
    dataAndColor$color <- args$params$color
    res <- aggregate(y ~ x + color, data = dataAndColor, sum)
    if(missing(legend)) {
      args$info$legend <- TRUE
    }
  }

  ## handle y values
  ##---------------------------------------------------------

  if (position == "stack") {
    res <- do.call(rbind, by(res, res$x, function(a) {
      a$ytop <- cumsum(a$y)
      a$ybottom <- a$ytop - a$y
      a
    }))

  } else if (position == "fill") {
    res <- do.call(rbind, by(res, res$x, function(a) {
      tmp <- a$y / sum(a$y)
      a$ytop <- cumsum(tmp)
      a$ybottom <- a$ytop - tmp
      a
    }))

  } else if (position == "dodge") {
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
  if(length(ind) > 0) {
    names(res)[ind] <- colorname
  }

  names(res)[which(names(res) == "xleft")] <- args$info$xName
  names(res)[which(names(res) == "ybottom")] <- args$info$yName

  badParamNames = c("color", "origin","breaks","right","binwidth", "position")
  remainingArgs = args$params
  remainingArgs = remainingArgs[! (names(remainingArgs) %in% badParamNames)]

  do.call(ly_rect, c(list(fig = fig,
    xleft = args$info$xName, ybottom = args$info$yName, xright = "xright", ytop = "ytop",
    color = colorname, data = res,
    lname = args$info$lname, lgroup = args$info$lgroup, legend = args$info$legend), remainingArgs))
}
