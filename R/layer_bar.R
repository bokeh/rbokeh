#' Add a "barchart" layer to a Bokeh figure
#'
#' Draws a bar chart
#' @param fig figure to modify
#' @param x values or field name for x variable, or if NULL, x-axis will be counts of y
#' @param y values or field name for y variable, or if NULL, y-axis will be counts of x
#' @param data an optional data frame, providing the source for inputs x, y, and color properties
#' @template par-coloralpha
#' @param position either "stack", "fill", or "dodge" (see details)
#' @param width with of each bar, a value between 0 (no width) and 1 (full width)
#' @param hover logical - should a hover tool be added to show the value of each bar?
#' @param origin,breaks,right,binwidth parameters to be used for binning x when it is continuous (not yet implemented)
#' @template par-lnamegroup
#' @template par-legend
#' @template dots-fillline
#' @details
#' This function expects one of either x or y to be categorical and the other to be numeric or NULL.  The numeric variable is summed for each categorical variable and bars are plotted.  If no numeric variable is supplied, the unique values of the categorical variable will be tabulated.  Within each categorical variable, if color maps to another grouping variable then the bars are split up.  In this case, there are three ways to display the bars with the \code{position} argument.  The default, "stack" will stack the bars.  The "fill" choice will show the relative proportion for each group within each categorical variable level, stacking the bars.  The "dodge" choice will plot the bars for each level of the categorical variable side by side.
#'
#' @family layer functions
#' @example man-roxygen/ex-bar.R
#' @export
ly_bar <- function(
  fig, x = NULL, y = NULL, data = figure_data(fig),
  color = NULL, alpha = 1,
  position = c("stack", "fill", "dodge"), width = 0.9, hover = FALSE,
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

  # we'll do everything as if x is the factor
  # but if the y variable was specified as the factor we'll swap back
  swap_axes <- FALSE

  x_miss <- missing(x)
  y_miss <- missing(y)

  if (x_miss && y_miss) {
    stop("must specify at least one of 'x' or 'y' for ly_bar", call. = FALSE)
  }

  if (y_miss) {
    # when y is missing, it automatically makes y->x and makes x a sequence
    args$data$x <- args$data$y
    args$data$y <- rep(1, length(args$data$x))
    args$info$x_name <- attr(args$data$x, "stringName")
    args$info$y_name <- "count"
  } else if (x_miss) {
    args$data$x <- rep(1, length(args$data$y))
    args$info$x_name <- "count"
  }

  if (!is.numeric(args$data$x) && is.numeric(args$data$y)) {
    # nothing  to do
  } else if (is.numeric(args$data$x) && !is.numeric(args$data$y)) {
    tmp <- args$data$x
    args$data$x <- args$data$y
    args$data$y <- tmp
    swap_axes <- TRUE
  } else {
    stop("in ly_bar one of 'x' or 'y' must be numeric and the other not numeric", call. = FALSE)
  }

  if (is.null(args$params$color) || length(args$params$color) == 1) {
    res <- stats::aggregate(y ~ x, data = args$data, sum)
  } else {
    data_and_color <- args$data
    data_and_color$color <- args$params$color
    res <- stats::aggregate(y ~ x + color, data = data_and_color, sum)
    if (missing(legend)) {
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
      a$p_ <- tmp
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

  if (position %in% c("stack", "fill")) {
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
  if (length(ind) > 0) {
    names(res)[ind] <- colorname
  }

  bad_param_names <- c("color", "origin", "breaks", "right", "binwidth", "position")
  remaining_args <- args$params
  remaining_args <- remaining_args[! (names(remaining_args) %in% bad_param_names)]

  if (hover) {
    hovdat <- data.frame(
      variable = res$x,
      value = res$y
    )
    if (position == "fill") {
      hovdat$proportion <- res$p_
    }
    extra_names <- setdiff(names(res), c("x", "y", "ytop", "ybottom", "xleft", "xright"))
    if (length(extra_names) > 0) {
      hovdat <- cbind(hovdat, res[, extra_names, drop = FALSE])
    }
  } else {
    hovdat <- NULL
  }

  # get rid of x and y as they are no longer needed
  # and may conflict with xname, yname
  res$x <- NULL
  res$y <- NULL
  res$p_ <- NULL

  remaining_args$width <- NULL

  # b_eval doesn't know how to deal with quoted inputs with a single-row data frame
  # so instead tag the data so it knows the input is quoted
  if (nrow(res) == 1)
    class(res) <- c(class(res), "quoted")

  color_value <- if (is.null(colorname)) args$params$color else colorname

  if (swap_axes) {
    ind <- match(c("ytop", "ybottom", "xright", "xleft"), names(res))
    names(res)[ind] <- c("xright", "xleft", "ybottom", "ytop")
  }

  do.call(ly_rect, append(list(fig = fig,
    xleft = "xleft", ybottom = "ybottom", xright = "xright", ytop = "ytop",
    xlab = args$info$x_name, ylab = args$info$y_name,
    data = res, hover = hovdat,
    color = color_value,
    lname = args$info$lname, lgroup = args$info$lgroup, legend = args$info$legend),
    remaining_args), quote = TRUE) # quote = TRUE needed for lazy_
}
