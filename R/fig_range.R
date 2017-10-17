#' Update x and y axis range in a Bokeh figure
#' @param fig Figure to modify.
#' @param lims Either a vector (min, max) if the range is numeric, or a vector of values if the range is categorical.  In the latter case, the order in which the values are supplied is how they will be arranged on the axis.
#' @param min_interval The level that the range is allowed to zoom in. For numeric ranges, it is expressed as the minimum visible interval. For categorical ranges, it is expressed as the minimum number of visible categories. If set to \code{NULL} (default), the minimum interval is not bound.
#' @param max_interval The level that the range is allowed to zoom out. For numeric ranges, it is expressed as the maximum visible interval. For categorical ranges, it is expressed as the maximum number of visible categories. Note that \code{bounds} can impose an implicit constraint on the maximum interval as well.
#' @param bounds The bounds that the range is allowed to go to - typically used to prevent the user from panning/zooming/etc away from the data. By default, the bounds are \code{NULL}, allowing unlimited panning or zooming. For numeric ranges, if you only want to constrain one end of the plot, you can set one end to NA, e.g. c(NA, 12). For categorical ranges, if \code{bounds='auto'}, bounds will be the same as factors and the plot will not be able to pan or zoom beyond the first and last factors. If you provide a vector of factor levels, then only the values that are in that list will be displayed on the plot and the plot will not pan or zoom outside the first and last items in the shortened factors list. Note the order of factors is the defining order for your plot. Values of bounds that are not in factors are acceptable and will simply have no impact on the plot.
#' @param flipped For numeric ranges only. Whether the range should be "flipped" from its normal direction when auto-ranging (boolean).
#' @param range_padding For numeric ranges only. How much padding to add around the computed data bounds. When \code{range_padding_units} is set to "percent", the span of the range span is expanded to make the range \code{range_padding} percent larger. When \code{range_padding_units} is set to "absolute", the start and end of the range span are extended by the amount \code{range_padding}.
#' @param range_padding_units For numeric ranges only. Whether the \code{range_padding} should be interpreted as a percentage, or as an absolute quantity. One of "percent" or "absolute".
#' @param default_span For numeric ranges only. A default width for the interval, in case \code{start} is equal to \code{end} (if used with a log axis, default_span is in powers of 10).
#' @param follow For numeric ranges only. Configure the data to follow one or the other data extreme, with a maximum range size of \code{follow_interval}. If set to \code{"start"} then the range will adjust so that \code{start} always corresponds to the minimum data value (or maximum, if \code{flipped} is \code{True}). If set to \code{"end"} then the range will adjust so that \code{end} always corresponds to the maximum data value (or minimum, if \code{flipped} is \code{True}). If set to \code{NULL} (default), then auto-ranging does not follow, and the range will encompass both the minimum and maximum data values. \code{follow} cannot be used with bounds, and if set, bounds will be set to \code{NULL}.
#' @param follow_interval For numeric ranges only. If \code{follow} is set to \code{"start"} or \code{"end"} then the range will always be constrained to that.
#' @param callback A string JavaScript code to be run any time the axis range is updated.
#' @param clear A logical indicating whether all non-specified parameters should be cleared out of any exising specification in the figure. This can be useful when modifying a previously-defined figure.
#' @example man-roxygen/ex-range.R
#' @name range
NULL

#' @rdname range
#' @export
x_range <- function(fig,
  lims = NULL,
  min_interval = NULL,
  max_interval = NULL,
  bounds = NULL,
  flipped = FALSE,
  range_padding = 0.07,
  range_padding_units = "percent",
  default_span = NULL,
  follow = NULL,
  follow_interval = NULL,
  callback = NULL,
  clear = FALSE
) {
  args <- get_specified_args(c("lims", "min_interval", "max_interval", "bounds",
    "flipped", "range_padding", "range_padding_units", "default_span", "follow",
    "follow_interval", "callback"))

  update_range(fig, args, clear, "x")
}

#' @rdname range
#' @export
y_range <- function(fig,
  lims = NULL,
  min_interval = NULL,
  max_interval = NULL,
  bounds = NULL,
  flipped = FALSE,
  range_padding = 0.07,
  range_padding_units = "percent",
  default_span = NULL,
  follow = NULL,
  follow_interval = NULL,
  callback = NULL,
  clear = FALSE
) {
  args <- get_specified_args(c("lims", "min_interval", "max_interval", "bounds",
    "flipped", "range_padding", "range_padding_units", "default_span", "follow",
    "follow_interval", "callback"))

  update_range(fig, args, clear, "y")
}

update_range <- function(fig, args, clear, which) {

  if ("lims" %in% names(args)) {
    lims <- args$lims
    args$lims <- NULL
    if (is.numeric(lims) || inherits(lims, c("Date", "POSIXct"))) {
      if (length(lims) != 2)
        stop("'lims' argument for numeric range must be a vector of length 2.")
      fig$x$pars$ranges[[which]]$lims_spec <- lims
    } else if (is.character(lims)) {
      fig$x$pars$ranges[[which]]$lims_spec <- lims
    } else if (is.factor(lims)) {
      fig$x$pars$ranges[[which]]$lims_spec <- levels(lims)
    }
  }

  # since we allow range_padding to be specified with figure(), we track it separately
  # and add it in when the range model is created
  if ("range_padding" %in% names(args)) {
    fig$x$pars$gen$range_padding[[which]] <- args$range_padding
    args$range_padding <- NULL
  }

  if (clear) {
    fig$x$pars$ranges[[which]]$args <- args
  } else {
    fig$x$pars$ranges[[which]]$args <-
      utils::modifyList(fig$x$pars$ranges[[which]]$args, args)
  }

  fig
}
