#' @importFrom stats aggregate complete.cases is.ts ppoints quantile qunif runif time
#' @importFrom grDevices rgb2hsv hsv boxplot.stats col2rgb contourLines hsv rgb2hsv
#' @importFrom pryr named_dots
#' @importFrom utils head tail
NULL


#' rbokeh: R interface for Bokeh
#'
#' R interface for creating plots in Bokeh.  Bokeh by Continuum Analytics, \url{http://bokeh.pydata.org/en/latest/}
#'
#' For full documentation on the package, visit \url{http://hafen.github.io/rbokeh}
#' @name rbokeh-package
#' @aliases rbokeh
#' @docType package
NULL

#' "Periodic Table" dataset
#'
#' @name elements
#' @docType data
#' @description
#' Data for periodic table of the elements
#' @usage elements
#' @keywords data
#' @example man-roxygen/ex-elements.R
NULL

#' Flight frequency dataset
#'
#' @name flightfreq
#' @docType data
#' @description
#' Daily counts of domestic flights in the U.S. from 1999 to mid-2008
#' @usage flightfreq
#' @keywords data
#' @example man-roxygen/ex-flightfreq.R
NULL


#' Hexagon binned counts of NYC taxi pickup locations
#'
#' @name nyctaxihex
#' @docType data
#' @description
#' Counts of NYC taxi pickups by location for January 2013, obtained from \href{http://chriswhong.com/open-data/foil_nyc_taxi/}{here}.
#' @usage nyctaxihex
#' @keywords data
#' @example man-roxygen/ex-gmap.R
NULL

#' Pipe figures
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs a Bokeh figure
#' @param rhs a layer to add to the figure
NULL
