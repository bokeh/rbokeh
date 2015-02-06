#' @param \ldots additional parameters for fine control over line properties (see "Additional parameters" below)
#' @section Additional parameters:
#' \tabular{ll}{
#'   \code{line_join} \tab how path segments should be joined together 'miter' 'round' 'bevel' \cr
#'   \code{line_cap} \tab how path segments should be terminated 'butt' 'round' 'square' \cr
#'   \code{line_dash} \tab an integer between 1 and 6 matching the \code{lty} property in \code{\link[graphics]{par}} or an array of integer pixel distances that describe the on-off pattern of dashing to use \cr
#'   \code{line_dash_offset} \tab the distance in pixels into the line_dash that the pattern should start from
#'  }
