#' @param \ldots Additional parameters for fine control over line properties. See "Additional parameters" below.
#' @section Additional parameters:
#' \tabular{ll}{
#'   \code{line_join} \tab How path segments should be joined together. One of 'miter' 'round' 'bevel'. \cr
#'   \code{line_cap} \tab How path segments should be terminated. One of 'butt' 'round' 'square'. \cr
#'   \code{line_dash} \tab An integer between 1 and 6 matching the \code{lty} property in \code{\link[graphics]{par}} or an array of integer pixel distances that describe the on-off pattern of dashing to use. \cr
#'   \code{line_dash_offset} \tab The distance in pixels into the line_dash that the pattern should start from.
#'  }
