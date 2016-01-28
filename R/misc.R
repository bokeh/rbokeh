#' Add a small amount of (rbokeh-compatible) noise to a character vector
#'
#' @param x numeric vector to which jitter should be added
#' @param factor a factor between 0 and 1 that
#' @export
# add seealso and example
catjitter <- function(x, factor = 0.8) {
  # validate factor and x
  paste0(x, ":", runif(min = 0.5 * (1 - factor), max = 0.5 + 0.5 * factor, length(x)))
}
