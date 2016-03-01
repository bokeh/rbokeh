#' Add a small amount of (rbokeh-compatible) noise to a character vector
#'
#' @param x numeric vector to which jitter should be added
#' @param factor a factor between 0 and 1 that
#' @export
#' @importFrom stats runif
# add seealso and example
catjitter <- function(x, factor = 0.8) {
  # validate factor and x
  paste0(x, ":", stats::runif(min = 0.5 * (1 - factor),
    max = 0.5 + 0.5 * factor, length(x)))
}

#' Get object ids and types from a figure
#'
#' @param fig a figure object
#' @export
get_object_refs <- function(fig) {
  tmp <- lapply(fig$x$spec$model, function(x) {
    data.frame(id = x$id, type = x$type, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, unname(tmp))
  res$name <- names(tmp)
  res
}
