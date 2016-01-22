#' Print the JSON of a Bokeh figure
#' @param fig figure to print
#' @param prepare logical - should the figure be sent through preparations that need to be done prior to plotting (TRUE), or printed as-is (FALSE)
#' @param pretty parameter passed on to \code{\link[jsonlite]{toJSON}}
#' @param file parameter passed on to \code{\link[base]{cat}}
#' @param pbcopy logical - if on OSX, should the results be passed to the clipboard (TRUE) instead of printed to the screen (FALSE)?
#' @examples
#' \donttest{
#' p <- figure() %>% ly_points(1:10) %>%
#'  tool_pan(dimensions = "height")
#' print_model_json(p)
#' }
#' @importFrom jsonlite toJSON
#' @export
print_model_json <- function(fig, prepare = TRUE, pretty = TRUE, file = "", pbcopy = FALSE) {
  if(prepare) {
    fig <- rbokeh_prerender(fig)
  }

  if(pbcopy)
    file <- pipe("pbcopy")
  cat(toJSON(fig$x$docs_json, pretty = pretty,
    auto_unbox = TRUE, null = "null", na = "null"), "\n", file = file)
}
