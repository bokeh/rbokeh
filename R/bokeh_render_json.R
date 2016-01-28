#' Plot a Bokeh JSON specification
#'
#' Take a path to a Bokeh JSON plot specification file and render it in the browser.
#'
#' @note This is mainly useful for development / debugging purposes for reading in json created from another platform like Python, or to be used with tweaking json output from \code{\link{print_model_json}}.
#'
#' @param json_file path to json file
#'
#' @seealso \code{\link{print_model_json}}
#' @import htmlwidgets
#' @importFrom jsonlite fromJSON
#'
#' @export
bokeh_render_json <- function(json_file) {

  # extract the id
  tmp <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
  docid <- names(tmp)[1]
  modelid <- tmp[[1]]$roots$root_ids[[1]]

  # forward options using x
  x <- list(
    docs_json = paste(readLines(json_file), collapse = "\n"),
    padding = list(type = "figure", y_pad = 0, x_pad = 0),
    elementid = digest::digest(Sys.time()),
    modelid = modelid,
    docid = docid,
    isJSON = TRUE
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'rbokeh',
    x,
    width = NULL,
    height = NULL,
    package = 'rbokeh'
  )
}
