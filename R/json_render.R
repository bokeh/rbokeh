#' Plot a Bokeh JSON specification
#'
#' Take a path to a Bokeh JSON plot specification file and render it in the browser.
#'
#' @note This is mainly useful for development / debugging purposes for reading in json created from another platform like Python, or to be used with tweaking json output from \code{\link{print_model_json}}.
#'
#' @param json path to json file
#' @param is_file is \code{json} a json string (FALSE) or a path to a json file (TRUE)?
#'
#' @seealso \code{\link{print_model_json}}
#' @import htmlwidgets
#' @importFrom jsonlite fromJSON
#'
#' @export
bokeh_render_json <- function(json, is_file = TRUE) {

  # extract the id
  tmp <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  docid <- names(tmp)[1]
  modelid <- tmp[[1]]$roots$root_ids[[1]]

  if (is_file)
    json <- paste(readLines(json), collapse = "\n")

  # forward options using x
  x <- list(
    docs_json = json,
    padding = list(type = "figure"),
    elementid = digest::digest(Sys.time()),
    modelid = modelid,
    docid = docid,
    isJSON = TRUE,
    debug = FALSE
  )

  # create widget
  htmlwidgets::createWidget(
    name = "rbokeh",
    x,
    width = NULL,
    height = NULL,
    package = "rbokeh"
  )
}
