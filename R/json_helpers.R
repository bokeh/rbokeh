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
  cat(toJSON(fig$x$all_models, pretty = pretty,
    auto_unbox = TRUE, null = "null", na = "null"), "\n", file = file)
}

#' Get the HTML content required to embed a Bokeh figure
#' @param fig figure
#' @export
get_bokeh_html <- function(fig) {
  all_models <- fig$x$spec$model
  elementid <- digest(Sys.time())
  modelid <- fig$x$spec$model$plot$id
  type <- fig$x$modeltype

  fig <- rbokeh_prerender(fig)
  fig <- toJSON(fig$x$all_models)

  a <- paste(
  '<head>\n',
  '<script src="http://cdn.pydata.org/bokeh/release/bokeh-0.8.1.min.js"></script>\n',
  '<link href="http://cdn.pydata.org/bokeh/release/bokeh-0.8.1.min.css" rel="stylesheet">\n',
  '</head>\n\n',
  '<div id=', elementid, ' class="plotdiv"></div>\n\n',
  '<script type="text/javascript">\n',
  'Bokeh.$(function() {\n',
  'var modelid = "', modelid, '";\n',
  'var modeltype = "', type,'";\n',
  'var elementid = "', elementid,'";\n',
  'Bokeh.logger.info("Realizing plot:");\n',
  'Bokeh.logger.info(" - modeltype: ', type, '");\n',
  'Bokeh.logger.info(" - modelid: ', modelid, '");\n',
  'Bokeh.logger.info(" - elementid: ', elementid, '");\n',
  'var all_models = ', fig, ';\n',
  'Bokeh.load_models(all_models);\n',
  'var model = Bokeh.Collections(modeltype).get(modelid);\n',
  'var view = new model.default_view({model: model, el: \'#', elementid, '\'});\n',
  'Bokeh.index[modelid] = view;\n',
  '});\n',
  '</script>\n',
  sep = "")
}

base_model_object <- function(type, id) {
  list(
    model = list(
      type = type,
      id = id,
      attributes = list(
        id = id,
        tags = list(),
        doc = NULL
      )
    ),
    ref = list(
      type = type,
      id = id
    )
  )
}

#' @importFrom digest digest
gen_id <- function(obj, name = NULL) {
  digest(c(name, obj$x$spec$time))
}

remove_model_names <- function(obj) {
  names(obj$plot$attributes$tools) <- NULL
  names(obj$plot$attributes$renderers) <- NULL
  names(obj) <- NULL
  obj
}

underscore2camel <- function(x) {
  x <- gsub("^([a-zA-Z])", "\\U\\1", x, perl = TRUE)
  gsub("_([a-zA-Z])", "\\U\\1", x, perl = TRUE)
}
