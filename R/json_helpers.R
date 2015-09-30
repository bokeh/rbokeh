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
    auto_unbox = TRUE, null = "null", na = "null"), file = file)
}

#' Get the HTML content required to embed a Bokeh figure
#' @param fig figure
#' @export
rbokeh2html <- function(fig) {
  all_models <- fig$x$spec$model
  elementid <- digest(Sys.time())
  modelid <- fig$x$spec$model$plot$id
  type <- fig$x$modeltype

  fig <- rbokeh_prerender(fig)
  fig <- toJSON(fig$x$all_models, pretty = pretty,
    auto_unbox = TRUE, null = "null", na = "null")

  ver <- get_bokeh_version()

  a <- paste0(
  '<!DOCTYPE html>\n',
  '<html>\n',
  '<head>\n',
  '<script src="http://cdn.pydata.org/bokeh/release/bokeh-', ver, '.min.js"></script>\n',
  '<link href="http://cdn.pydata.org/bokeh/release/bokeh-', ver, '.min.css" rel="stylesheet">\n',
  '</head>\n\n',
  '<body>\n',
  '<div id="', elementid, '" class="plotdiv"></div>\n\n',
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
  '</body>\n',
  '</html>')
}

get_bokeh_version <- function() {
  # assumes there is only one listed dependency here
  # (don't want dependency on yaml package just for this)
  yaml <- readLines(file.path(system.file(package = "rbokeh"), "htmlwidgets", "rbokeh.yaml"))
  yaml <- yaml[grepl("version:", yaml)]
  gsub(" +version: +(.*)", "\\1", yaml)
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

# bokeh2codepen <- function(fig, title = "rbokeh plot", description = "rbokeh plot", private = FALSE, tags = c("bokeh", "rbokeh")) {

#   all_models <- fig$x$spec$model
#   elementid <- digest(Sys.time())
#   modelid <- fig$x$spec$model$plot$id
#   type <- fig$x$modeltype

#   fig <- rbokeh_prerender(fig)
#   fig <- toJSON(fig$x$all_models, pretty = pretty,
#     auto_unbox = TRUE, null = "null", na = "null")

#   ver <- get_bokeh_version()

#   data <- list(
#     title = title,
#     description = description,
#     private = private,
#     tags = I(tags),
#     html = paste0('<div id="', elementid, '" class="plotdiv"></div>'),
#     js = paste0('Bokeh.$(function() {\n',
#       'var modelid = "', modelid, '";\n',
#       'var modeltype = "', type,'";\n',
#       'var elementid = "', elementid,'";\n',
#       'Bokeh.logger.info("Realizing plot:");\n',
#       'Bokeh.logger.info(" - modeltype: ', type, '");\n',
#       'Bokeh.logger.info(" - modelid: ', modelid, '");\n',
#       'Bokeh.logger.info(" - elementid: ', elementid, '");\n',
#       'var all_models = ', fig, ';\n',
#       'Bokeh.load_models(all_models);\n',
#       'var model = Bokeh.Collections(modeltype).get(modelid);\n',
#       'var view = new model.default_view({model: model, el: \'#', elementid, '\'});\n',
#       'Bokeh.index[modelid] = view;\n'),
#     css_external = paste0("http://cdn.pydata.org/bokeh/release/bokeh-", ver, ".min.css"),
#     js_external = paste0("http://cdn.pydata.org/bokeh/release/bokeh-", ver, ".min.js")
#   )
#   library(httr)
#   a <- POST("http://codepen.io/pen/define/", body = list(data = data), encode = "json")
# }

