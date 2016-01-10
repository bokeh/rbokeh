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

