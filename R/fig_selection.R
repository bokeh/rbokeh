
#' Add "selection" tool callback to a Bokeh figure
#'
#' This adds a selection callback to be used with the box select or lasso select tools.
#' @param fig figure to modify
#' @template callback
#' @export
tool_selection <- function(fig, callback, ref_layer) {

  nm <- paste(ref_layer, "data", sep = "_")
  did <- fig$x$spec$callback$layers[[ref_layer]][[nm]]$id

  callback <- handle_selection_callback(callback, fig$x$spec$callback$layers)

  cb_id <- gen_id(fig, c(did, "SelectionCallback",
    callback$args, callback$lnames))

  cb_model <- customjs_model(id = cb_id,
    code = callback$code, args = callback$args)

  fig$x$spec$model[[did]]$attributes$callback <- cb_model$ref
  fig$x$spec$model[[cb_id]] <- cb_model$model

  fig
}
