
# args should be a named list of refs to other things that code will reference
customjs_model <- function(type = "CustomJS", id, code, args) {
  res <- base_model_object(type, id)
  res$model$attributes$code <- code
  res$model$attributes$args <- args
  res
}

# translate a vector of lnames to a list of refs
# that will be made available inside custom callback
callback_lname2args <- function(lnames, fig_refs) {
  fig_refs <- fig_refs[lnames]
  unlist(unname(fig_refs), recursive = FALSE)
}

#' @export
shiny_callback <- function(id, lnames = NULL) {
  structure(list(id = id, lnames = lnames, args = NULL), class = "shinyCallback")
}

#' @export
console_callback <- function(lnames = NULL) {
  structure(list(code = "", lnames = lnames, args = NULL), class = "consoleCallback")
}

#' @export
custom_callback <- function(code, lnames = NULL) {
  # TODO: checking that code and args are correct
  # code should be string
  structure(list(code = code, lnames = lnames, args = NULL), class = "customCallback")
}

#' @export
debug_callback <- function(lnames = NULL) {
  structure(list(code = "debugger", lnames = lnames, args = NULL), class = "debugCallback")
}

## s3 methods
##---------------------------------------------------------

handle_range_callback <- function(x, args)
  UseMethod("handle_range_callback", x)

handle_tap_callback <- function(x, args)
  UseMethod("handle_tap_callback", x)

handle_hover_callback <- function(x, args)
  UseMethod("handle_hover_callback", x)

handle_selection_callback <- function(x, args)
  UseMethod("handle_selection_callback", x)

## console_callback
##---------------------------------------------------------

handle_range_callback.consoleCallback <- function(x, fig_refs) {
  list(
    code = "
if(range.get('factors')) {
  console.log(range.get('factors'))
} else if(range.get('start')) {
  console.log('[' + range.get('start').toFixed(2) + ',' + range.get('end').toFixed(2) + ']')
}",
    args = c(x$args, callback_lname2args(x$lnames, fig_refs))
  )
}

handle_tap_callback.consoleCallback <- function(x, fig_refs) {
  list(
    code = "
console.log('cb_data:')
console.log(cb_data)
console.log('cb_obj:')
console.log(cb_obj)",
    args = c(x$args, callback_lname2args(x$lnames, fig_refs))
  )
}

handle_hover_callback.consoleCallback <- function(x, fig_refs) {
  list(
    code = "
if(cb_data.index['1d'].indices.length > 0) {
  console.log('cb_data:')
  console.log(cb_data)
  console.log('cb_obj:')
  console.log(cb_obj)
}",
    args = c(x$args, callback_lname2args(x$lnames, fig_refs))
  )
}

## custom callback
##---------------------------------------------------------

handle_range_callback.customCallback <- function(x, fig_refs)
  handle_custom_callback(x, fig_refs)

handle_hover_callback.customCallback <- function(x, fig_refs)
  handle_custom_callback(x, fig_refs)

handle_tap_callback.customCallback <- function(x, fig_refs)
  handle_custom_callback(x, fig_refs)

handle_selection_callback.customCallback <- function(x, fig_refs)
  handle_custom_callback(x, fig_refs)

handle_custom_callback <- function(x, fig_refs) {
  x$args <- c(x$args, callback_lname2args(x$lnames, fig_refs))
  x
}

## debug callback
##---------------------------------------------------------

handle_range_callback.debugCallback <- function(x, fig_refs)
  handle_debug_callback(x, fig_refs)

handle_hover_callback.debugCallback <- function(x, fig_refs) {
  x$args <- c(x$args, callback_lname2args(x$lnames, fig_refs))
  x$code <- "
if(cb_data.index['1d'].indices.length > 0) {
  debugger;
}"
  x
}

handle_tap_callback.debugCallback <- function(x, fig_refs)
  handle_debug_callback(x, fig_refs)

handle_selection_callback.debugCallback <- function(x, fig_refs) {
  x$args <- c(x$args, callback_lname2args(x$lnames, fig_refs))
  x$code <- "
if(cb_obj.get('selected')['1d'].indices.length > 0) {
  debugger;
}"
  x
}

handle_debug_callback <- function(x, fig_refs) {
  x$args <- c(x$args, callback_lname2args(x$lnames, fig_refs))
  x
}


## character callback
##---------------------------------------------------------

handle_range_callback.character <- function(x, fig_refs)
  handle_character_callback(x, fig_refs)

handle_hover_callback.character <- function(x, fig_refs)
  handle_character_callback(x, fig_refs)

handle_tap_callback.character <- function(x, fig_refs)
  handle_character_callback(x, fig_refs)

handle_selection_callback.character <- function(x, fig_refs)
  handle_character_callback(x, fig_refs)

handle_character_callback <- function(x, fig_refs) {
  list(
    code = x,
    args = list()
  )
}

## shiny callback
##---------------------------------------------------------

handle_range_callback.shinyCallback <- function(x, fig_refs) {
  list(
    code = sprintf("
if (HTMLWidgets.shinyMode) {
  var dat = {factors: range.get('factors'), start: range.get('start'), end: range.get('end')}
  Shiny.onInputChange('%s', dat);
}
", as.character(x$id)),
    args = c(x$args, callback_lname2args(x$lnames, fig_refs))
  )
}

handle_tap_callback.shinyCallback <- function(x, fig_refs) {
  list(
    code = sprintf("
if (HTMLWidgets.shinyMode) {
  var cols = cb_obj.attributes.column_names;
  var idx = cb_obj.attributes.selected['1d'].indices;
  var res = null;
  if(idx.length > 0) {
    res = {}
    for(var i = 0; i < cols.length; i++) {
      res[cols[i]] = [];
      for (var j = 0; j < idx.length; j++) {
        res[cols[i]].push(cb_obj.attributes.data[cols[i]][idx[j]]);
      }
    }
  }
  Shiny.onInputChange('%s', res);
}
", as.character(x$id)),
    args = c(x$args, callback_lname2args(x$lnames, fig_refs))
  )
}

handle_hover_callback.shinyCallback <- function(x, fig_refs) {
  list(
    code = sprintf("
if (HTMLWidgets.shinyMode) {
  var dat = {index: cb_data.index, geom: cb_data.geometry}
  Shiny.onInputChange('%s', dat);
}
", as.character(x$id)),
    args = c(x$args, callback_lname2args(x$lnames, fig_refs))
  )
}

handle_selection_callback.shinyCallback <- function(x, fig_refs) {
  list(
    code = sprintf("
Shiny.onInputChange('%s', cb_obj.get('selected')['1d'].indices);
", as.character(x$id)),
    args = c(x$args, callback_lname2args(x$lnames, fig_refs))
  )
}

## default methods
##---------------------------------------------------------

handle_tap_callback.default <- function(x, fig_refs) {
  message("url callback not recognized - ignoring")
}

handle_hover_callback.default <- function(x, fig_refs) {
  message("hover callback not recognized - ignoring")
}

handle_range_callback.default <- function(x, fig_refs) {
  message("range callback not recognized - ignoring")
}

handle_selection_callback.default <- function(x, fig_refs) {
  message("selection callback not recognized - ignoring")
}

# figure() %>% ly_points(1:10) %>%
#   x_range(callback = console_callback())

# figure() %>% ly_points(1:10) %>%
#   x_range(callback = "console.log(range.get('start'))")


# var cols = cb_obj.attributes.column_names;
# var idx = cb_obj.attributes.selected['1d'].indices;
# var res = {}
# for(var i = 0; i < cols.length; i++) {
#   res[cols[i]] = [];
#   for (var j = 0; j < idx.length; j++) {
#     res[cols[i]].push(cb_obj.attributes.data[cols[i]][idx[j]]);
#   }
# }
# console.log(res)
