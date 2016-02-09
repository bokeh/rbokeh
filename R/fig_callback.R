
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

#' Specify a Shiny callback
#'
#' @param id a name that will be made available in your Shiny app as \code{input$id}
#' @note Depending on the type of callback you are using (selection, range, hover, tap), the value of \code{input$id} will change.  The best way to get familiar with what to expect as these values is to debug inside your Shiny app and inspect the contents.  You can also use \code{\link{custom_callback}} to write your own custom callbacks that can register other data in your Shiny app.  To see what the callbacks look like for each callback type, see, for example, the contents of \code{rbokeh:::handle_range_callback.shinyCallback}
#' @export
shiny_callback <- function(id) {
  structure(list(id = id, lnames = NULL, args = NULL), class = "shinyCallback")
}

#' Specify a console callback
#'
#' This registers a callback that simply prints the callback objects in the javascript console of your web browser.  A probalby more useful callback is the \code{\link{debug_callback}} which will place you inside a debugger in your web browser allowing you to inspect the callback objects.
#' @examples
#' \donttest{
#' figure() %>%
#'   ly_points(1:10) %>%
#'   x_range(callback = console_callback()) %>%
#'   y_range(callback = console_callback())
#' }
#' @export
console_callback <- function() {
  structure(list(code = "", lnames = NULL, args = NULL), class = "consoleCallback")
}

#' Specify a custom callback
#'
#' This registers a callback that allows you to specify your own custom callback javascript code.  A probalby more useful callback to use in conjunction with this for working on the javascript code is the \code{\link{debug_callback}} which will place you inside a debugger in your web browser allowing you to inspect the callback objects.
#' @param code a string of javascript callback code
#' @param lnames vector of layer names to be made available inside the callback in addition to the default callback objects (see details)
#' @param args named list of additional references to objects to be addressable in the callback
#' @details If we add a layer and provide it, for example the \code{lname} "points", then if we refer to it using the \code{lnames} parameter to the callback, several objects will be made available inside the callback for you to access, given the names "points_data", "points_glyph", "points_glyph_rend", "points_hov_glyph", "points_ns_glyph", all pointers to different objects associated with the "points" layer that your callback can manipulate.
#' @example man-roxygen/ex-hover-custom-callback.R
#' @export
custom_callback <- function(code, lnames = NULL, args = NULL) {
  # TODO: checking that code and args are correct
  # code should be string
  structure(list(code = code, lnames = lnames, args = args), class = "customCallback")
}

#' Specify a "debug" callback
#'
#' This registers a callback that simply places you inside a debugger in your web browser allowing you to inspect the callback objects.
#' @param lnames vector of layer names to be made available inside the callback in addition to the default callback objects (see \code{\link{custom_callback}} for details)
#' @param args named list of additional references to objects to be addressable in the callback
#' @example man-roxygen/ex-tap-debug-callback.R
#' @export
debug_callback <- function(lnames = NULL, args = NULL) {
  structure(list(code = "debugger", lnames = lnames, args = args), class = "debugCallback")
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

handle_selection_callback.consoleCallback <- function(x, fig_refs) {
  list(
    code = "
if(cb_obj.get('selected')['1d'].indices.length > 0) {
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
  // var cols = cb_obj.attributes.column_names;
  // var idx = cb_obj.attributes.selected['1d'].indices;
  // var res = null;
  // if(idx.length > 0) {
  //   res = {}
  //   for(var i = 0; i < cols.length; i++) {
  //     res[cols[i]] = [];
  //     for (var j = 0; j < idx.length; j++) {
  //       res[cols[i]].push(cb_obj.attributes.data[cols[i]][idx[j]]);
  //     }
  //   }
  // }
  Shiny.onInputChange('%s', cb_obj.get('selected')['1d'].indices);
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
