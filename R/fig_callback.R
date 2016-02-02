
# args should be a named list of refs to other things that code will reference
customjs_model <- function(type = "CustomJS", id, code, args) {
  res <- base_model_object(type, id)
  res$model$attributes$code <- code
  res$model$attributes$args <- args
  res
}



#' @export
shiny_callback <- function(id) {
  class(id) <- "shinyCallback"
  id
}

#' @export
console_callback <- function() {
  res <- 1
  class(res) <- "consoleCallback"
  res
}

#' @export
custom_callback <- function(code, args) {
  # TODO: checking that code and args are correct
  # code should be string
  # args should be named list of refs to parts of the model code accesses
  structure(list(code = code, args = args), class = "customCallback")
}





## range callbacks
##---------------------------------------------------------

handle_range_callback <- function(x, model)
  UseMethod("handle_range_callback", x)

handle_range_callback.character <- function(x, model) {
  list(
    code = x,
    args = list(range = model$ref)
  )
}

handle_range_callback.consoleCallback <- function(x, model) {
  list(
    code = "
console.log('factors: ' + range.get('factors') + ', start: ' + range.get('start') + ', end: ' + range.get('end'))
    ",
    args = list(range = model$ref)
  )
}
# if(range.get('factors')) {
#   console.log(range.get('factors'))
# } else if(range.get('start')) {
#   console.log('[' + range.get('start').toFixed(2) + ',' + range.get('end').toFixed(2) + ']')
# }

handle_range_callback.shinyCallback <- function(x, model) {
  list(
    code = sprintf("
if (HTMLWidgets.shinyMode) {
  var dat = {factors: range.get('factors'), start: range.get('start'), end: range.get('end')}
  Shiny.onInputChange('%s', dat);
}
", as.character(x)),
    args = list(range = model$ref)
  )
}

handle_range_callback.customCallback <- function(x, model) {
  x
}

handle_range_callback.default <- function(x, model) {
  message("range callback not recognized - ignoring")
}

## url callbacks
##---------------------------------------------------------

handle_tap_callback <- function(x, model)
  UseMethod("handle_tap_callback", x)

handle_tap_callback.character <- function(x, model) {
  list(
    code = x,
    args = list()
  )
}

handle_tap_callback.consoleCallback <- function(x, model) {
  list(
    code = "
var cols = cb_obj.attributes.column_names;
var idx = cb_obj.attributes.selected['1d'].indices;
var res = {}
for(var i = 0; i < cols.length; i++) {
  res[cols[i]] = [];
  for (var j = 0; j < idx.length; j++) {
    res[cols[i]].push(cb_obj.attributes.data[cols[i]][idx[j]]);
  }
}
console.log(res)
    ",
    args = list()
  )
}

handle_tap_callback.shinyCallback <- function(x, model) {
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
    Shiny.onInputChange('%s', res);
  }
}
", as.character(x)),
    args = list()
  )
}

handle_tap_callback.customCallback <- function(x, model) {
  x
}

handle_tap_callback.default <- function(x, model) {
  message("url callback not recognized - ignoring")
}


## hover callbacks
##---------------------------------------------------------

handle_hover_callback <- function(x, model)
  UseMethod("handle_hover_callback", x)

handle_hover_callback.character <- function(x, model) {
  list(
    code = x,
    args = list(hover = model$ref)
  )
}

handle_hover_callback.consoleCallback <- function(x, model) {
  list(
    code = "
console.log(cb_data);
    ",
    args = list(hover = model$ref)
  )
}

handle_hover_callback.shinyCallback <- function(x, model) {
  list(
    code = sprintf("
if (HTMLWidgets.shinyMode) {
  var dat = {index: cb_data.index, geom: cb_data.geometry}
  Shiny.onInputChange('%s', dat);
}
", as.character(x)),
    args = list()
  )
}

handle_hover_callback.customCallback <- function(x, model) {
  x
}

handle_hover_callback.default <- function(x, model) {
  message("hover callback not recognized - ignoring")
}

# figure() %>% ly_points(1:10) %>%
#   x_range(callback = console_callback())

# figure() %>% ly_points(1:10) %>%
#   x_range(callback = "console.log(range.get('start'))")

