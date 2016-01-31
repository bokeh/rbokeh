
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


# figure() %>% ly_points(1:10) %>%
#   x_range(callback = console_callback())

# figure() %>% ly_points(1:10) %>%
#   x_range(callback = "console.log(range.get('start'))")

