
sb <- substitute

#' @import stringr
grab <- function(...) {
  nameVals <- structure(as.list(match.call()[-1]), class = "uneval") %>%
    unlist() %>%
    as.character() %>%
    lapply(function(arg) {
      if (str_detect(arg, "^sb\\(") && str_detect(arg, "\\)$")) {
        arg <- arg %>% str_replace("^sb\\(", "") %>% str_replace("\\)", "")
      }
      arg
    }) %>% unlist()

  argVals <- list(...)
  names(argVals) <- nameVals

  if(length(argVals$dots) > 1) {
    dots <- argVals$dots[-1]
    dotVals <- as.character(dots)
    dotNames <- names(dots)
    for (i in seq_along(dots)) {
      argVals[[dotNames[i]]] <- dotVals[i]
    }
  }
  argVals$dots <- NULL

  # print(argVals)
  # eval(deparse(args))
  # args[[1]] <- eval(args[[1]])
  # list(
  #   args,
  #   match.call(),
  #   sys.calls()
  # )
  argVals
}
