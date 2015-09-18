
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
b_xy_data_and_names = function(x, y, xName, yName, xLab, yLab) {
  if(length(xName) > 1)
    xName <- NULL
  if(length(yName) > 1)
    yName <- NULL

  if(!is.null(attr(x, "stringName")))
    xName <- attr(x, "stringName")
  if(!is.null(attr(y, "stringName")))
    yName <- attr(y, "stringName")

  if(is.null(y)) {
    if(is.ts(x)) {
      y = as.vector(x)
      x = as.vector(time(x))
      yName <- xName
      xName <- "time"
    } else if(is.list(x)) {
      nms <- names(ret$x)
      y = x[[2]]
      x = x[[1]]
      xName = nms[1]
      yName = nms[2]
    } else {
      y = x
      x = seq_along(x)
      yName = xName
      xName = "index"
    }
  }

  # manual specification trumps

  if(!is.null(xLab)) {
    xName <- xLab
  }
  if(!is.null(yLab)) {
    yName <- yLab
  }

  ## deal with singleton x or y
  if(length(x) == 1)
    x <- rep(x, length(y))
  if(length(y) == 1) {
    y <- rep(y, length(x))
  }


  list(x = x, y = y, xName = xName, yName = yName)
}
