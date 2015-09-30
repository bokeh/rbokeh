
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

`[.substituted_value` <- function(x, ...) {
  # remove .substituted_value class
  xClasses <- class(x)
  substitutedValuePos <- which(xClasses == "substituted_value")
  class(x) <- xClasses[-1 * substitutedValuePos]

  # subset data
  ret <- x[...]

  # transfer attrs
  attr.names <- names(attributes(x))
  attr.names <- attr.names[attr.names != 'names']
  attributes(ret)[attr.names] <- attributes(x)[attr.names]

  # return classes
  class(ret) <- xClasses

  ret
}


# @example
# col = "blue"
# x <- sub_names(
#   data = iris
#   x = 5,
#   b = col,
#   col = "Species",
#   col2 = Species,
#   el = NULL
# )
# x
sub_names <- function(fig, data, argObj, ..., parentFrame = parent.frame()) {
  # , matchCall = match.call(parent.frame(2L))
  if (missing(data)) {
    stop(paste0("data was not supplied to ", match.call()[1]))
  }

  argNames <- names(argObj)
  if (any(argNames == "")) {
    stop("all arguments must be named arguments")
  }

  # retrieve the data with best guess
  dataNames <- names(data)

  if(!is.null(data)) {
    dataHasLength <- (nrow(data) > 1)
    dataLength <- nrow(data)
  } else {
    dataHasLength <- FALSE
    dataLength <- 1
  }

  handle_value <- function(val) {
    if(is.null(val)) { return(val) }
    if(length(val) == 1 && is.character(val) && dataHasLength) {
      if(val %in% dataNames) {
        ret <- data[[val]]
        attr(ret, "stringName") <- val
        class(ret) <- "substituted_value"
      } else {
        ret <- rep(val, dataLength)
      }
    } else {
      ret <- rep(val, dataLength)
    }
    ret
  }

  # get the unevaled values
  ret <- lapply(seq_along(argObj), function(argPos) {
    argName = argNames[argPos]
    argVal = argObj[[argPos]]

    ans <- switch(argName,
      hover = get_hover(argVal, data, parentFrame),
      lgroup = get_lgroup(argVal, fig),
      url = get_url(as.character(argVal), data),
      legend = get_legend(argVal),
      dots = print(argVal),
      xlab = as.character(argVal),
      ylab = as.character(argVal),
      switch(typeof(argVal),
        symbol = handle_value(deparse(argVal)),
        handle_value(argVal)
      )
    )
    if(inherits(ans, "try-error")) {
      stop("argument '", deparse(x), "' cannot be found")
    }
    ans
  })
  names(ret) <- argNames


  # get the x and y names and data
  ret[c("x", "y", "xName", "yName")] <- b_xy_data_and_names(
    ret$x, ret$y,
    argObj[["x"]], argObj[["y"]],
    ret$xlab, ret$ylab
  )

  return(ret)
  # return(list(ret, nameValList))
}



b_xy_data_and_names = function(x, y, xName, yName, xlab, ylab) {
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

  if(!is.null(xlab)) {
    xName <- xlab
  }
  if(!is.null(ylab)) {
    yName <- ylab
  }

  ## deal with singleton x or y
  if(length(x) == 1)
    x <- rep(x, length(y))
  if(length(y) == 1) {
    y <- rep(y, length(x))
  }


  list(x = x, y = y, xName = xName, yName = yName)
}

get_legend <- function(val) {
  valType = typeof(val)
  if (!is.null(val)) {
    if (valType != "logical") {
      if (valType != "character") {
        stop("'legend' must be a logical value or a character string")
      }
    }
  }
  val
}



if(FALSE) {
  load_all()
  col = "blue"
  sub_names(x = 5, b = col, col = "Species", col2 = Species, el = NULL, data = iris) -> x; x
}
