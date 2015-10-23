
#' @import lazyeval
b_eval_get_symbol = function(x) {
  eval(parse(text = paste0("substitute(", x$expr, ", x$env)")))
}
b_eval <- function(data, parentHeight = 2) {


  if (is.null(data)) {
    fn <- function(x) {
      variableUsed = as.character(match.call()[-1])
      # ans = eval(x, envir = parent.frame(parentHeight))

      ans = lazy_eval(x)

      # if it is an "data" variable name, set the stringName to the value used, such as "xVal"
      if(!is.null(ans)) {
        if (deparse(x$expr) %in% data_name_list_c()) {
          attr(ans, "stringName") <- deparse(b_eval_get_symbol(x))
        }
      }

      return(ans)
    }
  } else {

    if (! is.data.frame(data)) {
      data <- tryCatch(as.data.frame(data),
        error = function(e) {
          stop("data must be NULL or be able to pass as.data.frame")
        }
      )
    }

    # cat("\n\nData Rows: ");print(nrow(data));cat("\n")
    fn <- function(x) {

      if (! inherits(x, "lazy")) {
        stop("argument is not of class 'lazy'")
      }

      if (is.null(x$expr)) {
        return(NULL)
      }

      xSymbol <- b_eval_get_symbol(x)
      xName <- deparse(xSymbol)

      return_ans <- function(ans) {
        if (!is.null(ans)) {
          if (deparse(x$expr) %in% data_name_list_c()) {
            attr(ans, "stringName") <- deparse(b_eval_get_symbol(x))
          } else if (xName %in% names(data)) {
            attr(ans, "stringName") <- deparse(b_eval_get_symbol(x))
          }
        }
        ans
      }
      # cat("\n\n First\n");print(x)

      # check for "as is"
      # resClass <- try(class(eval(xSymbol, parent.frame(parentHeight))), silent = TRUE)
      plainLazyEval = try(lazy_eval(x), silent = TRUE)

      # couldn't evaluate, therefore in data?
      if (inherits(plainLazyEval, "try-error")) {
        # assuming it is an expression that fails when evaluated
        # must retrieve the symbol from the envir
        # then retrieve the data with that symbol

        # include the global env so things link "*" and "+" or global user functions work
        res <- try(eval(xSymbol, data, globalenv()), silent = TRUE)
        if(inherits(res, "try-error")) {
          # print(str(data))
          # browser()
          stop("argument '", xName, "' cannot be found in data")
        }

        return(return_ans(res))
      }

      # object exists as evaluated
      res <- plainLazyEval

      if (is.null(res)) {
        return(return_ans(res))
      }

      resClass <- class(res)
      if (identical(resClass, "AsIs")) {
        # this means it evaluated properly and is suppose to be "as is"
        return(return_ans(res))
      }

      ## In this case, the user has specified a 'data' argument
      ## but has also specified an "additional parameter" argument
      ## such as fill_alpha, etc. which has been set to a variable
      ## in the calling frame
      ## for example:
      ##  col <- "blue"
      ##  figure() %>% ly_polygon(..., data = d, fill_color = col)
      ## it is looking for "col" in 'data' but instead should get it from the calling frame
      ## but right now, we throw an error
      ## and the way around it is to not use the 'data' argument
      ## and specify everything explicitly

      ## variable name could have been supplied in quotes
      if(length(res) == 1 && is.character(res) && nrow(data) > 1) {
        if(res %in% names(data)) {
          nm <- res
          res <- data[[res]]
          attr(res, "stringName") <- nm
        } else {
          res <- rep(res, nrow(data))
        }
      }

      if (is.null(res)) {
        return(return_ans(res))
      }

      # # if the variable came from the data, give it a name
      # dp <- deparse(x)
      # if(dp[1] %in% names(data))
      #   attr(res, "nseName") <- dp

      # return(return_ans(res))
      return(res)
    }
  }

  fn
}















#' @import stringr
grab <- function(...) {
  nameVals <- structure(as.list(match.call()[-1]), class = "uneval") %>%
    unlist() %>%
    as.character()

  paramIdx <- nameVals %>%
    lapply(function(nameVal) {
      str_detect(nameVal, "^p_sb\\(") && str_detect(nameVal, "\\)$")
    }) %>% unlist()
  paramNames <- nameVals[paramIdx] %>%
    lapply(function(nameVal) {
      nameVal %>%
        str_replace("^p\\_sb\\(", "") %>%
        str_replace("\\)$", "")
    }) %>% unlist()
  nameVals[paramIdx] <- paramNames

  regularIdx <- seq_along(nameVals)[! (nameVals %in% c(paramNames, "dots"))]
  regularNames <- nameVals[regularIdx] %>%
    lapply(function(nameVal) {
      nameVal %>% str_replace("^sb\\(", "") %>% str_replace("\\)", "")
    }) %>% unlist()

  nameVals[regularIdx] <- regularNames

  argVals <- list(...)
  names(argVals) <- nameVals

  ret <- argVals[regularNames]
  ret$params <- argVals[paramNames]

  if(length(argVals$dots) > 1) {
    dots <- argVals$dots[-1]
    dotVals <- as.character(dots)
    dotNames <- names(dots)
    for (i in seq_along(dots)) {
      ret$params[[dotNames[i]]] <- dotVals[i]
    }
  }

  # eval(deparse(args))
  # args[[1]] <- eval(args[[1]])
  # list(
  #   args,
  #   match.call(),
  #   sys.calls()
  # )
  ret
}

# #' @export
# asJSON.substituted_value <- function(x, ...) {
#   class(x) <- NULL
#   asJSON(x)
# }
# `[.substituted_value` <- function(x, ...) {
#   # remove .substituted_value class
#   xClasses <- class(x)
#   substitutedValuePos <- which(xClasses == "substituted_value")
#   class(x) <- xClasses[-1 * substitutedValuePos]
#
#   # subset data
#   ret <- x[...]
#
#   # transfer attrs
#   attr.names <- names(attributes(x))
#   attr.names <- attr.names[attr.names != 'names']
#   attributes(ret)[attr.names] <- attributes(x)[attr.names]
#
#   # return classes
#   class(ret) <- xClasses
#
#   ret
# }


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
        # class(ret) <- "substituted_value"
      } else {
        ret <- rep(val, dataLength)
      }
    } else {
      ret <- val
    }
    ret
  }

  handle_language = function(argVal) {
    ret <- try(eval(argVal, data), silent = TRUE)

    if(inherits(ret, "try-error")) {
      ret <- try(eval(argVal), silent = TRUE)
      if(inherits(res, "try-error")) {
        stop("argument '", deparse(x), "' cannot be found")
      }
    }
    ret
  }

  # get the unevaled values
  parse_values <- function(x) {
    xNames <- names(x)
    res <- lapply(seq_along(x), function(argPos) {
      argName = xNames[argPos]
      argVal = x[[argPos]]

      ans <- switch(argName,
        params = parse_values(argVal),

        hover = get_hover(argVal, data, parentFrame),
        lgroup = get_lgroup(argVal, fig),
        url = get_url(argVal, data),
        legend = get_legend(argVal),
        xlab = as.character(argVal),
        ylab = as.character(argVal),
        direction = as.character(argVal),
        switch(typeof(argVal),
          language = handle_language(argVal),
          symbol = handle_value(deparse(argVal)),
          handle_value(argVal)
        )
      )
      if(inherits(ans, "try-error")) {
        stop("argument '", deparse(x), "' cannot be found")
      }
      ans
    })
    names(res) <- xNames
    res
  }
  ret <- parse_values(argObj)

  if (!is.null(ret$direction)) {
    check_arc_direction(ret$direction)
  }
  # ret <- lapply(seq_along(argObj), function(argPos) {
  #   argName = argNames[argPos]
  #   argVal = argObj[[argPos]]
  #
  #   ans <- switch(argName,
  #     hover = get_hover(argVal, data, parentFrame),
  #     lgroup = get_lgroup(argVal, fig),
  #     url = get_url(as.character(argVal), data),
  #     legend = get_legend(argVal),
  #     xlab = as.character(argVal),
  #     ylab = as.character(argVal),
  #     switch(typeof(argVal),
  #       language = handle_language(argVal),
  #       symbol = handle_value(deparse(argVal)),
  #       handle_value(argVal)
  #     )
  #   )
  #   if(inherits(ans, "try-error")) {
  #     stop("argument '", deparse(x), "' cannot be found")
  #   }
  #   ans
  # })
  # names(ret) <- argNames


  # get the x and y names and data
  ret[c("x", "y", "xName", "yName")] <- b_xy_data_and_names(
    ret$x, ret$y,
    argObj[["x"]], argObj[["y"]],
    ret$xlab, ret$ylab
  )

  return(ret)
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


subset_arg_obj = function(argObj, idxs) {
  nonSubsetableNames = c("lgroup", "lname", "url", "legend", "xlab", "ylab", "xName", "yName")
  n = length(idxs)

  subset_obj <- function(x) {
    argNames <- names(x)
    ret <- lapply(argNames, function(key) {
      val <- x[[key]]

      if (key %in% nonSubsetableNames) {
        return(val)
      }
      if (is.null(val)) {
        return(val)
      }

      if (key == "hover") {
        val$data <- subset(val$data, 1:nrow(val$data) %in% idxs)
        return(val)
      }

      if (length(val) == 1) {
        return(val)
      }

      if (length(val) < max(idxs)) {
        print(list(val = val, key = key, idxs = idxs))
        stop("bad key val for subset")
      }

      if (length(attributes(val)) > 1) {
        return(subset_with_attributes(val, idxs))
      } else {
        return(val[idxs])
      }
    })
    names(ret) <- argNames

    ret
  }

  subsetArgObj <- lapply(argObj, subset_obj)

  ret <- lapply(subsetArgObj, fix_args, n)

  ret
}
























































data_name_list = function() {
  list(
    c("x0", "y0", "x1", "y1"),
    c("xleft", "ybottom", "xright", "ytop"),
    c("xs", "ys"),
    c("x", "y")
  )
}
data_name_list_c = function() {
  unlist(data_name_list())
}
grab_data_index = function(nameVals) {
  dataNameList = data_name_list()
  dataNameIdx = lapply(dataNameList, function(dataNames) {
    all(dataNames %in% nameVals)
  }) %>% unlist() %>% which()

  if (length(dataNameIdx) == 0) {
    stop("invalid data names supplied to 'grab'")
  }
  if (length(dataNameIdx) > 1) {
    stop("too many data names were supplied to 'grab'")
  }

  dataIdx <- nameVals %in% dataNameList[[dataNameIdx]]

}

grab_regular_index = function(nameVals) {
  nameVals %in% c("hover", "group", "lname", "lgroup", "legend", "xlab", "ylab")
}

grab_param_index = function(dataIdx, regularIdx) {
  !(dataIdx | regularIdx)
}

grab2 <- function(..., dots, nullData = FALSE) {

  if (missing(dots)) {
    stop("'dots' must be supplied")
  }

  argVals <- lazy_dots(...)

  names(argVals) <- lapply(argVals, "[[", "expr") %>%
    unlist() %>%
    as.character()

  for (key in names(dots)) {
    argVals[[key]] <- dots[[key]]
  }

  nameVals <- names(argVals)

  if (nullData) {
    dataIdx <- rep(FALSE, length(nameVals))
    dt = list()

  } else {
    dataIdx <- grab_data_index(nameVals)

    dt <- argVals[dataIdx];
    names(dt) <- nameVals[dataIdx]
  }

  regularIdx <- grab_regular_index(nameVals)
  info <- argVals[regularIdx];
  names(info) <- nameVals[regularIdx]

  paramIdx   <- grab_param_index(dataIdx, regularIdx)
  params <- argVals[paramIdx]
  names(params) <- nameVals[paramIdx]

  list(data = dt, info = info, params = params)
}





sub_names2 <- function(
  fig, data, argObj,
  parentFrame = parent.frame(),
  processDataAndNames = TRUE
) {
  # , matchCall = match.call(parent.frame(2L))
  if (missing(fig)) {
    stop(paste0("'fig' was not supplied to ", match.call()[1]))
  }
  if (missing(data)) {
    stop(paste0("'data' was not supplied to ", match.call()[1]))
  }
  if (missing(argObj)) {
    stop(paste0("'data' was not supplied to ", match.call()[1]))
  }

  sub_fn <- b_eval(data)

  # retrieve the data with best guess
  dataNames <- names(data)

  # get the unevaled values
  parse_values <- function(x) {
    xNames <- names(x)
    res <- lapply(seq_along(x), function(argPos) {
      argName = xNames[argPos]
      argVal = x[[argPos]]

      # print(list(argName, argVal, typeof(argVal)))
      ans <- switch(argName,
        params    = parse_values(argVal),

        # send symbol to hover; also sending "data finding" function
        hover     = get_hover(argVal, data, parentFrame, sub_fn),
        lgroup    = get_lgroup(lazy_eval(argVal), fig),
        url       = get_url(argVal, data, sub_fn),
        legend    = get_legend(lazy_eval(argVal)),
        position  = as.character(lazy_eval(argVal)),
        xlab      = as.character(lazy_eval(argVal)),
        ylab      = as.character(lazy_eval(argVal)),
        direction = as.character(lazy_eval(argVal)),
        sub_fn(argVal)
      )
      ans
    })
    names(res) <- xNames
    res
  }

  # parse all values for
  ret = lapply(argObj, parse_values)

  if (!is.null(ret$params$direction)) {
    check_arc_direction(ret$params$direction)
  }

  # get the x and y names and data
  # print(ret)
  # print(ret$x)
  # print(ret$y)
  # browser()
  if (processDataAndNames) {
    d2AndNames <- b_xy_data_and_names2(
      ret$data[1:2],
      ret$info$xlab, ret$info$ylab
    )

    ret$data[1:2] <- d2AndNames$d2
    ret$info[c("xName", "yName")] <- d2AndNames$xyName
  }

  return(ret)
}



b_xy_data_and_names2 = function(d2, xlab, ylab) {
  x = d2[[1]]
  y = d2[[2]]

  xName <- "x"
  yName <- "y"


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
      nms <- names(x)
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

  d2[[1]] = x
  d2[[2]] = y


  list(d2 = d2, xyNames = list(xName = xName, yName = yName))
}
