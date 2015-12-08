
#' Retrieve the symbol used from a lazy env
#' @import lazyeval
b_eval_get_symbol = function(x) {
  eval(parse(text = paste0("substitute(", x$expr, ", x$env)")))
}

#' Eval lazy symbol
#'
#' Evaluate the argument from the env it came from, or from within the data.  The arg supplied to the returned function must be lazy.
#' @param data data set to be used for evaluation.  May be \code{NULL}
#' @return a function that takes in one lazy argument to be evaluated
b_eval <- function(data) {


  if (is.null(data)) {
    fn <- function(x) {
      ans = lazy_eval(x)

      # if it is an "data" variable name, set the stringName to the value used, such as "xVal"
      if(!is.null(ans)) {
        if (
          paste0(deparse(x$expr), collapse = "") %in% data_name_list_c()
        ) {
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

    fn <- function(x) {
      # internal helper
      if (! inherits(x, "lazy")) {
        # print(x)
        # browser()
        stop("argument is not of class 'lazy'")
      }

      if (is.null(x$expr)) {
        return(NULL)
      }

      xSymbol <- b_eval_get_symbol(x)
      xName <- deparse(xSymbol)

      return_ans <- function(ans) {
        # attach the variable name from where it came
        if (!is.null(ans)) {
          if (deparse(x$expr) %in% data_name_list_c()) {
            # it's data, force the stringName
            attr(ans, "stringName") <- deparse(b_eval_get_symbol(x))
          } else if (xName %in% names(data)) {
            # it's parameter, add for fun
            attr(ans, "stringName") <- deparse(b_eval_get_symbol(x))
          }
        }
        ans
      }
      # cat("\n\n First\n");print(x)

      # if the single item is in the data names, return that column
      if (xName %in% names(data)) {
        res <- eval(xSymbol, data, globalenv())
        return(return_ans(res))
      }

      # check for "as is"
      plainLazyEval = try(lazy_eval(x), silent = TRUE)

      # couldn't evaluate, therefore in data?
      if (inherits(plainLazyEval, "try-error")) {
        # assuming it is an expression that fails when evaluated
        # must retrieve the symbol from the envir
        # then retrieve the data with that symbol

        # include the global env so things like "*" and "+" or global user functions work
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

      ## variable name could have been supplied in quotes
      if(length(res) == 1 && is.character(res) && nrow(data) > 1) {
        if(res %in% names(data)) {
          nm <- res
          res <- data[[res]]
          attr(res, "stringName") <- nm
        } else {
          # # TODO. I vote to not repeat parameters unless given a vector to begin with
          res <- rep(res, nrow(data))
        }
      }

      if (is.null(res)) {
        return(return_ans(res))
      }

      return(return_ans(res))
    }
  }

  fn
}


get_legend <- function(val) {
  valType = typeof(val)
  if (!is.null(val)) {
    if (valType != "logical") {
      if (!(valType == "character" || is.factor(val))) {
        stop("'legend' must be a logical value or a character string")
      }
    }
  }
  val
}


#' Subset args object
#'
#' @param argObj args object to be subsetted
#' @param idxs indicies that should be used
#' @return similar argObj, just subsetted at the suggested indicies
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
        # browser()
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





#' List of all types of data name structures that could appear
data_name_list = function() {
  # order from most specific to least specific
  list(
    c("x0", "y0", "x1", "y1", "cx0", "cy0", "cx1", "cy1"),
    c("x0", "y0", "x1", "y1", "cx", "cy"),
    c("x0", "y0", "x1", "y1"),
    c("xleft", "ybottom", "xright", "ytop"),
    c("xs", "ys"),
    c("x", "y"),
    c("x")
  )
}
data_name_list_c = function() {
  unlist(data_name_list())
}


#' helper function to get all 'data' positions from the given names
grab_data_index = function(nameVals) {
  dataNameList = data_name_list()
  dataNameIdx = lapply(dataNameList, function(dataNames) {
    all(dataNames %in% nameVals)
  }) %>% unlist() %>% which()

  if (length(dataNameIdx) == 0) {
    stop("invalid data names supplied to 'grab'")
  }
  if (length(dataNameIdx) > 1) {
    dataNameIdx <- min(dataNameIdx)
    # stop("too many data names were supplied to 'grab'")
  }

  dataIdx <- nameVals %in% dataNameList[[dataNameIdx]]

}

#' helper function to get all 'info' positions from the given names
grab_regular_index = function(nameVals) {
  nameVals %in% c("hover", "group", "lname", "lgroup", "legend", "xlab", "ylab")
}

#' helper function to get all 'params' positions from the given names
grab_param_index = function(dataIdx, regularIdx) {
  !(dataIdx | regularIdx)
}

#' Grab function
#'
#' This function takes in all arguments and lazy wraps them.  Once wrapped, they are placed into one of three groups: data, info, or params.  params should be able to be sent to \code{check_opts} without issue
#' @param ... arguments (that may or may not be able to evaluate right away) to be captured
#' @param dots output from \code{lazy_dots()} from the parent function
#' @param nullData boolen to determine if data args are being passed in
#' @return list of three groups: data, info, and params
grab <- function(..., dots, nullData = FALSE) {

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




#' retrieve and properly parse all data
#'
#' @param fig figure to be used
#' @param data data to be used
#' @param argObj args object supplied by \code{grab}
#' @param processDataAndNames boolean to determine if the data and xName and yName should be post processed
#' @return list of three groups: data, info, and params.
sub_names <- function(
  fig, data, argObj,
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
        hover     = get_hover2(argVal, data, sub_fn),
        lgroup    = get_lgroup(lazy_eval(argVal), fig),
        url       = get_url(argVal, data, sub_fn),
        legend    = get_legend(lazy_eval(argVal)),
        position  = as.character(lazy_eval(argVal)),
        xlab      = as.character(lazy_eval(argVal)),
        ylab      = as.character(lazy_eval(argVal)),
        direction = as.character(lazy_eval(argVal)),
        anchor    = as.character(lazy_eval(argVal)),
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


#' process data and data names
#' @param dt only the first two components of the data section
#' @param xlab xlab argument
#' @param ylab ylab argument
#' @return a list containing d2 and xyNames. d2 is the first two data positions and xyNames is a list of xName and yName
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
