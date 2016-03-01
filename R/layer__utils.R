
# Retrieve the symbol used from a lazy env
#' @import lazyeval
b_eval_get_symbol <- function(x) {
  eval(parse(text = paste0("substitute(", x$expr, ", x$env)")))
}

lazy_eval_unname <- function(x, data = NULL) {
  x <- lazy_eval(x = x, data = data)
  if(is.vector(x))
    x <- unname(x)
  x
}

#' Eval lazy symbol
#'
#' Evaluate the argument from the env it came from, or from within the data.  The arg supplied to the returned function must be lazy.
#' @param data data set to be used for evaluation.  May be \code{NULL}
#' @return a function that takes in one lazy argument to be evaluated
b_eval <- function(data) {

  deparse_or_string <- function(val) {
    if(is.character(val)) {
      val
    } else {
      deparse(val)
    }
  }

  if(is.null(data)) {
    fn <- function(x, key = stop("didn't supply key in b_eval fn")) {
      ans <- lazy_eval_unname(x)

      # if it is an "data" variable name, set the stringName to the value used, such as "xVal"
      if (!is.null(ans)) {
        if (key %in% data_name_list_c()) {
          if (is.language(x$expr) || is.symbol(x$expr)) {
            attr(ans, "stringName") <- deparse(x$expr)
          }
        }
      }

      return(ans)
    }

  } else {

    if(! is.data.frame(data)) {
      data <- tryCatch(as.data.frame(data),
        error = function(e) {
          stop("data must be NULL or be able to pass as.data.frame")
        }
      )
    }

    fn <- function(x, key = stop("didn't supply key in b_eval fn")) {
      # internal helper
      if(! inherits(x, "lazy")) {
        # print(x)
        # browser()
        stop("argument is not of class 'lazy'")
      }

      if(is.null(x$expr)) {
        return(NULL)
      }

      return_ans <- function(ans, x_name = stop("x_name not supplied")) {
        # attach the variable name from where it came
        if(!is.null(ans)) {
          if(! is.null(x_name)) {
            # if a name can be supplied, add it
            attr(ans, "stringName") <- x_name
          }
        }
        ans
      }

      if (is.symbol(x$expr) || is.language(x$expr)) {
        # x$expr is symbol
        x_name <- deparse(x$expr)
        res <- lazy_eval_unname(x, data = data)
      } else {
        # is a real value (not symbol)
        x_name <- NULL
        res <- x$expr
      }

      res_class <- class(res)
      if(identical(res_class, "AsIs")) {
        # this means it evaluated properly and is suppose to be "as is"
        return(return_ans(res, x_name))
      }

      ## variable name could have been supplied in quotes
      if(length(res) == 1 && is.character(res) && nrow(data) > 1) {
        if(res %in% names(data)) {
          nm <- res
          res <- data[[res]]
          return(return_ans(res, nm))
        } else {
          # # TODO. I vote to not repeat parameters unless given a vector to begin with
          res <- rep(res, nrow(data))
          return(return_ans(res, x_name))
        }
      }

      # is not an expression
      # is not an as is
      # is not a single character string
      return(return_ans(res, x_name))
    }
  }

  fn
}


get_legend <- function(val) {
  val_type <- typeof(val)
  if(!is.null(val)) {
    if(val_type != "logical") {
      if(!(val_type == "character" || is.factor(val))) {
        stop("'legend' must be a logical value or a character string")
      }
    }
  }
  val
}


# Subset args object
#
# @param arg_obj args object to be subsetted
# @param idxs indicies that should be used
# @return similar arg_obj, just subsetted at the suggested indicies
subset_arg_obj <- function(arg_obj, idxs) {
  non_subsetable_names <- c("lgroup", "lname", "legend", "xlab", "ylab", "x_name", "y_name")
  n <- length(idxs)

  subset_obj <- function(x) {
    arg_names <- names(x)
    ret <- lapply(arg_names, function(key) {
      val <- x[[key]]

      if(key %in% non_subsetable_names) {
        return(val)
      }

      if(is.null(val)) {
        return(val)
      }

      if(key %in% c("hover", "url")) {
        if (is.null(nrow(val$data))) {
          return(val)
        }
        val$data <- subset(val$data, 1:nrow(val$data) %in% idxs)
        return(val)
      }

      if(length(val) == 1) {
        return(val)
      }

      if(length(val) < max(idxs)) {
        print(list(val = val, key = key, idxs = idxs))
        stop("bad key val for subset")
      }

      if(length(attributes(val)) > 1) {
        return(subset_with_attributes(val, idxs))
      } else {
        return(val[idxs])
      }
    })
    names(ret) <- arg_names

    ret
  }

  subset_arg_obj <- lapply(arg_obj, subset_obj)

  ret <- lapply(subset_arg_obj, fix_args, n)

  ret
}





#' List of all types of data name structures that could appear
data_name_list <- function() {
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
data_name_list_c <- function() {
  unlist(data_name_list())
}


# helper function to get all 'data' positions from the given names
grab_data_index <- function(name_vals) {
  data_name_list <- data_name_list()
  data_name_idx <- lapply(data_name_list, function(data_names) {
    all(data_names %in% name_vals)
  }) %>% unlist() %>% which()

  if(length(data_name_idx) == 0) {
    stop("invalid data names supplied to 'grab'")
  }
  if(length(data_name_idx) > 1) {
    data_name_idx <- min(data_name_idx)
    # stop("too many data names were supplied to 'grab'")
  }

  data_idx <- name_vals %in% data_name_list[[data_name_idx]]

}

# helper function to get all 'info' positions from the given names
grab_regular_index <- function(name_vals) {
  name_vals %in% c("hover", "url", "group", "lname", "lgroup", "legend", "xlab", "ylab")
}

# helper function to get all 'params' positions from the given names
grab_param_index <- function(data_idx, regular_idx) {
  !(data_idx | regular_idx)
}

# Grab function
#
# This function takes in all arguments and lazy wraps them.  Once wrapped, they are placed into one of three groups: data, info, or params.  params should be able to be sent to \code{check_opts} without issue
# @param ... arguments (that may or may not be able to evaluate right away) to be captured
# @param dots output from \code{lazy_dots()} from the parent function
# @param null_data boolen to determine if data args are being passed in
# @return list of three groups: data, info, and params
grab <- function(..., dots, null_data = FALSE) {

  if(missing(dots)) {
    stop("'dots' must be supplied")
  }

  pf <- parent.frame()
  pf2 <- parent.frame(2)

  arg_vals <- pryr::named_dots(...) %>%
    lapply(function(expr) {
      correct_value <- pryr::substitute_q(expr, pf)
      lazy_(correct_value, pf2)
    })

  # forces a name stomp (but that shouldn't happen)
  for (key in names(dots)) {
    arg_vals[[key]] <- dots[[key]]
  }

  name_vals <- names(arg_vals)

  if(null_data) {
    data_idx <- rep(FALSE, length(name_vals))
    dt <- list()

  } else {
    data_idx <- grab_data_index(name_vals)

    dt <- arg_vals[data_idx];
    names(dt) <- name_vals[data_idx]
  }

  regular_idx <- grab_regular_index(name_vals)
  info <- arg_vals[regular_idx];
  names(info) <- name_vals[regular_idx]

  param_idx   <- grab_param_index(data_idx, regular_idx)
  params <- arg_vals[param_idx]
  names(params) <- name_vals[param_idx]

  list(data = dt, info = info, params = params)
}




#' Retrieve and properly parse all data
#'
#' @param fig figure to be used
#' @param data data to be used
#' @param arg_obj args object supplied by \code{grab}
#' @param process_data_and_names boolean to determine if the data and x_name and y_name should be post processed
#' @return list of three groups: data, info, and params.
sub_names <- function(
  fig, data, arg_obj,
  process_data_and_names = TRUE
) {
  # , matchCall = match.call(parent.frame(2L))
  if(missing(fig)) {
    stop(paste0("'fig' was not supplied to ", match.call()[1]))
  }
  if(missing(data)) {
    stop(paste0("'data' was not supplied to ", match.call()[1]))
  }
  if(missing(arg_obj)) {
    stop(paste0("'data' was not supplied to ", match.call()[1]))
  }

  sub_fn <- b_eval(data)

  # retrieve the data with best guess
  data_names <- names(data)

  # get the unevaled values
  parse_values <- function(x) {
    x_names <- names(x)
    res <- lapply(seq_along(x), function(arg_pos) {
      arg_name <- x_names[arg_pos]
      arg_val <- x[[arg_pos]]

      # print(list(arg_name, arg_val, typeof(arg_val)))
      ans <- switch(arg_name,
        params    = parse_values(arg_val),

        # send symbol to hover; also sending "data finding" function
        hover     = get_hover2(arg_val, data, sub_fn),
        lgroup    = get_lgroup(lazy_eval_unname(arg_val), fig),
        url       = get_url(arg_val, data, sub_fn),
        legend    = get_legend(lazy_eval_unname(arg_val)),
        lname     = as.character(lazy_eval_unname(arg_val)),
        position  = as.character(lazy_eval_unname(arg_val)),
        lname     = as.character(lazy_eval_unname(arg_val)),
        xlab      = as.character(lazy_eval_unname(arg_val)),
        ylab      = as.character(lazy_eval_unname(arg_val)),
        direction = as.character(lazy_eval_unname(arg_val)),
        anchor    = as.character(lazy_eval_unname(arg_val)),
        sub_fn(arg_val, arg_name)
      )
      ans
    })
    names(res) <- x_names
    res
  }

  # parse all values for
  ret <- lapply(arg_obj, parse_values)

  if(!is.null(ret$params$direction)) {
    check_arc_direction(ret$params$direction)
  }

  # TODO: handle histogram and hexbin here...

  # get the x and y names and data
  if(process_data_and_names) {
    d2_and_names <- b_xy_data_and_names2(
      ret$data[1:2],
      ret$info$xlab, ret$info$ylab
    )

    ret$data[1:2] <- d2_and_names$d2
    ret$info[c("x_name", "y_name")] <- d2_and_names$xy_name
  }

  return(ret)
}


# process data and data names
# @param dt only the first two components of the data section
# @param xlab xlab argument
# @param ylab ylab argument
# @return a list containing d2 and xy_names. d2 is the first two data positions and xy_names is a list of x_name and y_name
b_xy_data_and_names2 <- function(d2, xlab, ylab) {
  x <- d2[[1]]
  y <- d2[[2]]

  x_name <- "x"
  y_name <- "y"


  if(!is.null(attr(x, "stringName")))
    x_name <- attr(x, "stringName")
  if(!is.null(attr(y, "stringName")))
    y_name <- attr(y, "stringName")



  if(is.null(y)) {
    if(stats::is.ts(x)) {
      y <- as.vector(x)
      x <- as.vector(time(x))
      y_name <- x_name
      x_name <- "time"
    } else if(is.list(x)) {
      nms <- names(x)
      y <- x[[2]]
      x <- x[[1]]
      x_name <- nms[1]
      y_name <- nms[2]
    } else {
      y <- x
      x <- seq_along(x)
      y_name <- x_name
      x_name <- "index"
    }
  }

  # manual specification trumps

  if(!is.null(xlab)) {
    x_name <- xlab
  }
  if(!is.null(ylab)) {
    y_name <- ylab
  }

  ## deal with singleton x or y
  if(length(x) == 1)
    x <- rep(x, length(y))
  if(length(y) == 1) {
    y <- rep(y, length(x))
  }

  d2[[1]] <- x
  d2[[2]] <- y


  list(d2 = d2, xy_names = list(x_name = x_name, y_name = y_name))
}
