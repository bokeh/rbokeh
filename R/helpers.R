#' @export
.datatable.aware <- TRUE

## internal helper methods

get_bokeh_version <- function() {
  # assumes there is only one listed dependency here
  # (don't want dependency on yaml package just for this)
  yaml <- readLines(file.path(system.file(package = "rbokeh"), "htmlwidgets", "rbokeh.yaml"))
  yaml <- yaml[grepl("version:", yaml)]
  gsub(" +version: +(.*)", "\\1", yaml)
}

validate_fig <- function(fig, fct) {
  if(!inherits(fig$x$spec, "BokehFigure"))
    stop("Error in ", fct, ": first argument must be of type 'BokehFigure'", call. = FALSE)
}

## some things like rainbow(), etc., give hex with alpha
## Bokeh doesn't like hex alpha, so get rid of it
validate_colors <- function(opts) {
  col_fields <- c("line_color", "fill_color", "text_color")

  for(fld in col_fields) {
    if(!is.null(opts[[fld]])) {
      ind <- which(grepl("^#", opts[[fld]]) & nchar(opts[[fld]]) == 9)
      if(length(ind) > 0) {
        message("note - ", fld, " has hex colors with with alpha information - removing alpha - please specify that through fill_alpha or line_alpha")
        opts[[fld]][ind] <- substr(opts[[fld]][ind], 1 , 7)
      }
    }
  }
  opts
}

## should make this return something that will be evaluated at render time
get_next_color <- function(lgroupobj, which = "fill_color", type = "discrete", theme) {
  cur_theme <- theme[[type]][[which]](10)
  n_layers <- length(lgroupobj$glyph_ids) + 1
  next_color_idx <- (n_layers - 1) %% length(cur_theme) + 1
  cur_theme[next_color_idx]
}

check_arc_direction <- function(direction) {
  if(! all(direction %in% c("clock", "anticlock")))
    stop("'direction' must be 'clock' or 'anticlock'", call. = FALSE)
}

## take a set of layer groups
## and come up with the next increment of 'layer[int]'
gen_layer_name <- function(cur_names, prefix = "group") {
  # cur_names <- c("asdf", "layer1", "layer23", "qwert", "alayer7", "layer12b")
  if(length(cur_names) == 0) {
    name <- paste0(prefix, "1")
  } else {
    names_with_prefix <- cur_names[grepl(paste0("^", prefix, "([0-9]+)$"), cur_names)]
    if(length(names_with_prefix) == 0) {
      name <- paste0(prefix, "1")
    } else {
      nn <- as.integer(gsub(prefix, "", names_with_prefix))
      name <- paste(prefix, max(nn) + 1, sep = "")
    }
  }
  name
}

## get the axis type and range for x and y axes
get_glyph_axis_type_range <- function(x, y, assert_x = NULL, assert_y = NULL, glyph = "") {
  x_axis_type <- get_glyph_axis_type(x)
  y_axis_type <- get_glyph_axis_type(y)

  if(glyph != "")
    glyph_text <- paste("'", glyph, "' ")

  if(!is.null(assert_x)) {
    if(x_axis_type != assert_x)
      stop("Glyph ", glyph, " expects a ", assert_x, " x axis", call. = FALSE)
  }
  if(!is.null(assert_y)) {
    if(y_axis_type != assert_y)
      stop("Glyph ", glyph, "expects a ", assert_y, " y axis", call. = FALSE)
  }

  list(
    x_axis_type = x_axis_type,
    y_axis_type = y_axis_type,
    x_range = get_glyph_range(x, x_axis_type),
    y_range = get_glyph_range(y, y_axis_type)
  )
}
# attr(get_glyph_axis_type_range, "keys") <- c("x_axis_type", "y_axis_type", "x_range", "y_range")

## determine whether axis is "numeric" or "categorical"
get_glyph_axis_type <- function(a) {
  # this will surely get more complex...
  if(is.character(a) || is.factor(a)) {
    return("categorical")
  } else if(inherits(a, c("Date", "POSIXct"))) {
    return("datetime")
  } else {
    return("numeric")
  }
}

## determine the range of an axis for a glyph
get_glyph_range <- function(a, axis_type = NULL, ...) {
  if(is.null(axis_type))
    axis_type <- get_glyph_axis_type(a)
  ## ... can be size, etc. attributes
  if(axis_type %in% c("numeric", "datetime")) {
    range(a, na.rm = TRUE)
  } else {
    # gsub removes suffixes like ":0.6"
    if(is.factor(a))
      a <- levels(a)
    unique(gsub("(.*):(-*[0-9]*\\.*)*([0-9]+)*$", "\\1", a))
  }
}

validate_axis_type <- function(fig_type, cur_type, which) {
  if(length(fig_type) > 0 && length(cur_type) > 0) {
    # make this more informative...
    if(fig_type != cur_type)
      stop(which, " axis type (numerical / categorical) does not match that of other elements in this figure", call. = FALSE)
  }
}

## take a collection of glyph ranges (x or y axis)
## and find the global range across all glyphs
get_all_glyph_range <- function(ranges, padding_factor, axis_type = "numeric", log = FALSE) {
  if(axis_type == "numeric") {
    range_mat <- do.call(rbind, ranges)
    hard_range <- c(min(range_mat[,1], na.rm = TRUE),
      max(range_mat[,2], na.rm = TRUE))
    ## if log, we need to make padding multiplicative
    if(log) {
      hard_range <- hard_range * c(padding_factor * 10, 2 - (padding_factor * 10))
    } else {
      hard_range <- hard_range + c(-1, 1) * padding_factor * diff(hard_range)
    }
    if(hard_range[1] == hard_range[2])
      hard_range <- hard_range + c(-0.5, 0.5)
    hard_range
  } else if(axis_type == "datetime") {
    range_mat <- do.call(rbind, ranges)
    hard_range <- c(min(range_mat[,1], na.rm = TRUE),
      max(range_mat[,2], na.rm = TRUE))
    hard_range <- hard_range + c(-1, 1) * padding_factor / 2 * diff(hard_range)
  } else {
    sort(unique(do.call(c, ranges)))
  }
}

## give a little warning if any options are specified that won't be used
check_opts <- function(opts, type, formals = NULL) {
  opts <- opts[! names(opts) %in% c("x_name", "y_name")]

  cur_glyph_props <- glyph_props[[type]]

  valid_opts <- c("glyph", "visible", "xlab", "ylab", formals)
  if(cur_glyph_props$lp)
    valid_opts <- c(valid_opts, line_prop_names)
  if(cur_glyph_props$fp)
    valid_opts <- c(valid_opts, fill_prop_names)
  if(cur_glyph_props$tp)
    valid_opts <- c(valid_opts, text_prop_names)

  if(length(opts) > 0) {
    # only get names of opts that are not NULL
    idx <- which(sapply(opts, function(x) !is.null(x)))
    if(length(idx) > 0) {
      not_used <- setdiff(names(opts)[idx], valid_opts)
      if(length(not_used) > 0)
        message("note - arguments not used when adding glyph '", type, "': ", paste(not_used, collapse = ", "))
    }
  }
}

## take a hex color and reduce its saturation by a factor
## (used to get fill for pch=21:25)
reduce_saturation <- function(col, factor = 0.5) {
  col2 <- do.call(grDevices::rgb2hsv,
    structure(as.list(grDevices::col2rgb(col)[,1]),
    names = c("r", "g", "b")))
  col2['s', ] <- col2['s', ] * factor
  do.call(grDevices::hsv, as.list(col2[,1]))
}

## handle different x, y input types
## this should be more "class"-y
## but this will suffice
get_xy_data <- function(x, y) {
  if(is.null(y)) {
    if(stats::is.ts(x)) {
      res <- list(x = as.vector(stats::time(x)), y = as.vector(x))
    } else if(is.list(x)) {
      res <- list(x = x[[1]], y = x[[2]])
    } else {
      res <- list(x = seq_along(x), y = x)
    }
  } else {
    res <- list(x = x, y = y)
  }
  ## deal with singleton x or y
  if(length(res$x) == 1)
    res$x <- rep(res$x, length(res$y))
  if(length(res$y) == 1)
    res$y <- rep(res$y, length(res$x))
  res
}

get_xy_names <- function(x, y, xname, yname, dots) {

  if(length(xname) > 1)
    xname <- NULL
  if(length(yname) > 1)
    yname <- NULL

  if(!is.null(attr(x, "stringName")))
    xname <- attr(x, "stringName")
  if(!is.null(attr(y, "stringName")))
    yname <- attr(y, "stringName")

  if(is.null(y)) {
    if(stats::is.ts(x)) {
      res <- list(x = "time", y = xname)
    } else if(is.list(x)) {
      nms <- names(x)
      res <- list(x = nms[1], y = nms[2])
    } else {
      res <- list(x = "index", y = xname)
    }
  } else {
    res <- list(x = xname, y = yname)
  }

  # manual specification trumps
  if("xlab" %in% names(dots))
    res$x <- dots$xlab
  if("ylab" %in% names(dots))
    res$y <- dots$ylab

  res
}


resolve_line_args <- function(fig, args) {

  if(!is.null(args$color)) {
    if(!is.null(args$line_color)) {
      # if(any(args$color != args$line_color))
      #   message("both color and line_color specified - honoring line_color")
    } else {
      args$line_color <- args$color
    }
  }

  if(!is.null(args$alpha)) {
    if(!is.null(args$line_alpha)) {
      # if(any(args$alpha != args$line_alpha))
      #   message("both alpha and line_alpha specified - honoring line_alpha")
    } else {
      args$line_alpha <- args$alpha
    }
  }

  ## map to what bokeh expects
  if(is.null(args$line_dash) && !is.null(args$type))
    args$line_dash <- args$type
  args$type <- NULL

  if(is.null(args$line_width) && !is.null(args$width))
    args$line_width <- args$width
  args$width <- NULL

  if(is.numeric(args$line_dash)) {
    if(length(args$line_dash) == 1) {
      args$line_dash <- as.character(args$line_dash)
    }
  }
  if(is.character(args$line_dash)) {
    if(!args$line_dash %in% names(lty_dict))
      stop("'line_dash' should be one of: ", paste(names(lty_dict), collapse = ", "), call. = FALSE)
    args$line_dash <- lty_dict[[args$line_dash]]
  }

  if(is.numeric(args$line_cap))
    args$line_cap <- ljoin_dict[[as.character(args$line_cap)]]

  if(is.null(args$line_color))
    args$line_color <- get_next_color(fig, theme = fig$x$spec$theme)

  args$color <- NULL
  args$alpha <- NULL

  args
}

## take args color and alpha and translate them to f
resolve_color_alpha <- function(args, has_line = TRUE, has_fill = TRUE, ly = NULL, solid = FALSE, theme = NULL) {

  ## if no color at all is specified, choose from the theme
  if(is.null(args$color) && is.null(args$fill_color) && is.null(args$line_color))
    args$color <- get_next_color(ly, theme = theme)

  if(!is.null(args$color)) {
    if(!is.null(args$line_color)) {
      # if(any(args$color != args$line_color))
      #   message("both color and line_color specified - honoring line_color")
    } else {
      args$line_color <- args$color
    }
    if(!is.null(args$fill_color)) {
      # if(any(args$color != args$fill_color))
      #   message("both color and fill_color specified - honoring fill_color")
    } else {
      args$fill_color <- args$color
    }
  } else {
    if(is.null(args$line_color))
      args$line_color <- NA
  }

  if(!is.null(args$alpha)) {
    if(!is.null(args$line_alpha)) {
      # if(any(args$alpha != args$line_alpha))
      #   message("both alpha and line_alpha specified - honoring line_alpha")
    } else {
      args$line_alpha <- args$alpha
    }
    if(!is.null(args$fill_alpha)) {
      # if(any(args$alpha != args$fill_alpha))
      #   message("both alpha and fill_alpha specified - honoring fill_alpha")
    } else {
      args$fill_alpha <- args$alpha * 0.5
    }
  }

  if(solid)
    args$fill_alpha <- args$line_alpha

  args$color <- NULL
  args$alpha <- NULL

  args
}
# attr(resolve_color_alpha, "keys") <- c("color", "line_color", "fill_color", "alpha", "line_alpha", "fill_alpha")

## make sure marker fill and line properties are correct for marker glyphs
## (for example, some, such as glyph = 1, must not have fill)
resolve_glyph_props <- function(glyph, args, lgroup) {
  if(valid_glyph(glyph)) {
    cur_glyph_props <- marker_dict[[as.character(glyph)]]
    args$glyph <- cur_glyph_props$glyph
    if(cur_glyph_props$fill) {
      if(is.null(args$fill_color)) {
        if(!is.null(args$line_color)) {
          args$fill_color <- args$line_color
        } else {
          args$fill_color <- lgroup
        }
      }
    } else {
      # if set to NULL, it will use bokeh default as the fill
      if(glyph_props[[args$glyph]]$fp) {
        args$fill_color <- NA
        args$fill_alpha <- NA
      } else {
        args$fill_color <- NULL
        args$fill_alpha <- NULL
      }
    }

    if(cur_glyph_props$line) {
      if(is.null(args$line_color))
        if(!is.null(args$fill_color)) {
          args$line_color <- args$fill_color
        } else {
          args$line_color <- lgroup
        }
    } else {
      args$line_color <- NULL
      args$line_width <- NULL
      args$line_alpha <- NULL
    }
  }
  args
}
# attr(resolve_glyph_props, "keys") <- c("glyph", "line_color", "fill_color", "line_width", "line_alpha")

get_lgroup <- function(lgroup, fig) {
  if(is.null(lgroup))
    lgroup <- gen_layer_name(names(fig$x$spec$layers))
  lgroup <- as.character(lgroup)
}

get_hover2 <- function(lazy_hover_val, data, sub_fn) {

  # three cases
  # 1. evaluates right away
  # 2. evaluates to a list that can't evaluate. must look at data
  # 3. comes from a string that must have an '@' symbol

  hover_symbol <- lazy_hover_val$expr
  if (is.null(hover_symbol)) {
    return(NULL)
  }

  hover_symbol_list <- as.list(hover_symbol)

  is_list_or_c <- deparse(hover_symbol_list[[1]]) %in% c("list", "c")
  is_parseable <- FALSE
  is_at_string <- FALSE
  is_data_frame <- FALSE

  if (inherits(hover_symbol, "character")) {
    if (grepl("@", hover_symbol)) {
      is_at_string <- TRUE
      tmp_split <- strsplit(hover_symbol, "@")[[1]][-1]
      tmp_names <- gsub("^([a-zA-Z0-9_]+).*", "\\1", tmp_split)
      hover_symbol_list <- lapply(tmp_names, as.symbol)
      is_parseable <- TRUE
    }
  } else {
    # try to eval the arg to a char string
    # if it evals, check to see if it's full of
    maybe_var <- try(lazy_eval(lazy_hover_val), silent = TRUE)
    if (!inherits(maybe_var, "try-error")) {
      if(is.null(maybe_var)) {
        return(NULL)
      } else if (is.data.frame(maybe_var)) {
        hover_symbol_list <- as.list(maybe_var)
        is_data_frame <- TRUE
        is_parseable <- FALSE
        is_list_or_c <- FALSE
      } else if (is.vector(maybe_var) || is.list(maybe_var)) {
        if (all(unlist(maybe_var) %in% names(data))) {
          hover_symbol_list <- lapply(maybe_var, as.symbol)
          is_list_or_c <- FALSE
          is_parseable <- TRUE
        }
      }
    }
  }

  is_single_symbol <- (length(hover_symbol_list) == 1 && ! is_data_frame)
  if (is_list_or_c || is_single_symbol || is_parseable) {
    # item is a list, get the elements from the list
    if (is_list_or_c) {
      hover_symbol_list <- hover_symbol_list[-1]
    }
    hover_list_names <- names(hover_symbol_list)

    # if no names are supplied, then
    if (is.null(hover_list_names)) {
      # assume they want to name the value with the column names
      hover_list_names <- as.character(hover_symbol_list)
    }

    # correct missing name issues
    if (any((missing_names <- hover_list_names == ""))) {
      hover_list_names[missing_names] <- as.character(hover_symbol_list[missing_names])
    }

    # set the hover_symbol_list
    names(hover_symbol_list) <- hover_list_names

    # get results into a list
    hover_val_list <- lapply(hover_symbol_list, function(symbol_val) {
      lazy_val <- as.lazy(
        symbol_val,
        env = lazy_hover_val$env
      )
      sub_fn(lazy_val, "hover")
    })


  } else if (is_data_frame) {
    hover_val_list <- hover_symbol_list

  } else {
    # hover value is not interpretable
    hover_val <- try(lazy_eval(lazy_hover_val), silent = TRUE)

    if (inherits(hover_val, "try-error")) {
      message("there was an issue evaluating the hover argument")
    }

    hover_val_list <- as.list(hover_val)
  }

  # keep the original names
  hover_dt_names <- names(hover_val_list)

  hover_val_list <- lapply(hover_val_list, format)

  # make the hover list into a dataframe
  hover_val_dt <- as.data.frame(hover_val_list, stringsAsFactors = FALSE)

  # if(nrow(hover_val_dt) == 1) {
  #   hover_val_dt <- lapply(hover_val_dt, I)
  # }

  # make fake, easy to use key names "hover_col_1", "hover_col_2",...
  names(hover_val_dt) <- hover_dt_key <- paste0("hover_col_", seq_along(hover_dt_names))

  # list of list(pretty name, key name)
  if (is_at_string) {
    tmp <- hover_symbol
    for(ii in seq_along(hover_dt_key)) {
      tmp <- gsub(
        paste0("@", hover_dt_names[ii]),
        paste0("@", hover_dt_key[ii]),
        tmp
      )
    }
    hdict <- tmp

  } else {
    hdict <- lapply(seq_along(hover_dt_names), function(i) {
      list(hover_dt_names[i], paste0("@", hover_dt_key[i]))
    })
  }

  return(structure(list(
    data = hover_val_dt,
    dict = hdict
  ), class = "hoverSpec"))
}

# get the "hover" argument and turn it into data and dict
# if a string was provided, parse the @var values
# if a data frame was provided, the arg sould be a
# list of column names
# otherwise it should be a named list or data frame
get_hover <- function(hn, data, sub_fn) {

  # try to get the raw data
  tmp <- try(lazy_eval(hn), silent = TRUE)

  if (is.null(tmp)) {
    return(NULL)
  }

  # if failed, it's a "fancy symbol", so retrieve and eval that
  if (inherits(tmp, "try-error")) {
    tmp <- sub_fn(hn)
    # to work with code below
    hn <- b_eval_get_symbol(hn)

    # there is also a sub_fn to retrieve data, but didn't want to mess with code
  }

  # is.character is bad because it's true for try-error
  if(inherits(tmp, "character")) {
    # extract variable names
    tmp_split <- strsplit(tmp, "@")[[1]][-1]
    hn <- NULL
    if(length(tmp_split) > 0) {
      hn <- gsub("^([a-zA-Z0-9_]+).*", "\\1", tmp_split)
      hn_miss <- setdiff(hn, names(data))
      if(length(hn_miss) > 0) {
        message("hover tool couldn't find following variables in the data: ", paste(hn_miss, collapse = ", "), " - ignoring them...")
        hn <- setdiff(hn, hn_miss)
      }
      data <- subset(data, select = hn)
    }
  } else if(is.data.frame(tmp)) {
    data <- tmp
    hn <- names(data)
  } else if(inherits(tmp, "hoverSpec")) {
    return(tmp)
  } else {
    if(deparse(hn)[1] == "NULL")
      return(NULL)
    if(is.null(data)) {
      if(!is.data.frame(hn)) {
        message("hover tool not added - 'hover' must be a data frame or list of variables present in the data frame supplied as the 'data' argument")
        return(NULL)
      } else {
        data <- hn
        hn <- names(data)
      }
    } else {
      hn <- deparse(hn)[1]
      hn <- gsub("c\\(|list\\(|\\)| +", "", hn)
      hn <- strsplit(hn, ",")[[1]]
      if(all(! hn %in% names(data))) {
        message("There were no columns: ", paste(hn, collapse = ", "), " in the data for the hover tool - hover not added")
        return(NULL)
      }
      data <- subset(data, select = hn)
    }
  }
  ## to be safe, give hover columns their own name and format them as strings
  hn2 <- paste("hover_", gsub("\\.|\\(|\\)| ", "_", hn), sep = "")
  names(data) <- hn2

  for(ii in seq_len(ncol(data)))
    data[[ii]] <- format(data[[ii]])

  if(inherits(tmp, "character")) {
    for(ii in seq_along(hn))
      tmp <- gsub(paste0("@", hn[ii]), paste0("@hover_", hn[ii]), tmp)
    hdict <- tmp
  } else {
    hdict <- lapply(seq_along(hn), function(ii) list(hn[ii], paste("@", hn2[ii], sep = "")))
  }

  if(nrow(data) == 1)
    data <- lapply(data, I)

  return(structure(list(
    data = data,
    dict = hdict
  ), class = "hoverSpec"))
}


# get the "url" argument and turn it into data and "dict"
# must be a vector or a string referencing variables in data
get_url <- function(url, data, sub_fn) {
  url <- lazy_eval(url)

  if(is.null(url))
    return(NULL)
  url <- as.character(url)
  if(length(url) == 1) {
    if(!grepl("@", url)) {
      message("url tap tool not added - 'url' must be a vector of URLs or a string referencing names of 'data' with e.g. @varname")
      return(NULL)
    }
    if(!is.null(data)) {
      tmp <- strsplit(url, "@")[[1]][-1]
      vars <- gsub("(^[A-Za-z]+).*", "\\1", tmp)
      if(!all(vars %in% names(data))) {
        message("url tap tool not added - one or more of the following detected variables are not in the 'data' argument: ", paste(vars, collapse = ", "))
        return(NULL)
      } else {
        data <- subset(data, select = vars)
      }
    } else {
      message("url tap tool not added - 'url' must be a vector of URLs or a string referencing names of 'data' with e.g. @varname")
      return(NULL)
    }
  } else {
    data <- data.frame(data_url = url)
    url <- "@data_url"
  }

  return(structure(list(
    data = data,
    url = url
  ), class = "urlSpec"))
}

v_eval <- function(x, data) {
  res <- try(eval(x, data), silent = TRUE)

  if(inherits(res, "try-error")) {
    res <- try(eval(x), silent = TRUE)
    if(inherits(res, "try-error")) {
      stop("argument '", deparse(x), "' cannot be found")
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
  }

  ## variable name could have been supplied in quotes
  if(length(res) == 1 && is.character(res) && nrow(data) > 0) {
    if(res %in% names(data)) {
      nm <- res
      res <- data[[res]]
      attr(res, "stringName") <- nm
    } else {
      res <- rep(res, nrow(data))
    }
  }

  if (is.null(res))
    return(res)

  # # if the variable came from the data, give it a name
  # dp <- deparse(x)
  # if(dp[1] %in% names(data))
  #   attr(res, "nseName") <- dp

  res
}

fix_args <- function(args, n) {
  # print(args); cat("\n\n\n\n\n\n\n\n\n\n\n")

  lns <- sapply(names(args), function(item_name) {
    item_val = args[[item_name]]

    if (is.null(item_val)) {
      return(0)
    }

    switch(item_name,
      url = 1,
      hover = ifelse(is.data.frame(item_val$data), nrow(item_val$data), length(item_val$data)),

      if (is.data.frame(item_val) || is.matrix(item_val)) {
        nrow(item_val)
      } else {
        length(item_val)
      }

    )
  })
  idx <- which(!lns %in% c(0, 1, n))

  if(length(idx) > 0) {
    nms <- names(args)
    print(args[idx])
    stop("Arguments do not have correct length of ", n, ": ", paste(nms[idx], " (", lns[idx],")", sep = "", collapse = ", "))
  }

  # scl_idx <- which(lns == 1)
  # split_idx <- which(lns == n)
  null_idx <- which(lns == 0)
  if(length(null_idx) > 0)
    args[null_idx] <- NULL

  # print(args); cat("\n\n\n\n")
  args
}

## take output of map() and convert it to a data frame
map2df <- function(a) {
  dd <- data.frame(lon = a$x, lat = a$y,
    group = cumsum(is.na(a$x) & is.na(a$y)) + 1)
  dd[stats::complete.cases(dd$lon, dd$lat), ]
}


# # importFrom bitops bitShiftL bitOr
# # export
# to_uint32 <- function(x) {
#   if(is.vector(x))
#     x <- matrix(x, nrow = 1)
#   bitOr(bitOr(bitOr(bitShiftL(x[,4], 24), bitShiftL(x[,3], 16)),
#     bitShiftL(x[,2], 8)), bitShiftL(x[,1], 0))
# }

handle_singleton <- function(x, fn) {
  if(is.list(x) && length(x) == 1) {
    list(fn(x[[1]]))
  } else {
    fn(x)
  }
}

to_epoch <- function(x) {
  if(inherits(x, "Date")) {
    return(as.numeric(x) * 86400000)
  } else if(inherits(x, "POSIXt")) {
    return(as.numeric(x) * 1000)
  }
  x
}

subset_with_attributes <- function(x, ...) {
  res <- x[...]
  attrs <- attributes(x)
  attr_names <- names(attrs)
  attr_names <- attr_names[! (attr_names %in% c("names", "class"))]

  ans <- try({
    attributes(res)[attr_names] <- attributes(x)[attr_names]
  }, silent = TRUE)

  # if there's trouble setting the attributes,
  # (like in Time-Series data, 'tsp' attr)
  # try doing them one at a time
  if (inherits(ans, "try-error")) {
    for (attr_name in attr_names) {
      try({
        attributes(res)[attr_name] <- attributes(x)[attr_name]
      }, silent = TRUE)
    }
  }

  res
}

# http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
