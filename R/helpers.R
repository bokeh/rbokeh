## internal helper methods

resolve_line_args <- function(fig, args) {

  if(!is.null(args$color)) {
    if(!is.null(args$line_color)) {
      if(args$color != args$line_color)
        message("both color and line_color specified - honoring line_color")
    } else {
      args$line_color <- args$color
    }
  }

  if(!is.null(args$alpha)) {
    if(!is.null(args$line_alpha)) {
      if(args$alpha != args$line_alpha)
        message("both alpha and line_alpha specified - honoring line_alpha")
    } else {
      args$line_alpha <- args$alpha
    }
  }

  ## map to what bokeh expects
  args$line_dash <- args$type
  args$type <- NULL

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
    args$line_color <- get_next_color(fig)

  args
}

validate_fig <- function(fig, fct) {
  if(!inherits(fig, "BokehFigure"))
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
get_next_color <- function(lgroupobj, which = "fill_color", type = "discrete") {
  cur_theme <- bk_theme[[which]][[type]](10)
  n_layers <- length(lgroupobj$glyph_ids) + 1
  next_color_idx <- (n_layers - 1) %% length(cur_theme) + 1
  cur_theme[next_color_idx]
}

check_arc_direction <- function(direction) {
  if(!direction %in% c("clock", "anticlock"))
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
  cur_glyph_props <- glyph_props[[type]]

  valid_opts <- c("glyph", "xlab", "ylab", formals)
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
        message("note - arguments not used: ", paste(not_used, collapse = ", "))
    }
  }
}

## take a hex color and reduce its saturation by a factor
## (used to get fill for pch=21:25)
reduce_saturation <- function(col, factor = 0.5) {
  col2 <- do.call(rgb2hsv, structure(as.list(col2rgb(col)[,1]), names = c("r", "g", "b")))
  col2['s', ] <- col2['s', ] * factor
  do.call(hsv, as.list(col2[,1]))
}

## handle different x, y input types
## this should be more "class"-y
## but this will suffice
get_xy_data <- function(x, y) {
  if(is.null(y)) {
    if(is.ts(x)) {
      return(list(x = as.vector(time(x)), y = as.vector(x)))
    } else if(is.list(x)) {
      return(list(x = x[[1]], y = x[[2]]))
    } else {
      return(list(x = seq_along(x), y = x))
    }
  }
  list(x = x, y = y)
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
    if(is.ts(x)) {
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

## take args color and alpha and translate them to f
resolve_color_alpha <- function(args, has_line = TRUE, has_fill = TRUE, ly, solid = FALSE) {

  ## if no color at all is specified, choose from the theme
  if(is.null(args$color) && is.null(args$fill_color) && is.null(args$line_color))
    args$color <- get_next_color(ly)

  if(!is.null(args$color)) {
    if(!is.null(args$line_color)) {
      if(args$color != args$line_color)
        message("both color and line_color specified - honoring line_color")
    } else {
      args$line_color <- args$color
    }
    if(!is.null(args$fill_color)) {
      if(args$color != args$fill_color)
        message("both color and fill_color specified - honoring fill_color")
    } else {
      args$fill_color <- args$color
    }
  }

  if(!is.null(args$alpha)) {
    if(!is.null(args$line_alpha)) {
      if(args$alpha != args$line_alpha)
        message("both alpha and line_alpha specified - honoring line_alpha")
    } else {
      args$line_alpha <- args$alpha
    }
    if(!is.null(args$fill_alpha)) {
      if(args$alpha != args$fill_alpha)
        message("both alpha and fill_alpha specified - honoring fill_alpha")
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

## make sure marker fill and line properties are correct for marker glyphs
## (for example, some, such as glyph = 1, must not have fill)
resolve_glyph_props <- function(glyph, args, lgroup) {
  if(glyph %in% names(marker_dict)) {
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
      if(cur_glyph_props$line) {
        if(is.null(args$fill_alpha)) {
          args$fill_alpha <- 0.5
        } else {
          args$fill_alpha <- args$fill_alpha * 0.5
        }
      }
    } else {
      args$fill_color <- NA
      args$fill_alpha <- NA
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

get_lgroup <- function(lgroup, fig) {
  if(is.null(lgroup))
    lgroup <- gen_layer_name(names(fig$layers))
  lgroup <- as.character(lgroup)
}

# get the "hover" argument and turn it into data and dict
# if a data frame was provided, the arg sould be a
# list of column names
# otherwise it should be a named list or data frame
get_hover <- function(hn, data) {
  tmp <- try(eval(hn), silent = TRUE)
  if(is.data.frame(tmp)) {
    data <- tmp
    hn <- names(data)
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
      data <- data[hn]
    }
  }
  # hn <- setdiff(hn, c("x", "y", "size", "glyph", "color", "line_color", "fill_color"))
  hn2 <- gsub("\\.|\\(|\\)| ", "_", hn)
  names(data) <- hn2

  hdict <- lapply(seq_along(hn), function(ii) list(hn[ii], paste("@", hn2[ii], sep = "")))

  return(structure(list(
    data = data,
    dict = hdict
  ), class = "hoverSpec"))
}

v_eval <- function(x, data) {
  res <- eval(x, data)

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

  if(is.null(res))
    return(res)
  dp <- deparse(x)[1]
  if(dp %in% names(data))
    attr(res, "nseName") <- dp
  res
}

fix_args <- function(args, n) {
  lns <- sapply(args, length)
  nms <- names(args)
  idx <- which(!lns %in% c(0, 1, n))

  if(length(idx) > 0)
    stop("Arguments do not have correct length: ", paste(nms[idx], " (", lns[idx],")", sep = "", collapse = ", "))

  # scl_idx <- which(lns == 1)
  # split_idx <- which(lns == n)
  null_idx <- which(lns == 0)
  if(length(null_idx) > 0)
    args[null_idx] <- NULL

  args
}

## take output of map() and convert it to a data frame
map2df <- function(a) {
  dd <- data.frame(lon = a$x, lat = a$y,
    group = cumsum(is.na(a$x) & is.na(a$y)) + 1)
  dd[complete.cases(dd$lon, dd$lat), ]
}


# # importFrom bitops bitShiftL bitOr
# # export
# to_uint32 <- function(x) {
#   if(is.vector(x))
#     x <- matrix(x, nrow = 1)
#   bitOr(bitOr(bitOr(bitShiftL(x[,4], 24), bitShiftL(x[,3], 16)),
#     bitShiftL(x[,2], 8)), bitShiftL(x[,1], 0))
# }

to_epoch <- function(x) {
  if(inherits(x, "Date")) {
    return(as.numeric(x) * 86400000)
  } else if(inherits(x, "POSIXct")) {
    return(as.numeric(x) * 1000)
  }
  x
}

subset_with_attributes <- function(x, ...) {
  res <- x[...]
  attr.names <- names(attributes(x))
  attr.names <- attr.names[attr.names != 'names']
  attributes(res)[attr.names] <- attributes(x)[attr.names]
  res
}




