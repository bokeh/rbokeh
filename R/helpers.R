#' @export
asis <- function(x) {
  class(x) <- unique(c(class(x), "as_is"))
  x
}

#' @export
to_epoch <- function(x) {
  if (inherits(x, "Date")) {
    return(as.numeric(x) * 86400000)
  } else if (inherits(x, "POSIXt")) {
    return(as.numeric(x) * 1000)
  }
  x
}

#' @export
sanitize <- function(x) {
  gsub("\\.", "_", x)
}

add_layer <- function(fig, spec, lgroup, lname) {
  if (is.null(lname))
    lname <- get_next_layer_name(fig)
  if (is.null(lgroup))
    lgroup <- "common"

  fig$x$layers[[lname]] <- spec

  fig
}

get_next_layer_name <- function(obj) {
  nms <- names(obj$x$layers)
  nms <- nms[grepl("^l[0-9]+", nms)]
  if (length(nms) == 0)
    return ("l1")
  val <- as.integer(gsub("l(.*)", "\\1", nms))
  paste0("l", max(val) + 1)
}

## determine whether axis is "numeric" or "categorical"
get_glyph_axis_type <- function(a, prev_type = NULL, which = "x") {
  # this will surely get more complex...
  res <- NULL
  if (is.character(a) || is.factor(a)) {
    res <- "categorical"
  } else if (inherits(a, c("Date", "POSIXct"))) {
    res <- "datetime"
  } else {
    res <- "numeric"
  }
  if (!is.null(prev_type) && !identical(prev_type, res)) {
    message("Note: a layer has been specified with ", which,
      "-axis data of type '", res,
      "' but a layer already exists with type '", prev_type,
      ". Honoring existing layer.")
    return(prev_type)
  }
  res
}

get_glyph_range <- function(vals, prev_range, axis_type = NULL) {
  if (is.null(axis_type))
    axis_type <- get_axis_type(vals)

  if (inherits(vals, c("Date", "POSIXt")))
    vals <- to_epoch(vals)

  if (axis_type %in% c("numeric", "datetime")) {
    range(c(vals, prev_range), na.rm = TRUE)
  } else {
    # gsub removes suffixes like ":0.6"
    if (is.factor(vals))
      vals <- levels(vals)
    vals <- unique(gsub("(.*):(-*[0-9]*\\.*)*([0-9]+)*$", "\\1", vals))
    if (!is.null(prev_range)) {
      new_vals <- setdiff(vals, prev_range)
      if (length(new_vals) > 0) {
        message("A layer with categorical data was found that has ",
          "additional levels than what has been previously specified. ",
          "Appending these levels to the axis range.")
        vals <- c(prev_range, new_vals)
      }
    }
    return(vals)
  }
}

get_next_color <- function(lgroupobj, which = "fill_color", type = "discrete", theme) {
  cur_theme <- theme[[type]][[which]](10)
  n_layers <- length(lgroupobj$glyph_ids) + 1
  next_color_idx <- (n_layers - 1) %% length(cur_theme) + 1
  cur_theme[next_color_idx]
}

valid_glyph <- function(dd) {
  is_numeric_glyph <- is.numeric(dd) & dd %in% marker_pch_types
  is_named_glyph <- dd %in% marker_names
  all(is_numeric_glyph | is_named_glyph)
}

valid_color <- function(dd) {
  all(dd %in% css_colors | (nchar(as.character(dd)) == 7 && grepl("^#", dd)))
}

valid_line <- function(dd) {
  all(as.character(dd) %in% lty_names)
}

# remove fill or line props according to specified glyph
fix_glyph_color_alpha <- function(ly, cur_glyph_props) {
  if (!is.null(cur_glyph_props)) {
    # TODO: provide message if fill or line explicitly specified but not part of glyph?
    if (!cur_glyph_props$fill) {
      ly$fill_color <- NA
      ly$fill_alpha <- NA
    }
    if (!cur_glyph_props$line) {
      ly$line_color <- NA
      ly$line_alpha <- NA
    }
  }
  ly
}

# remove ns_, hov_, etc. prefixes from parameter names
get_alt_glyph_layer <- function(ly, prefix) {
  nms <- names(ly)
  nms_pre <- nms[grepl(prefix, nms)]
  nms_post <- gsub(prefix, "", nms_pre)
  for (ii in seq_along(nms_pre))
    ly[[nms_post[ii]]] <- ly[[nms_pre[ii]]]
  ly
}
