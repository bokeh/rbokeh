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


#' Add a small amount of (rbokeh-compatible) noise to a character vector
#'
#' @param x numeric vector to which jitter should be added
#' @param factor a factor between 0 and 1 that
#' @export
#' @importFrom stats runif
# @examples
# figure(data = lattice::singer) %>%
#   ly_points(catjitter(voice.part), jitter(height), color = "black") %>%
#   ly_boxplot(voice.part, height, with_outliers = FALSE)
jitter_cat <- function(x, factor = 0.5) {
  # TODO: validate factor and x
  # TODO: make sure this preserves factor ordering
  paste0(x, ":", stats::runif(min = 0.5 * (1 - factor),
    max = 0.5 + 0.5 * factor, length(x)))
}

# nms is an optional vector of names to subset results to
get_specified_args <- function(nms = NULL, nnms = NULL) {

  res <- rlang::lang_args(match.call(
    def = sys.function(-1),
    call = sys.call(-1)
  ))

  if (!is.null(nms) && is.character(nms)) {
    res <- res[intersect(names(res), nms)]
  } else if (!is.null(nnms) && is.character(nnms)) {
    res <- res[setdiff(names(res), nnms)]
  }

  lapply(res, rlang::eval_tidy)
}

# get the name of every argument that can be specified to a model's constructor
get_init_formals <- function(cls) {
  names(formals(cls$public_methods$initialize))
}

# for a given model and list of arguments, return a reduced list of valid ones
restrict_to_valid_args <- function(model, args) {
  # TODO: should we message when args aren't used?
  nms <- get_init_formals(model)
  args[intersect(nms, names(args))]
}

call_with_valid_args <- function(model, args) {
  args <- restrict_to_valid_args(model, args)
  do.call(model$new, args)
}

get_bk_props_recurse <- function(mod) {
  res <- list()
  ii <- 1
  while (mod != "Base") {
    res[[ii]] <- bk_prop_types[[mod]]
    mod <- as.character(utils::getFromNamespace(mod, "rbokeh")$inherit)
    ii <- ii + 1
  }
  unlist(res, recursive = FALSE)
}

get_can_be_vector <- function(mod) {
  types <- get_bk_props_recurse(mod)
  can_be_vector <- sapply(types, function(x) grepl("'field'|DistanceSpec|AngleSpec|NumberSpec", x$type))
  can_be_vector <- names(can_be_vector[can_be_vector])
  if (any(grepl("fill_color|line_color", can_be_vector)))
    can_be_vector <- c(can_be_vector, "color")
  if (any(grepl("fill_alpha|line_alpha", can_be_vector)))
    can_be_vector <- c(can_be_vector, "alpha")
  if (any(grepl("line_width", can_be_vector)))
    can_be_vector <- c(can_be_vector, "width")

  can_be_vector <- c(
    can_be_vector,
    paste0("hov_", can_be_vector)
  )

  if (mod == "Line")
    can_be_vector <- setdiff(can_be_vector, c("color", "line_color",
      "alpha", "line_alpha", "line_width"))

  can_be_vector
}

# a <- unlist(bk_prop_types, recursive = FALSE)
# a <- lapply(a, function(x) x$type)
# a[grepl("fill_color", names(a))]
# # all fill colors can be field
# a[grepl("fill_alpha", names(a))]
# # all fill alphas can be fields
# a[grepl("line_color", names(a))]
# # all line colors can be fields
# a[grepl("line_alpha", names(a))]
# # all line alphas can be fields
# a[grepl("line_width", names(a))]
# # all line widths except for CrosshairTool can be fields


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
  if (is.character(a) || is.factor(a) || inherits(a, "bk_cat")) {
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
    if (is.factor(vals)) {
      vals <- levels(vals)
    } else if (inherits(vals, "bk_cat")) {
      lvls <- attr(vals, "levels")
      if (!is.null(lvls)) {
        vals <- lvls
      } else {
        vals <- unique(sapply(vals, "[[", 1))
      }
    } else {
      vals <- unique(vals)
    }

    # vals <- unique(gsub("(.*):(-*[0-9]*\\.*)*([0-9]+)*$", "\\1", vals))
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
