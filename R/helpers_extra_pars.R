handle_extra_pars <- function(pars, map) {
  for(nm in names(pars)) {
    cur_map <- map[[nm]]
    if(!is.null(cur_map)) {
      cur_fn <- par_type_validate_fns[[cur_map]]
      pars[nm] <- list(cur_fn(nm, pars[[nm]]))
    }
  }

  idx <- names(pars) %in% names(map)
  if(length(which(idx)) > 0) {
    pars <- pars[idx]
  } else {
    pars <- NULL
  }

  pars
}

par_type_validate_fns <- list(
  legend_location = function(name, val) {
    validate_enum(name, val, c("top_right", "top_left", "bottom_left", "bottom_right"))
  },
  line_cap = function(name, val) {
    validate_enum(name, val, c("butt", "round", "square"))
  },
  line_join = function(name, val) {
    validate_enum(name, val, c("miter", "round", "bevel"))
  },
  label_orientation = function(name, val) {
    if(is.numeric(val))
      return(val * pi / 180) # convert from degrees to radians
    validate_enum(name, val, c("horizontal", "vertical"))
  },
  text_baseline = function(name, val) {
    validate_enum(name, val, c("top", "middle", "bottom", "alphabetic", "hanging"))
  },
  text_align = function(name, val) {
    validate_enum(name, val, c("left", "right", "center"))
  },
  font_style = function(name, val) {
    validate_enum(name, val, c("normal", "italic", "bold"))
  },
  toolbar_location = function(name, val) {
    validate_enum(name, val, c("above", "below", "left", "right"), null_ok = TRUE)
  },
  logo = function(name, val) {
    validate_enum(name, val, c("normal", "grey"), null_ok = TRUE)
  },
  color = function(name, val) {
    if(!valid_color(val))
      stop(name, " must be a hex code with no alpha or a valid css color", call. = FALSE)
    val
  },
  string = function(name, val) {
    if(!is.character(val))
      val <- as.character(val)
    val
  },
  logical = function(name, val) {
    if(!is.logical(val))
      val <- suppressWarnings(as.logical(val))
    if(is.na(val))
      stop(name, " must be a TRUE/FALSE or coercible to TRUE/FALSE", call. = FALSE)
    val
  },
  int = function(name, val) {
    if(!is.numeric(val))
      val <- suppressWarnings(as.numeric(val))
    if(is.na(val))
      stop(name, " must be an integer or coercible to an integer", call. = FALSE)
    as.integer(val)
  },
  font_size_string = function(name, val) {
    if(!is.character(val))
      val <- as.character(val)
    ## make sure its format is something like "12pt"
    pt_strip <- suppressWarnings(as.integer(gsub("pt", "", val)))
    if(is.na(pt_strip))
      stop(name, " must be a string such as '12pt'", call. = FALSE)
    val
  },
  num_data_spec = function(name, val) {
    if(!is.numeric(val))
      val <- suppressWarnings(as.numeric(val))
    if(is.na(val))
      stop(name, " must be numeric", call. = FALSE)

    list(units = "data", value = val)
  },
  line_dash = function(name, val) {
    is.numeric(val)
      val <- as.character(val)

    if(!val %in% names(lty_dict))
      stop(name, " should be one of: ", paste(names(lty_dict), collapse = ", "), call. = FALSE)

    lty_dict[[val]]
  },
  datetime_format = function(name, val) {
    if (!is.list(val) || is.null(names(val)))
      stop(name, " must be a named list", call. = FALSE)

    vals = c("microseconds", "milliseconds", "seconds", "minsec", "minutes",
             "hourmin", "hours", "days", "months", "years")
    if (any(!(names(val) %in% vals)))
      stop(name, " must be in: ('", paste(vals, collapse = "', '"), "')", call. = FALSE)

    jsonlite::toJSON(val)
  }
)

validate_enum <- function(name, val, vals, null_ok = FALSE) {
  if(is.null(val)) {
    if(null_ok) {
      return(NULL)
    } else {
      val <- "__NULL__"
    }
  }
  null_str <- ""
  if(null_ok)
    null_str <- " or NULL"
  if(!is.character(val))
    val <- as.character(val)
  if(!val %in% vals)
    stop(name, " must be in: ('", paste(vals, collapse = "', '"), "')", null_str, call. = FALSE)

  val
}
