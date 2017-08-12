##
##---------------------------------------------------------

get_mod_json <- function(json_path) {
  mods <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

  mods$Model$base <- list("Base")
  mods$Model$props
  id_props <- list(name = "id", type = "String", desc = "id", default = NULL)
  mods$Base <- list(
    name = "Base",
    base = list(NULL),
    desc = "Base class"
  )
  mods$AbstractButton$bases <- mods$AbstractButton$bases[1]
  mods$ButtonGroup$base <- mods$ButtonGroup$base[1]
  for (ii in seq_along(mods))
    mods[[ii]]$props <- c(mods[[ii]]$props, list(id_props))

  # all(sapply(mods, function(x) length(x$base)) == 1)
  # sort(sapply(mods, function(x) length(x$base)))
  mods
}

get_class_string <- function(class_name, obj) {
  a <- obj[[class_name]]
  cls <- a$name
  base <- get_inherit(a$base)

  base_attrs <- get_attrs(obj, base)
  attrs <- get_attrs(obj, cls)
  this_nms <- setdiff(names(attrs), names(base_attrs)) # attrs not in base

  desc <- paste_nice(a$desc, simplify = FALSE)[[1]]
  desc <- paste("#", desc, collapse = "\n")

  res <- paste0(desc, "\n", cls, ' <- R6::R6Class("', cls, '",',
  ifelse(is.null(base), "", paste0("\n  inherit = ", base, ",")), '
  public = list(
    specified_args = NULL,
    initialize = function(\n',
      paste_nice(get_default_string(attrs), indent = 6), '
    ) {\n',
      paste_nice(get_super_string(base_attrs), indent = 6, exdent = 8),
      '\n',
      get_initialize_string(cls),
      '
      self$specified_args <- get_specified_arg_names(match.call())
    }
  ),
  private = list(\n', get_private_string(attrs[this_nms]), '
  )
)

')

  res
}

# get_test_string <- function(a, obj) {
#   # if there are any instance types, we need to specify them...
#   all_attrs <- get_attrs(obj, a$name, dflt = a$dflt, recurse = TRUE)
#   types <- unlist(lapply(all_attrs, function(x) x$type))
#   idx <- which(grepl("^Instance", types))
#   extra_args <- ""
#   if (length(idx) > 0) {
#     extra_args <- paste0(", ", paste(unlist(lapply(all_attrs[idx], function(x) {
#       paste0(x$name, " = ", init_default_check(a$dflt[[x$name]], x$type))
#     })), collapse = ", "))
#   }

#   paste0(
# 'test_that("\'', a$name, '\' model matches prototype", {
#   obj <- ', a$name, '$new(id = "', a$dflt$id, '"', extra_args, ')
#   jsn <- as.character(obj$to_json(include_defaults = TRUE, pretty = TRUE))
#   expect_identical(jsn,\n\'', fixup(strip_white(a$json)), '\')
# })

# ')
# }

##
##---------------------------------------------------------

get_inherit <- function(x) {
  x <- unlist(x)
  if (is.null(x) || length(x) == 0)
    return(NULL)
  unname(sapply(strsplit(x, ",")[[1]], function(y) {
    tail(strsplit(y, "\\.")[[1]], 1)
  }))
}

get_attrs <- function(obj, name) {
  if (is.null(name))
    return(NULL)
  a <- obj[[name]]
  attrs <- a$props

  if (length(attrs) == 0) {
    attrs <- NULL
  } else {
    names(attrs) <- sapply(attrs, function(x) x$name)
  }

  attrs
}

get_default_string <- function(attrs) {
  if (is.null(attrs))
    return("")

  dflts <- sapply(attrs, function(atr)
    paste0(atr$name, "=", init_default(atr$default, atr$type)))

  paste(dflts, collapse = ", ")
}

init_default <- function(val, type) {
  if (is.null(val))
    return("NULL")

  if (!is.null(type) && grepl("^Instance", type))
    return("NULL")

  return (clean_dput(val))
}

get_super_string <- function(attrs) {
  if (is.null(attrs))
    return("\nsuper$initialize()")
  paste0("\nsuper$initialize(", paste0(names(attrs), "=", names(attrs), collapse = ", "), ")")
}

get_initialize_string <- function(cls) {
  paste(
  paste0(
  "      types <- bk_prop_types[[\"", cls, "\"]]"),
  "      for (nm in names(types)) {",
  "        private[[nm]] <- validate(get(nm), types[[nm]]$type, nm)",
  "      }"
  , sep = "\n")
}

get_private_string <- function(attrs) {
  if (length(attrs) == 0)
    return ("")
  last_attr <- attrs[[length(attrs)]]$name
  res <- unlist(
    lapply(attrs, function(atr) {
      res <- paste_nice(atr$desc, simplify = FALSE)[[1]]
      res <- paste("    #", res, collapse = "\n")
      res <- paste0(res, "\n    # > ", atr$type)
      res <- c(res, paste0("    ", atr$name, " = NULL",
        ifelse(atr$name == last_attr, "", ",")))
      paste(res, collapse = "\n")
    }
  ))
  paste(res, collapse = "\n")
}

clean_dput <- function(x) {
  tmp <- paste(capture.output(dput(x)), collapse = "")
  if (grepl("^structure\\(list", tmp) && !grepl("character\\(0\\)", tmp)) {
    tmp <- gsub("^structure\\(", "", tmp)
    tmp <- gsub("(.*), \\.Names.*", "\\1", tmp)
  }
  tmp
}

init_default_check <- function(val, type) {
  if (is.null(val))
    return("NULL")

  return (clean_dput(val))
}

paste_nice <- function(..., indent = 0, exdent = NULL, simplify = TRUE) {
  if (is.null(exdent))
    exdent <- indent
  wr <- strwrap(paste0(...), indent = indent, exdent = exdent, simplify = simplify, width = 72)
  if (!simplify)
    return (wr)
  res <- paste0(wr, collapse = "\n")
  # add spaces around equals
  gsub("([^ ])=([^ ])", "\\1 = \\2", res)
}

