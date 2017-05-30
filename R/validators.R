validate <- function(x, type, name) {

  # Completed:
  # "Bool"
  # "Float"
  # "Int"
  # "String"
  # "Seq ("
  # "Enum ("
  # "List ("
  # "Tuple ("
  # "Any"
  # "Instance ("
  # "Dict ( String"
  # "Color"
  # "Percent"
  # "Angle"
  # "JSON"
  # "StringSpec"
  # "ScreenDistanceSpec"
  # "NumberSpec"
  # "FontSizeSpec"
  # "DistanceSpec"
  # "AngleSpec"
  # "ColorSpec"
  # "TitleProp"

  # Need to do:
  # "RelativeDelta ( "
  # "MinMaxBounds"
  # "Either (" # ignore "Auto"
  # "DashPattern"
  # "ColumnData ( String , Seq ( Any  ) )"

  if (type == "Auto")
    type <- "String"

  if (type == "DashPattern")
    type <- "Seq(Int)"

  ttrns <- list(String = "character", Bool = "logical", Int = "numeric",
    Float = "numeric", Date = "numeric", Color = "character",
    Percent = "numeric", Angle = "numeric", JSON = "character")
  ttrnsnm <- names(ttrns)

  # if scalar
  if (type %in% ttrnsnm) {
    return (validate_scalar(x, ttrns[[type]], type, name))
  }

  if (grepl("^Tuple\\(", type)) {
    if (is.null(x))
      return(NULL)
    if (length(x) != 2)
      stop("Attribute '", name, "' must be a vector of length 2.", call. = FALSE)
    subtype <- strip_white(gsub("^Tuple\\((.*)\\)$", "\\1", type))
    if (subtype %in% c("Date , Date", "Float , Float")) {
      if (is.list(x) && all(sapply(x, length) == 1))
        x <- unlist(x)
      if (!is.numeric(x))
        stop("Attribute '", name, "' must be a numeric vector of length 2", call. = FALSE)
      return (validate_vector(x, ttrns$Float, "Float", name))
    }
  }

  if (grepl("^Seq", type)) {
    # Seq can be both [] (list()) and null (NULL)
    if (is.null(x))
      return(NULL)
    if (length(x) == 0)
      return(list())
    type <- gsub("^Seq\\(([A-Za-z]*)\\)$", "\\1", type)
    if (type %in% ttrnsnm) {
      return (validate_vector(x, ttrns[[type]], type, name))
    }
  }

  if (grepl("^List\\(", type)) {
    if (is.null(x))
      return(NULL)
    if (length(x) == 0)
      return(list())
    # List with scalar types and Seq with scalar types are the same
    subtype <- strip_white(gsub("^List\\((.*)\\)$", "\\1", type))
    if (subtype %in% ttrnsnm) {
      return (validate_vector(unname(x), ttrns[[subtype]], subtype, name))
    } else {
      if (!is.list(x))
        x <- as.list(x)
      x <- validate_list(x, named = FALSE, name = name)
      return (lapply(x, function(a) validate(a, subtype)))
    }
  }

  if (grepl("^Enum", type)) {
    # some Enums can be "None" which is null in javascript (how to tell which ones allow none?)
    if (is.null(x) || is.na(x))
      return(NULL)
    type <- get_enum_type(type)
    return (validate_enum(x, type, name))
  }

  # Instance is either NULL or list(id = string, type = string)
  if (grepl("^Instance", type) || type == "TitleProp") {
    if (is.null(x) || is.na(x))
      return(NULL)
    nms <- names(x)
    if (is.list(x) && "id" %in% nms && "type" %in% nms &&
      length(x$id) == 1 && length(x$type == 1)) {

      return (list(
        id = as.character(x$id),
        type = as.character(x$type)
      ))
    } else {
      stop("Attribute '", name, "' must be NULL or a list with 'id' and 'type'", call. = FALSE)
    }
  }

  if (grepl("^Dict\\(String, ", type)) {
    subtype <- strip_white(gsub("^Dict\\(String, (.*) \\)", "\\1", type))
    x <- validate_list(x, named = TRUE, name)
    # validate subtypes
    return (lapply(x, function(a) validate(a, subtype)))
  }

  # Specs are lists of scalars
  if (grepl("Spec\\(", type)) {
    # At some point, we can check specific attributes of each of these
    #   StringSpec
    #   ScreenDistanceSpec
    #   NumberSpec
    #   FontSizeSpec
    #   DistanceSpec
    #   AngleSpec
    #   ColorSpec
    if (is.null(x) || is.na(x))
      return(NULL)

    if (is.atomic(x))
      x <- list(value = x)

    x <- validate_list(x, named = TRUE, name)
    if (any(unlist(lapply(x, length)) > 1))
      stop("Attribute '", name, "' must a named list of scalars", call. = FALSE)
    if (any(unlist(lapply(x, function(a) !inherits(a, c("character", "numeric")))) > 1))
      stop("Attribute '", name, "' must a named list of scalars that are character or numeric",
        call. = FALSE)

    return(x)
  }

  if (grepl("^Either", type)) {
    type <- strip_white(gsub("^Either\\((.*)\\)$", "\\1", type))
    # the regex below splits on commas not inside parens
    types <- strip_white(strsplit(type, ",(?![^()]*+\\))", perl = TRUE)[[1]])

    for (tp in types) {
      tmp <- try(validate(x, tp), silent = TRUE)
      if (!inherits(tmp, "try-error")) {
        x <- tmp
        return(x)
      }
    }
  }

  if (type == "Any")
    return(x)

  # all others
  message("Note: could not find validator for type '", type, "'... Returning as is.")
  return(x)

  # dflt_res$StringSpec[[1]]
  # dflt_res$ScreenDistanceSpec[[1]]
  # dflt_res$NumberSpec[[1]]
  # dflt_res$FontSizeSpec[[1]]
  # dflt_res$DistanceSpec[[1]]
  # dflt_res$AngleSpec[[1]]
  # dflt_res$ColorSpec[[1]]

  # dflt_res$MinMaxBounds[[1]]
  # dflt_res$TitleProp[[1]]
  # dflt_res$Angle[[1]]
  # dflt_res$Percent[[1]]
  # dflt_res$Color[[1]]
  # dflt_res$DashPattern[[1]]
  # dflt_res$Date[[1]]
  # dflt_res$JSON[[1]]
  # dflt_res[["ColumnData ( String , Seq ( Any  ) )"]][[1]]
}


emnlist <- function()
  structure(list(), .Names = character(0))

strip_white <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

#' @importFrom methods as
validate_scalar <- function(x, type = "character", otype = "", name) {
  if (length(x) > 1)
    stop("Attribute '", name, "' must be a scalar.", call. = FALSE)
  # ensure NA gets populated as null (unbox wants a "character" NA for this)
  if (length(x) == 1 && is.na(x))
    return(NULL)
  if (is.null(x) || length(x) == 0)
    return(NULL)
  methods::as(x, type)
}

validate_vector <- function(x, type = "character", otype = "", name) {
  # if it's NA, return null
  if (length(x) == 1 && is.na(x))
    return (NULL)
  if (is.null(x) || length(x) == 0)
    return(NULL)
  if (!is.vector(x))
    stop("Attribute '", name, "' must be a vector.", call. = FALSE)
  as.list(methods::as(x, type))
}

# TODO: add additional validation for scalar and vector for otype Color and Percent

validate_enum <- function(x, type, name) {
  if (!x %in% type)
    stop("Attribute '", name, "' with value '", x, "' must be one of ",
      paste(type, collapse = ", "), call. = FALSE)
  x
}

validate_list <- function(x, named = FALSE, name) {
  if (is.null(x) || is.na(x) || length(x) == 0) {
    if (named) {
      return (structure(list(), .Names = character(0)))
    } else {
      return (list())
    }
  }
  if (!is.list(x))
    stop("Attribute '", name, "' must be a list", call. = FALSE)
  if (named && is.null(names(x)))
    stop("Attribute '", name, "' must be a named list", call. = FALSE)
  if (!named && !is.null(names(x)))
    names(x) <- NULL
  x
}

validate_list_class <- function(x, class, named = FALSE, name) {
  if (!is.list(x) && !all(sapply(x, function(a) inherits(a, class))))
    stop("Attribute '", name, "' must be a list with all elements of class ", class,
      call. = FALSE)
  if (named && is.null(names(x)))
    stop("Attribute '", name, "' must be a named list", call. = FALSE)
  if (!named && !is.null(names(x)))
    names(x) <- NULL
  x
}

get_enum_type <- function(x) {
  x <- gsub("^Enum\\((.*)\\)$", "\\1", x)
  x <- gsub("'", "", x)
  strsplit(x, ", ")[[1]]
}
