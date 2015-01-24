# to add a layer:
# - create GlyphRenderer model
#   - create glyph model (e.g. "Circle")
#   - add glyph model to object
#   - add glyph reference as "glyph" attribute
#   - optionally create nonselection_glyph, selection_glyph attributes in similar fashion
#   - create ColumnDataSource model
#   - add ColumnDataSource model to object
#   - add ColumnDataSource reference as "data_source" attribute
# - add GlyphRenderer reference to "renderers"
# - add GlyphRenderer model to object

addLayer <- function(obj, spec, dat, lname, lgroup) {
  glyph <- spec$glyph
  glyph <- underscore2camel(glyph)
  spec$glyph <- NULL

  specInData <- names(spec) %in% names(dat)

  glyphAttrs <- lapply(seq_along(specInData), function(ii) {
    if(specInData[ii]) {
      list(units = "data", field = spec[[ii]])
    } else {
      list(units = "data", value = spec[[ii]])
    }
  })

  names(glyphAttrs) <- names(spec)
  if(!is.null(glyphAttrs$size))
    glyphAttrs$size$units <- "screen"

  if(!is.null(glyphAttrs$line_dash))
    glyphAttrs$line_dash <- glyphAttrs$line_dash$value

  if(!is.null(glyphAttrs$text))
    glyphAttrs$text$field <- glyphAttrs$text$field$field

  if(glyph == "Image") {
    cId <- genId(obj, "ColorMapper")
    cmap <- colorMapperModel(cId)
    glyphAttrs$color_mapper <- cmap$ref
    obj$model[[cId]] <- cmap$model
  }

  glId <- genId(obj, c(glyph, lgroup, lname))
  glyph <- glyphModel(glId, glyph, glyphAttrs)

  dId <- genId(obj, digest(dat))
  datMod <- dataModel(dat, dId)

  glrId <- genId(obj, c("glyphRenderer", lgroup, lname))
  glyphRend <- glyphRendererModel(glrId, datMod$ref, glyph$ref)

  obj$model$plot$attributes$renderers[[glrId]] <- glyphRend$ref

  obj$model[[dId]] <- datMod$model
  obj$model[[glId]] <- glyph$model
  obj$model[[glrId]] <- glyphRend$model

  obj
}

dataModel <- function(dd, id = NULL) {
  res <- base_model_object("ColumnDataSource", id)

  res$model$attributes$column_names <- I(names(dd))
  res$model$attributes$selected <- I(NULL)
  res$model$attributes$discrete_ranges <- structure(list(), .Names = character(0))
  res$model$attributes$cont_ranges <- structure(list(), .Names = character(0))
  res$model$attributes$data <- as.list(dd)

  res
}

glyphModel <- function(id, glyph = "Circle", attrs) {
  res <- base_model_object(glyph, id)
  res$model$attributes <- c(
    res$model$attributes, attrs)
  res
}

glyphRendererModel <- function(id, dataRef, glyphRef) {
  res <- base_model_object("GlyphRenderer", id)
  res$model$attributes["selection_glyph"] <- list(NULL)
  res$model$attributes["nonselection_glyph"] <- list(NULL)
  res$model$attributes["server_data_source"] <- list(NULL)
  res$model$attributes["name"] <- list(NULL)
  res$model$attributes$data_source <- dataRef
  res$model$attributes$glyph <- glyphRef
  res
}

colorMapperModel <- function(id) {
  res <- base_model_object("LinearColorMapper", id)
  res$model$attributes$palette <- c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")
  res
}
