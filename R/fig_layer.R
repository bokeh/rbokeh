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

## these are all internal functions called from make_glyph
## which is called from the various layer functions

add_layer <- function(fig, spec, dat, lname, lgroup) {
  glyph <- spec$glyph
  glyph <- underscore2camel(glyph)
  spec$glyph <- NULL

  glyph_attrs <- get_glyph_attrs(spec, dat)

  if(glyph == "Image") {
    c_id <- gen_id(fig, "ColorMapper")
    cmap <- color_mapper_model(c_id, palette = dat$palette)
    glyph_attrs$color_mapper <- cmap$ref
    fig$x$spec$model[[c_id]] <- cmap$model
  }

  gl_id <- gen_id(fig, c(glyph, lgroup, lname))
  glyph_mod <- glyph_model(gl_id, glyph, glyph_attrs)

  # nonselection glyph
  ns_glyph_attrs <- glyph_attrs
  color_ind <- which(grepl("_color", names(ns_glyph_attrs)))

  for(ii in color_ind) {
    if(!is.null(ns_glyph_attrs[[ii]])) {
      names(ns_glyph_attrs[[ii]])[names(ns_glyph_attrs[[ii]]) == "field"] <- "value"
      if(!is.null(ns_glyph_attrs[[ii]]$value))
        if(!is.na(ns_glyph_attrs[[ii]]$value))
          ns_glyph_attrs[[ii]]$value <- "#e1e1e1"
    }
  }
  nsgl_id <- gen_id(fig, c("ns", glyph, lgroup, lname))
  ns_glyph_mod <- glyph_model(nsgl_id, glyph, ns_glyph_attrs)

  # hover glyph
  hov_glyph_attrs <- glyph_attrs
  alpha_ind <- which(grepl("_alpha", names(hov_glyph_attrs)))

  for(ii in alpha_ind) {
    if(!is.null(hov_glyph_attrs[[ii]])) {
      names(hov_glyph_attrs[[ii]])[names(hov_glyph_attrs[[ii]]) == "field"] <- "value"
      if(!is.null(hov_glyph_attrs[[ii]]$value))
        if(!is.na(hov_glyph_attrs[[ii]]$value))
          hov_glyph_attrs[[ii]]$value <- 1
    }
  }
  hovgl_id <- gen_id(fig, c("hov", glyph, lgroup, lname))
  hov_glyph_mod <- glyph_model(hovgl_id, glyph, hov_glyph_attrs)

  d_id <- gen_id(fig, digest(dat))
  dat_mod <- data_model(dat, d_id)

  glr_id <- gen_id(fig, c("glyph_renderer", lgroup, lname))
  glyph_rend_mod <- glyph_renderer_model(glr_id, dat_mod$ref, glyph_mod$ref,
    ns_glyph_mod$ref, hov_glyph_mod$ref)

  fig$x$spec$model$plot$attributes$renderers[[glr_id]] <- glyph_rend_mod$ref

  fig$x$spec$model[[d_id]] <- dat_mod$model
  fig$x$spec$model[[gl_id]] <- glyph_mod$model
  fig$x$spec$model[[nsgl_id]] <- ns_glyph_mod$model
  fig$x$spec$model[[hovgl_id]] <- hov_glyph_mod$model
  fig$x$spec$model[[glr_id]] <- glyph_rend_mod$model

  # add in mappings from lname to refs for use in custom js callbacks
  # but ignore legend glyphs
  if(!grepl("^__legend", lgroup) && length(lname) == 1) {
    if(is.null(fig$x$spec$callback$layers))
      fig$x$spec$callback$layers <- list()

    cb <- list(
      glyph_mod$ref,
      ns_glyph_mod$ref,
      hov_glyph_mod$ref,
      dat_mod$ref,
      glyph_rend_mod$ref
    )
    names(cb) <- paste(lname, c("glyph", "ns_glyph", "hov_glyph",
      "data", "glyph_rend"), sep = "_")

    fig$x$spec$callback$layers[[lname]] <- cb
  }

  fig
}

get_glyph_attrs <- function(spec, dat = NULL) {
  spec_names <- names(spec)
  spec_in_data <- spec_names %in% names(dat)

  glyph_attrs <- lapply(seq_along(spec_in_data), function(ii) {
    units <- "data"
    if(grepl("angle$", spec_names[ii]))
      units <- "rad"

    if(length(spec[[ii]]) != 0) {
      if(all(is.na(spec[[ii]]))) {
        return(NULL)
      } else {
        if(spec_in_data[ii]) {
          list(units = units, field = spec[[ii]])
        } else {
          list(units = units, value = spec[[ii]])
        }
      }
    } else {
      return(NULL)
    }
  })
  names(glyph_attrs) <- names(spec)

  if(!is.null(glyph_attrs$size))
    glyph_attrs$size$units <- "screen"

  if(!is.null(glyph_attrs$line_dash))
    glyph_attrs$line_dash <- glyph_attrs$line_dash$value

  if(!is.null(glyph_attrs$anchor))
    glyph_attrs$anchor <- glyph_attrs$anchor$value

  if(!is.null(glyph_attrs$dilate))
    glyph_attrs$dilate <- glyph_attrs$dilate$value

  if(!is.null(glyph_attrs$text))
    glyph_attrs$text$field <- glyph_attrs$text$field$field

  if(!is.null(glyph_attrs$visible))
    glyph_attrs$visible <- glyph_attrs$visible$value

  glyph_attrs
}

data_model <- function(dd, id = NULL) {
  res <- base_model_object("ColumnDataSource", id)

  res$model$attributes$column_names <- I(names(dd))
  res$model$attributes$selected <- I(NULL)
  res$model$attributes$discrete_ranges <- structure(list(), .Names = character(0))
  res$model$attributes$cont_ranges <- structure(list(), .Names = character(0))
  res$model$attributes$data <- as.list(dd)

  res
}

glyph_model <- function(id, glyph = "Circle", attrs) {
  res <- base_model_object(glyph, id)
  res$model$attributes <- c(
    res$model$attributes, attrs)
  res
}

glyph_renderer_model <- function(id, data_ref, glyph_ref,
  ns_glyph_ref, hov_glyph_ref) {

  res <- base_model_object("GlyphRenderer", id)
  res$model$attributes["selection_glyph"] <- list(NULL)
  res$model$attributes$nonselection_glyph <- ns_glyph_ref
  res$model$attributes["hover_glyph"] <- list(NULL)
  res$model$attributes$hover_glyph <- hov_glyph_ref
  res$model$attributes["server_data_source"] <- list(NULL)
  res$model$attributes["name"] <- list(NULL)
  res$model$attributes$data_source <- data_ref
  res$model$attributes$glyph <- glyph_ref
  res
}

color_mapper_model <- function(id, palette = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")) {
  res <- base_model_object("LinearColorMapper", id)
  res$model$attributes$palette <- palette
  res
}
