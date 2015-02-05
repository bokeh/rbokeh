# glyphs are stored in layers
# layers have names (lname) and group names (lgroup)
# you can add new layers to an existing layer group
# which will ensure that aesthetics are mapped to glyphs within the layer

make_glyph <- function(fig, type, lname, lgroup, data, args, axis_type_range, hover = NULL, legend = NULL, xname = NULL, yname = NULL, data_sig = NA) {

  ## give it a unique layer group name if not provided
  if(is.null(lgroup))
    lgroup <- gen_layer_name(names(fig$layers))
  lgroup <- as.character(lgroup)

  fig$layers[[lgroup]]$lgroup <- lgroup

  ## give it a unique layer name if not provided
  if(is.null(lname))
    lname <- gen_layer_name(names(fig$layers[[lgroup]]$glyph_ids), prefix = "layer")
  lname <- as.character(lname)

  ## some figure elements need a single index to a layer name/group combination
  ## such as glyph_defer and glyph_x_ranges / glyph_y_ranges
  lgn <- paste(lgroup, lname, sep = "_")

  ## every layer has an associated glyph_renderer
  ## whose id is generated from the layer name and group
  glr_id <- gen_id(fig, c("glyph_renderer", lgroup, lname))

  ## used to keep track of how many layers are in the group
  fig$layers[[lgroup]]$glyph_ids[lname] <- list(glr_id)

  ## keep track of data sources
  if(is.na(data_sig))
    data_sig <- NULL
  fig$data_sigs[[glr_id]] <- list(glr_id = glr_id, sig = data_sig)

  ## make sure axis types match anything
  ## that has already been plotted
  validate_axis_type(fig_type = fig$x_axis_type,
    cur_type = axis_type_range$x_axis_type, which = "x")
  validate_axis_type(fig_type = fig$y_axis_type,
    cur_type = axis_type_range$y_axis_type, which = "y")

  fig$x_axis_type <- axis_type_range$x_axis_type
  fig$y_axis_type <- axis_type_range$y_axis_type

  ## make sure specified colors are bokeh-valid hex codes (if they are hex codes)
  ## only to ones that don't need to be mapped
  is_mapped <- sapply(args, function(x) !is.null(attr(x, "nseName")))
  mapped_args <- names(args)[is_mapped]
  ind <- setdiff(names(args), mapped_args)
  args[ind] <- validate_colors(args[ind])

  ## deal with aesthetic inputs that are to be mapped
  ## e.g. fill_color might be mapped to a variable in the data
  ## in which case we need to track its domain
  ## and its range will be filled in from a theme
  ## when the figure is printed
  aes_maps <- get_aes_maps(args, glr_id)

  ## deal with manual legend
  if(!is.null(legend)) {
    legend <- as.character(legend)
    if(!is.null(aes_maps)) {
      if(legend == "FALSE") {
        fig$layers[[lgroup]]$do_legend <- FALSE
      } else {
        message("Ignoring custom legend because an aesthetic is being mapped and therefore the legend is being taken care of automatically.")
      }
    } else {
      if(!is.null(fig$common_legend[[legend]])) {
        fig$common_legend[[legend]]$args <- c(fig$common_legend[[legend]]$args, list(args))
      } else {
        fig$common_legend[[legend]] <- list(name = legend, args = list(args))
      }
    }
  }

  ## merge in aesthetic mappings (if any)
  fig$layers[[lgroup]]$maps <- merge_aes_maps(fig$layers[[lgroup]]$maps, aes_maps)

  ## save defer function (if any) and remove from data
  if(!is.null(data$defer)) {
    fig$glyph_defer[[lgn]] <- list(fn = data$defer)
    data$defer <- NULL
  }

  ## validate the spec args
  # validate_opts(opts, type)

  ## move all data scalars over to the spec
  data_lengths <- sapply(data, length)
  data_is_list <- sapply(data, is.list)
  data_names <- names(data)
  scalar_ind <- which(data_lengths == 1 & !data_is_list)
  for(ii in scalar_ind)
    args[[data_names[ii]]] <- data[[ii]]
  data[scalar_ind] <- NULL

  ## move all non-scalar args over to data
  ## except for "line_dash"
  arg_lengths <- sapply(args, length)
  arg_names <- names(args)
  long_ind <- which(arg_lengths > 1 & arg_names != "line_dash")
  for(ii in long_ind) {
    data[[arg_names[ii]]] <- args[[ii]]
  }
  args[long_ind] <- NULL

  ## remove NAs in data at this point?

  ## spec needs to point to corresponding data
  data_names <- names(data)
  for(nm in data_names)
    args[[nm]] <- nm

  ## data must have something in it or it won't work
  if(length(data) == 0)
    data <- list(dummy = list(1))

  ## fix spec for "text" glyph
  if("text" %in% names(args)) {
     args$text <- list(field = "text")
  }

  if(!is.null(fig$glyph_defer[[lgn]])) {
    fig$glyph_defer[[lgn]]$spec <- args
    fig$glyph_defer[[lgn]]$data <- data
    fig$glyph_defer[[lgn]]$lgroup <- lgroup
    fig$glyph_defer[[lgn]]$lname <- lname
  }

  ## add hover info
  if(!is.null(hover)) {
    renderer_ref <- list(
      type = "GlyphRenderer",
      id = glr_id
    )

    fig <- fig %>% add_hover(hover$dict, renderer_ref)
    data <- c(data, hover$data)
  }

  args$glyph <- type

  if(axis_type_range$x_axis_type == "datetime") {
    axis_type_range$x_range <- to_epoch(axis_type_range$x_range)
    if(!is.null(data$x))
      data$x <- to_epoch(data$x)
    if(!is.null(data$x0))
      data$x0 <- to_epoch(data$x0)
    if(!is.null(data$x1))
      data$x1 <- to_epoch(data$x1)
  }

  if(axis_type_range$y_axis_type == "datetime") {
    axis_type_range$y_range <- to_epoch(axis_type_range$y_range)
    if(!is.null(data$y))
      data$y <- to_epoch(data$y)
    if(!is.null(data$y0))
      data$y0 <- to_epoch(data$y0)
    if(!is.null(data$y1))
      data$y1 <- to_epoch(data$y1)
  }

  fig <- fig %>% add_layer(args, data, lname, lgroup)

  ## add x and y range for this glyph
  fig$glyph_x_ranges[[lgn]] <- axis_type_range$x_range
  fig$glyph_y_ranges[[lgn]] <- axis_type_range$y_range

  ## add x and y labels if missing
  if(is.null(fig$xlab) && length(xname) > 0)
    fig$xlab <- xname

  if(is.null(fig$ylab) && length(yname) > 0)
    fig$ylab <- yname

  fig
}
