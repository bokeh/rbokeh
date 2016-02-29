# glyphs are stored in layers
# layers have names (lname) and group names (lgroup)
# you can add new layers to an existing layer group
# which will ensure that attributes are mapped to glyphs within the layer

make_glyph <- function(fig, type, lname, lgroup, data, args,
  axis_type_range, hover = NULL, url = NULL, legend = NULL,
  xname = NULL, yname = NULL, data_sig = NA, ly_call,
  dots = NULL) {

  if(is.null(args))
    args <- list()

  ## give it a unique layer group name if not provided
  if(is.null(lgroup))
    lgroup <- gen_layer_name(names(fig$x$spec$layers))
  lgroup <- as.character(lgroup)

  fig$x$spec$layers[[lgroup]]$lgroup <- lgroup

  ## give it a unique layer name if not provided
  prefix <- paste0(lgroup, "l")
  if(grepl("^group", prefix))
    prefix <- gsub("roup", "", prefix)

  if(length(lname) == 0)
    lname <- gen_layer_name(names(fig$x$spec$layers[[lgroup]]$glyph_ids), prefix = prefix)
  lname <- as.character(lname)

  ## some figure elements need a single index to a layer name/group combination
  ## such as glyph_defer and glyph_x_ranges / glyph_y_ranges
  lgn <- paste(lgroup, lname, sep = "_")

  ## every layer has an associated glyph_renderer
  ## whose id is generated from the layer name and group
  glr_id <- gen_id(fig, c("glyph_renderer", lgroup, lname))

  ## used to keep track of how many layers are in the group
  fig$x$spec$layers[[lgroup]]$glyph_ids[lname] <- list(glr_id)

  ## keep track of data sources
  if(is.na(data_sig))
    data_sig <- NULL
  fig$x$spec$data_sigs[[glr_id]] <- list(glr_id = glr_id, sig = data_sig)

  ## make sure axis types match anything
  ## that has already been plotted
  validate_axis_type(fig_type = fig$x$spec$x_axis_type,
    cur_type = axis_type_range$x_axis_type, which = "x")
  validate_axis_type(fig_type = fig$x$spec$y_axis_type,
    cur_type = axis_type_range$y_axis_type, which = "y")

  fig$x$spec$x_axis_type <- axis_type_range$x_axis_type
  fig$x$spec$y_axis_type <- axis_type_range$y_axis_type

  ## get rid of factors
  for(ii in seq_along(args)) {
    if(is.factor(args[[ii]]))
      args[[ii]] <- as.character(args[[ii]])
  }

  ## get names of data in args that were specified by user
  ## and get whether variable is mapped
  for(nm in names(args)) {
    if(!is.null(args[[nm]])) {
      arg_nm <- ly_call[[nm]][1]

      ## deal with 'color' and 'alpha' higher-level parameters
      if(is.null(arg_nm)) {
        if(nm %in% c("fill_alpha", "line_alpha"))
          arg_nm <- ly_call[["alpha"]]
        if(nm %in% c("fill_color", "line_color"))
          arg_nm <- ly_call[["color"]]
      }

      if(!is.null(arg_nm)) {
        attr(args[[nm]], "name") <- arg_nm
        ## see if named args are valid
        ## if they are not, they will be marked as mapped
        if(!is.null(needs_map_fns[[nm]]))
          if(needs_map_fns[[nm]](args[[nm]]) && !is.na(args[[nm]]))
            attr(args[[nm]], "mapped") <- TRUE
      }
    }
  }

  ## make sure specified colors are bokeh-valid hex codes (if they are hex codes)
  ## only to ones that don't need to be mapped
  is_mapped <- sapply(args, function(x) !is.null(attr(x, "mapped")))
  mapped_args <- names(args)[is_mapped]
  ind <- setdiff(names(args), mapped_args)
  args[ind] <- validate_colors(args[ind])

  ## deal with attribute inputs that are to be mapped
  ## e.g. fill_color might be mapped to a variable in the data
  ## in which case we need to track its domain
  ## and its range will be filled in from a theme
  ## when the figure is printed
  if(is.null(args$glyph))
    args$glyph <- type
  attr_maps <- get_attr_maps(args, glr_id)

  ## deal with manual legend
  if(!is.null(legend)) {
    if(!(is.logical(legend) && !legend)) {
      legend <- as.character(legend)
      if(!is.null(attr_maps)) {
        if(legend == "FALSE") {
          fig$x$spec$layers[[lgroup]]$do_legend <- FALSE
        } else {
          message("Ignoring custom legend because an attribute is being mapped and therefore the legend is being taken care of automatically.")
        }
      } else {
        if(!is.null(fig$x$spec$common_legend[[legend]])) {
          fig$x$spec$common_legend[[legend]]$args <- c(fig$x$spec$common_legend[[legend]]$args, list(args))
        } else {
          fig$x$spec$common_legend[[legend]] <- list(name = legend, args = list(args))
        }
      }
    } else {
      if(is.logical(legend))
        fig$x$spec$layers[[lgroup]]$do_legend <- legend
    }
  }

  ## merge in attribute mappings (if any)
  fig$x$spec$layers[[lgroup]]$maps <- merge_attr_maps(fig$x$spec$layers[[lgroup]]$maps, attr_maps)

  ## save defer function (if any) and remove from data
  if(!is.null(data$defer)) {
    fig$x$spec$glyph_defer[[lgn]] <- list(fn = data$defer)
    data$defer <- NULL
  }

  ## validate the spec args
  # validate_opts(opts, type)

  ## move all data scalars over to the spec
  data_lengths <- sapply(data, length)
  data_is_list <- sapply(data, is.list)
  data_names <- names(data)
  max_data_length <- max(data_lengths)
  scalar_ind <- which(data_lengths == 1 & !data_is_list & max_data_length != 1)

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

  ## data elements of length 1 must be lists (since toJSON auto_unbox = TRUE)
  length_one <- which(sapply(data, length) == 1 & !sapply(data, is.list))
  for(ii in length_one) {
    data[[ii]] <- list(data[[ii]])
  }

  # ## NAs must be changed to NaN for bokeh to be happy
  # for(ii in seq_along(data)) {
  #   if(inherits(data[[ii]], c("Date", "POSIXct")))
  #     data[[ii]] <- as.character(data[[ii]])
  #   data[[ii]][which(is.na(data[[ii]]))] <- NaN
  # }
  ## actually this causes problems with categorical and date data
  ## se we'll handle it in the js prior to rendering

  ## spec needs to point to corresponding data
  data_names <- names(data)
  for(nm in data_names)
    args[[nm]] <- nm

  ## data must have something in it or it won't work
  if(length(data) == 0)
    data <- list(dummy = list(1))

  ## fix spec for "text" glyph
  if("text" %in% names(args))
     args$text <- list(field = "text")

  if(!is.null(fig$x$spec$glyph_defer[[lgn]])) {
    fig$x$spec$glyph_defer[[lgn]]$spec <- args
    fig$x$spec$glyph_defer[[lgn]]$data <- data
    fig$x$spec$glyph_defer[[lgn]]$lgroup <- lgroup
    fig$x$spec$glyph_defer[[lgn]]$lname <- lname
  }

  renderer_ref <- list(
    type = "GlyphRenderer",
    id = glr_id
  )

  ## add hover info
  if(!is.null(hover)) {
    # convert to character and make it a list so it shows up properly
    hover$data <- lapply(hover$data, function(x) {
      if(length(x) == 1) {
        return(list(as.character(x)))
      } else {
        return(as.character(x))
      }
    })

    fig <- fig %>% add_hover(hover$dict, renderer_ref)
    data <- c(data, hover$data)
  }

  # if(!is.null(url) && !is.null(tap_callback)) {
  #   message("'url' and 'tap_callback' can't be specified simultaneously - honoring 'url'")
  #   tap_callback <- NULL
  # }

  if(!is.null(url)) {
    fig <- fig %>% add_url(url$url, renderer_ref)
    data <- c(data, url$data)
  }

  args$glyph <- type

  if(axis_type_range$x_axis_type == "datetime") {
    axis_type_range$x_range <- to_epoch(axis_type_range$x_range)
    if(!is.null(data$x))
      data$x <- handle_singleton(data$x, to_epoch)
    if(!is.null(data$x0))
      data$x0 <- handle_singleton(data$x0, to_epoch)
    if(!is.null(data$x1))
      data$x1 <- handle_singleton(data$x1, to_epoch)
    if(!is.null(data$left))
      data$left <- handle_singleton(data$left, to_epoch)
    if(!is.null(data$right))
      data$right <- handle_singleton(data$right, to_epoch)
  }

  if(axis_type_range$y_axis_type == "datetime") {
    axis_type_range$y_range <- to_epoch(axis_type_range$y_range)
    if(!is.null(data$y))
      data$y <- handle_singleton(data$y, to_epoch)
    if(!is.null(data$y0))
      data$y0 <- handle_singleton(data$y0, to_epoch)
    if(!is.null(data$y1))
      data$y1 <- handle_singleton(data$y1, to_epoch)
    if(!is.null(data$top))
      data$top <- handle_singleton(data$top, to_epoch)
    if(!is.null(data$bottom))
      data$bottom <- handle_singleton(data$bottom, to_epoch)
  }

  fig <- fig %>% add_layer(args, data, lname, lgroup)

  ## add x and y range for this glyph
  fig$x$spec$glyph_x_ranges[[lgn]] <- axis_type_range$x_range
  fig$x$spec$glyph_y_ranges[[lgn]] <- axis_type_range$y_range

  ## add x and y labels if missing
  if(is.null(fig$x$spec$xlab) && length(xname) > 0)
    fig$x$spec$xlab <- xname

  if(is.null(fig$x$spec$ylab) && length(yname) > 0)
    fig$x$spec$ylab <- yname

  fig
}
