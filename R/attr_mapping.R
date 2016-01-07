# __layer_attr_map__
# -> var1
#   -> domain (vector of characters or numeric range)
#   -> legend_glyphs (list of attrLegendGlyph objects)
#   -> map_entries (list of attrMapEntry objects)
# -> var2

# __attrLegendGlyph__
# -> name (name of the glyph this map applies to)
# -> map_args (vector of glyph attribute names that need to be mapped)
# -> args (arguments that do not need to be mapped - used to create legend glyphs)

# __attrMapEntry__
# -> id (id of glyph_renderer that needs its variables updated)
# -> map_args (vector of glyph attribute names that need to be mapped)

get_attr_maps <- function(args, glr_id) {
  nms <- names(args)
  ## get an index of arguments that need a map
  ## name attribute is used in the legend so we know what variable created the map
  mappable <- sapply(args, function(x) !is.null(attr(x, "mapped")))

  if(length(mappable) > 0) {
    if(length(which(mappable)) == 0)
      return(NULL)

    # build an attrMap object with an entry for each unique name
    # entry has the name, domain
    arg_names <- as.character(sapply(args[mappable], function(x) attr(x, "name")))
    u_arg_names <- unique(arg_names)

    layer_attr_map <- structure(vector("list",
      length = length(u_arg_names)), names = u_arg_names)

    for(nm in u_arg_names) {
      ## args we need to map
      margs <- args[mappable][arg_names == nm]
      ## args we need to maintain for legend glyphs
      ## -- for now the assumption is these will be scalar
      ## -- (assume all args are either mapped or scalar)
      ## -- but should relax this by using idx when building "entries" below
      gargs <- lapply(args[!mappable], function(x) x[1])

      ## some args need to be overridden based on the glyph type
      gargs <- override_legend_glyph_args(gargs)

      dmn <- get_domain(do.call(c, lapply(margs, get_domain)))
      layer_attr_map[[nm]]$domain <- dmn

      glyph_name <- as.character(args$glyph[1])
      if(!glyph_name %in% names(glyph_props))
        glyph_name <- "mappedGlyph"

      layer_attr_map[[nm]]$legend_glyphs[[glyph_name]] <- list(name = glyph_name, args = gargs, map_args = names(margs))

      layer_attr_map[[nm]]$map_entries[[glr_id]] <- list(id = glr_id, map_args = names(margs), args = gargs)
    }
    return(layer_attr_map)
  }
  NULL
}

# merge map2 into map1
merge_attr_maps <- function(map1, map2) {
  m1n <- names(map1)
  m2n <- names(map2)
  same_var <- intersect(m1n, m2n)
  new_var <- setdiff(m2n, same_var)
  if(length(new_var) > 0) {
    map1[new_var] <- map2[new_var]
  }
  if(length(same_var) > 0) {
    for(nm in same_var) {
      ## merge the domains
      map1[[nm]]$domain <- merge_attr_domains(map1[[nm]]$domain, map2[[nm]]$domain)
      ## merge map entries
      map1[[nm]]$legend_glyphs <- merge_attr_legend_glyphs(map1[[nm]]$legend_glyphs, map2[[nm]]$legend_glyphs)
      id <- map2[[nm]]$map_entries[[1]]$id
      map1[[nm]]$map_entries[[id]] <- map2[[nm]]$map_entries[[1]]
    }
  }
  map1
}

merge_attr_legend_glyphs <- function(gly1, gly2) {
  g1n <- names(gly1)
  g2n <- names(gly2)
  same_var <- intersect(g1n, g2n)
  new_var <- setdiff(g2n, same_var)
  if(length(new_var) > 0) {
    gly1[new_var] <- gly2[new_var]
  }
  # if(length(same_var) > 0) {
  #   if(any(same_var == "mappedGlyph")) {
  #     if(!identical(gly1[[same_var]], gly2[[same_var]]))
  #       message("A layer added to an existing layer group has the same glyph mapped to a same attribute... it will be ignored.")
  #   } else {
  #     message("A layer added to an existing layer group has the same glyph mapped to a same attribute... it will be ignored.")
  #   }
  # }
  gly1
}

merge_attr_domains <- function(d1, d2) {
  if(is.null(d1))
    return(d2)
  if(is.null(d2))
    return(d1)

  if(is.character(d1)) {
    return(unique(c(d1, d2)))
  } else {
    return(range(c(d1, d2), na.rm = TRUE))
  }
}

get_domain <- function(x) {
  if(is.factor(x)) {
    return(levels(x))
  } else if(is.character(x)) {
    return(sort(unique(x)))
  } else {
    return(range(x, na.rm = TRUE))
  }
}

get_theme_value <- function(domain, values, type, theme) {
  is_discrete <- ifelse(is.numeric(domain), FALSE, TRUE)
  mode <- ifelse(is_discrete, "discrete", "continuous")
  if(is_discrete) {
    idx <- match(values, domain)
    vals <- theme[[mode]][[type]](length(domain))
  } else {
    ct <- cut(values, domain, include.lowest = TRUE)
    vals <- theme[[mode]][[type]](length(levels(ct)))
    idx <- as.integer(ct)
  }
  if(length(idx) == 1) {
    return(vals[[idx]])
  } else {
    return(vals[idx])
  }
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

# any variable with a name will be a candidate to be mapped
# but if it is a valid value, it won't be mapped
needs_map_fns <- list(
  glyph = function(dd)
    !valid_glyph(dd),
  color = function(dd)
    !valid_color(dd),
  line_color = function(dd)
    !valid_color(dd),
  fill_color = function(dd)
    !valid_color(dd),
  text_color = function(dd)
    !valid_color(dd),
  lty = function(dd)
    !valid_line(dd),
  size = function(dd)
    !(is.numeric(dd) & all(dd > 0)),
  line_dash = function(dd)
    !valid_line(dd)
)

override_legend_glyph_args <- function(args) {
  if(!is.null(args$end_angle))
    args$end_angle <- 2*pi
  if(!is.null(args$start_angle))
    args$start_angle <- 0
  args
}
