#' Bare-bones line layer function
#'
#' @param obj an rbokeh object
#' @param x x values
#' @param y y values
#' @param legend optional string to specify the legend entry for this glyph
#' @param lname optional layer name
#' @export
ly_line <- function(obj, x, y = x, legend = NULL, lname = NULL) {
  cds <- ColumnDataSource$new(
    column_names = c("x", "y"),
    data = list(x = x, y = y)
  )

  glph <- Line$new(
    line_color = "#1f77b4",
    line_width = 2,
    x = "x",
    y = "y"
  )

  nsglph <- Line$new(
    line_alpha = 0.1,
    line_color = "#1f77b4",
    line_width = 2,
    x = "x",
    y = "y"
  )

  glr <- GlyphRenderer$new(
    data_source = cds$get_instance(),
    glyph = glph$get_instance(),
    nonselection_glyph = nsglph$get_instance()
  )

  lngi <- NULL
  if (!is.null(legend)) {
    if (is.character(legend) && is.atomic(legend)) {
      lngi <- LegendItem$new(label = legend,
        renderers = list(glr$get_instance())
      )
    } else {
      message("'legend' misspecified...")
    }
  }

  if (is.null(obj$x$mods$layers))
    obj$x$mods$layers <- list()

  if (is.null(lname))
    lname <- get_next_layer_name(obj)

  if (! (is.character(lname) && is.atomic(lname))) {
    message("'lname' misspecified...")
    lname <- get_next_layer_name(obj)
  }

  obj$x$mods$layers[[lname]] <- list(
    group = "gp1",
    legend = legend,
    data_source = cds,
    glyph = glph,
    ns_glyph = nsglph,
    hov_glyph = NULL,
    sel_glyph = NULL,
    mut_glyph = NULL,
    glyph_renderer = glr,
    legend_item = lngi
  )

  obj
}

get_next_layer_name <- function(obj) {
  nms <- names(obj$x$mods$layers)
  nms <- nms[grepl("^l[0-9]+", nms)]
  if (length(nms) == 0)
    return ("l1")
  val <- as.integer(gsub("l(.*)", "\\1", nms))
  paste0("l", max(val) + 1)
}

add_axes <- function(mods) {
  if (is.null(mods$axes$x)) {
    tck1 <- BasicTicker$new()

    grd1 <- Grid$new(
      plot = mods$plot$get_instance(),
      ticker = tck1$get_instance()
    )

    btf1 <- BasicTickFormatter$new()

    lax <- LinearAxis$new(
      axis_label = "x",
      formatter = btf1$get_instance(),
      plot = mods$plot$get_instance(),
      ticker = tck1$get_instance()
    )

    xrng <- DataRange1d$new()

    if (is.null(mods$axes))
      mods$axes <- list()

    mods$axes$x <- list(
      ticker = tck1,
      grid = grd1,
      tick_formatter = btf1,
      axis = lax,
      range = xrng
    )
  }

  if (is.null(mods$axes$y)) {
    tck2 <- BasicTicker$new()

    grd2 <- Grid$new(
      dimension = 1,
      plot = mods$plot$get_instance(),
      ticker = tck2$get_instance()
    )

    btf2 <- BasicTickFormatter$new()

    lay <- LinearAxis$new(
      formatter = btf2$get_instance(),
      plot = mods$plot$get_instance(),
      ticker = tck2$get_instance()
    )

    yrng <- DataRange1d$new()

    if (is.null(mods$axes))
      mods$axes <- list()

    mods$axes$y <- list(
      ticker = tck2,
      grid = grd2,
      tick_formatter = btf2,
      axis = lay,
      range = yrng
    )
  }

  mods
}

add_title <- function(mods, title = NULL) {
  if (!is.null(title)) {
    ttl <- Title$new(plot = NULL, text = "simple line example")
    mods$title <- ttl
  }

  mods
}

add_tools <- function(mods) {
  ba <- BoxAnnotation$new(
    bottom_units = "screen",
    fill_alpha = 0.5,
    fill_color = "lightgrey",
    left_units = "screen",
    level = "overlay",
    line_alpha = 1,
    line_color = "black",
    line_dash = c(4, 4),
    line_width = 2,
    render_mode = "css",
    right_units = "screen",
    top_units = "screen"
  )

  bzt <- BoxZoomTool$new(
    overlay = ba$get_instance(),
    plot = mods$plot$get_instance()
  )

  rst <- ResetTool$new(plot = mods$plot$get_instance())

  hlp <- HelpTool$new(plot = mods$plot$get_instance())

  pnt <- PanTool$new(plot = mods$plot$get_instance())

  wzt <- WheelZoomTool$new(plot = mods$plot$get_instance())

  svt <- SaveTool$new(plot = mods$plot$get_instance())

  mods$tools <- list(
    box_zoom = bzt,
    reset = rst,
    help = hlp,
    pan = pnt,
    wheel_zoom = wzt,
    save = svt
  )

  te <- ToolEvents$new()

  tb <- Toolbar$new(
    active_drag = "auto",
    active_scroll = "auto",
    active_tap = "auto",
    tools = list(
      bzt$get_instance(),
      rst$get_instance(),
      hlp$get_instance(),
      pnt$get_instance(),
      wzt$get_instance(),
      svt$get_instance()
    )
  )

  mods$box_zoom_ann <- ba
  mods$tool_events <- te
  mods$toolbar <- tb

  mods
}

add_legend <- function(mods) {
  lgnd_items <- unname(lapply(mods$layers, function(x) {
    if (is.null(x$legend_item))
      return(NULL)
    x$legend_item$get_instance()
  }))
  lgnd_items[sapply(lgnd_items, is.null)] <- NULL

  if (length(lgnd_items) > 0) {
    lgnd <- Legend$new(
      items = lgnd_items,
      plot = mods$plot$get_instance()
    )
    mods$legend <- lgnd
  }

  mods
}

get_renderers <- function(mods) {
  glrs <- unname(lapply(mods$layers, function(x) x$glyph_renderer$get_instance()))

  res <- c(glrs,
    list(
      mods$axes$x$axis$get_instance(),
      mods$axes$x$grid$get_instance(),
      mods$axes$y$axis$get_instance(),
      mods$axes$y$grid$get_instance()
    )
  )

  if (!is.null(mods$legend))
    res[[length(res) + 1]] <- mods$legend$get_instance()

  if (!is.null(mods$box_zoom_ann))
    res[[length(res) + 1]] <- mods$box_zoom_ann$get_instance()

  res
}

prepare_figure <- function(mods) {
  mods <- add_axes(mods)
  mods <- add_title(mods)
  mods <- add_tools(mods)
  mods <- add_legend(mods)

  mods$plot$set_prop("below", list(mods$axes$x$axis$get_instance()))
  mods$plot$set_prop("left", list(mods$axes$y$axis$get_instance()))
  mods$plot$set_prop("x_range", mods$axes$x$range$get_instance())
  mods$plot$set_prop("y_range", mods$axes$y$range$get_instance())
  mods$plot$set_prop("renderers", get_renderers(mods))
  if (!is.null(mods$title))
    mods$plot$set_prop("title", mods$title$get_instance())
  if (!is.null(mods$tool_events))
    mods$plot$set_prop("tool_events", mods$tool_events$get_instance())
  if (!is.null(mods$toolbar))
    mods$plot$set_prop("toolbar", mods$toolbar$get_instance())

  mods
}
