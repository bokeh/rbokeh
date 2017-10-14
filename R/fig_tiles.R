#' Specify a tile provider for a tile map plot.
#' @param url Tile service url e.g., http://c.tile.openstreetmap.org/{Z}/{X}/{Y}.png
#' @param attribution Data provider attribution content. This can include HTML content.
#' @param type The type of tile provider (currently only "WMTS" is supported).
tile_spec <- function(url, attribution, type = "WMTS") {
  if (type != "WMTS")
    stop("Currently only 'WMTS' tile sources are supported.")

  structure(
    list(url = url, attribution = attribution, type = type),
    class = c("list", "tile_spec")
  )
}

#' Add map tiles to a Bokeh figure.
#' @param fig figure to modify
#' @param tile_spec A character string indicating to use one of the pre-defined tile specifications (see details) or a custom tile specification from calling \code{\link{tile_spec}}.
#' @param hi_res When using a pre-defined tile specification, should a high-resolution (if available) version of the tiles be served? Boolean.
#' @param y_origin_offset A y-offset in plot coordinates
#' @param x_origin_offset An x-offset in plot coordinates
#' @param min_zoom A minimum zoom level for the tile layer. This is the most zoomed-out level.
#' @param max_zoom A maximum zoom level for the tile layer. This is the most zoomed-in level.
#' @param extra_url_vars A dictionary that maps url variable template keys to values. These variables are useful for parts of tile urls which do not change from tile to tile (e.g. server host name, or layer name).
#' @param initial_resolution Resolution (plot_units / pixels) of minimum zoom level of tileset projection. NULL to auto-compute.
#' @param tile_size Tile size in pixels (e.g. 256)
#' @param alpha Tile opacity 0.0 - 1.0.
#' @param render_parents Flag enable/disable drawing of parent tiles while waiting for new tiles to arrive. Default value is TRUE.
#' @param visible Is the renderer visible?
#' @param level Specifies the level in which to paint this renderer. One of 'image', 'underlay', 'glyph', 'annotation', 'overlay'.
#' @details
#' Here is a list of pre-defined tile specifications from different sources:
#'
#' \strong{Stamen} (\url{http://maps.stamen.com/})
#'
#' \itemize{
#'   \item toner ---\url{http://maps.stamen.com/toner/#12/37.7706/-122.3782}
#'   \item toner-hybrid ---\url{http://maps.stamen.com/toner-hybrid/#12/37.7706/-122.3782}
#'   \item toner-labels ---\url{http://maps.stamen.com/toner-labels/#12/37.7706/-122.3782}
#'   \item toner-lines ---\url{http://maps.stamen.com/toner-lines/#12/37.7706/-122.3782}
#'   \item toner-background ---\url{http://maps.stamen.com/toner-background/#12/37.7706/-122.3782}
#'   \item toner-lite ---\url{http://maps.stamen.com/toner-lite/#12/37.7706/-122.3782}
#'   \item terrain ---\url{http://maps.stamen.com/terrain/#12/37.7706/-122.3782}
#'   \item terrain-labels ---\url{http://maps.stamen.com/terrain-labels/#12/37.7706/-122.3782}
#'   \item terrain-lines ---\url{http://maps.stamen.com/terrain-lines/#12/37.7706/-122.3782}
#'   \item terrain-background ---\url{http://maps.stamen.com/terrain-background/#12/37.7706/-122.3782}
#' }
#'
#' \strong{Carto} (\url{https://carto.com/location-data-services/basemaps/})
#'
#' \itemize{
#'   \item carto_light_all
#'   \item carto_dark_all
#'   \item carto_light_nolabels
#'   \item carto_light_only_labels
#'   \item carto_dark_nolabels
#'   \item carto_dark_only_labels
#' }
#'
#' \strong{Wikipedia}
#'
#' \itemize{
#'   \item wikepedia
#' }
#' @examples
#' figure(width = 800) %>%
#'   add_tiles("stamen_toner")
#'
#' figure(width = 800) %>%
#'   add_tiles("carto_light_all")
#'
#' figure(width = 800) %>%
#'   add_tiles("wikipedia")
#' @export
add_tiles <- function(
  fig,
  tile_spec,
  hi_res = TRUE,
  y_origin_offset = NULL,
  x_origin_offset = NULL,
  min_zoom = NULL,
  max_zoom = NULL,
  extra_url_vars = NULL,
  initial_resolution = NULL,
  tile_size = NULL,
  alpha = NULL,
  render_parents = NULL,
  visible = NULL,
  level = NULL
) {
  if (is.character(tile_spec)) {
    if (tile_spec %in% names(tile_sources)) {
      tile_spec <- tile_sources[[tile_spec]]
      if (hi_res && !grepl("watercolor", tile_spec$url)) {
        tile_spec$url <- gsub("(.*)(\\.png)", "\\1@2x\\2", tile_spec$url)
      }
    } else {
      stop("Tile specification ", tile_spec, " is not one of the pre-defined ",
        "specifications to choose from", call. = FALSE)
    }
  }

  if (!inherits(tile_spec, "tile_spec"))
    stop("Argument 'tile_spec' to 'add_tiles()' must come from calling 'tile_spec()'.",
      call. = FALSE)

  args <- get_specified_args(nnms = c("fig", "tile_spec", "hi_res"))

  args_rend_nms <- c("alpha", "render_parents", "visible", "level")
  args_rend <- args[intersect(names(args), args_rend_nms)]
  args_tile <- args[setdiff(names(args), args_rend_nms)]

  args_tile$url <- tile_spec$url
  args_tile$attribution <- tile_spec$attribution
  fig$x$pars$tiles <- list(args_tile = args_tile, args_rend = args_rend, type = tile_spec$type)
  fig
}

toner_terrain_attrib <- paste0(
  "Map tiles by <a href=\"http://stamen.com\">Stamen Design</a>, ",
  "under <a href=\"http://creativecommons.org/licenses/by/3.0\">CC BY 3.0</a>. ",
  "Data by <a href=\"http://openstreetmap.org\">OpenStreetMap</a>, ",
  "under <a href=\"http://www.openstreetmap.org/copyright\">ODbL</a>"
)

watercolor_attrib <- paste0(
  "Map tiles by <a href=\"http://stamen.com\">Stamen Design</a>, ",
  "under <a href=\"http://creativecommons.org/licenses/by/3.0\">CC BY 3.0</a>. ",
  "Data by <a href=\"http://openstreetmap.org\">OpenStreetMap</a>, ",
  "under <a href=\"http://creativecommons.org/licenses/by-sa/3.0\">CC BY SA</a>."
)

carto_attrib <- paste0(
  "&copy; <a href=\"http://www.openstreetmap.org/copyright\">OpenStreetMap</a> ",
  "contributors,&copy; <a href=\"https://cartodb.com/attributions\">Carto</a>"
)

wiki_attrib <- paste0(
  "Wikimedia Maps | ",
  "&copy; <a href=\"http://www.openstreetmap.org/copyright\">OpenStreetMap</a> ",
  "contributors"
)

tile_sources <- list(
  "stamen_toner" = tile_spec(
    url = "http://tile.stamen.com/toner/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_toner_hybrid" = tile_spec(
    url = "http://tile.stamen.com/toner-hybrid/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_toner_labels" = tile_spec(
    url = "http://tile.stamen.com/toner-labels/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_toner_lines" = tile_spec(
    url = "http://tile.stamen.com/toner-lines/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_toner_background" = tile_spec(
    url = "http://tile.stamen.com/toner-background/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_toner_lite" = tile_spec(
    url = "http://tile.stamen.com/toner-lite/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_terrain" = tile_spec(
    url = "http://tile.stamen.com/terrain/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_terrain_labels" = tile_spec(
    url = "http://tile.stamen.com/terrain-labels/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_terrain_lines" = tile_spec(
    url = "http://tile.stamen.com/terrain-lines/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_terrain_background" = tile_spec(
    url = "http://tile.stamen.com/terrain-background/{Z}/{X}/{Y}.png",
    attribution = toner_terrain_attrib
  ),
  "stamen_watercolor" = tile_spec(
    url = "http://tile.stamen.com/watercolor/{z}/{x}/{y}.png",
    attribution = watercolor_attrib
  ),
  "carto_light_all" = tile_spec(
    url = "http://tiles.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
    attribution = carto_attrib
  ),
  "carto_dark_all" = tile_spec(
    url = "http://tiles.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
    attribution = carto_attrib
  ),
  "carto_light_nolabels" = tile_spec(
    url = "http://tiles.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
    attribution = carto_attrib
  ),
  "carto_light_only_labels" = tile_spec(
    url = "http://tiles.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png",
    attribution = carto_attrib
  ),
  "carto_dark_nolabels" = tile_spec(
    url = "http://tiles.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}.png",
    attribution = carto_attrib
  ),
  "carto_dark_only_labels" = tile_spec(
    url = "http://tiles.basemaps.cartocdn.com/dark_only_labels/{z}/{x}/{y}.png",
    attribution = carto_attrib
  ),
  "wikipedia" = tile_spec(
    url = "https://maps.wikimedia.org/osm-intl/{Z}/{X}/{Y}.png",
    attribution = wiki_attrib
  )
)

# http://geo.holoviews.org/Working_with_Bokeh.html
