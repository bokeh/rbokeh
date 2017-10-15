#' Create a hover specification.
#'
#' @param line_policy When showing tooltips for lines, designates whether the tooltip position should be the "previous" or "next" points on the line, the "nearest" point to the current mouse position, or "interpolate" alongthe line to the current mouse position. Must be one of 'prev', 'next', 'nearest', 'interp', 'none'.
#' @param point_policy Whether the tooltip position should snap to the "center" (or other anchor) position of the associated glyph, or always follow the current mouse cursor position. Must be one of 'snap_to_data', 'follow_mouse', 'none'.
#' @param anchor If point policy is set to `"snap_to_data"`, `anchor` defines the attachment point of a tooltip. The default is to attach to the center of a glyph. Must be one of 'top_left', 'top_center', 'top_right', 'center_left', 'center', 'center_right', 'bottom_left', 'bottom_center', 'bottom_right'.
#' @param show_arrow Logical specifying whether tooltip's arrow should be shown.
#' @param mode Whether to consider hover pointer as a point (x/y values), or a span on h or v directions. Must be one of 'mouse', 'hline', 'vline'.
#' @param attachment Whether tooltip's arrow should appear in the horizontal or vertical dimension. Must be one of 'horizontal', 'vertical'.
#' @param callback A callback to run in the browser whenever the input's value changes. The cb_data parameter that is available to the Callback code will contain two HoverTool specific fields: :index: object containing the indices of the hovered points in the data, and source :geometry: object containing the coordinates of the hover cursor.
#' @export
hov <- function(
  data = NULL,
  line_policy = NULL,
  callback = NULL,
  anchor = NULL,
  show_arrow = NULL,
  mode = NULL,
  attachment = NULL,
  point_policy = NULL) {

  spec <- list()
  spec$data <- enquo(data)
  spec$line_policy <- line_policy
  spec$callback <- callback
  spec$anchor <- anchor
  spec$show_arrow <- show_arrow
  spec$mode <- mode
  spec$attachment <- attachment
  spec$point_policy <- point_policy

  class(spec) <- c("list", "hover_spec")

  spec
}
