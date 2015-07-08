option_names <- c("width", "height", "title","ylim","xlim","plot_width","plot_height","x_axis_type","y_axis_type","x_mapper_type","y_mapper_type","background_fill","border_fill","min_border","min_border_left","min_border_right","min_border_top","min_border_bottom","h_symmetry","v_symmetry","outline_line_color", "xaxes", "yaxes", "tools")

line_prop_names <- c("line_color", "line_width", "line_alpha", "line_join", "line_cap", "line_dash", "line_dash_offset")
fill_prop_names <- c("fill_color", "fill_alpha")
text_prop_names <- c("text_font", "text_font_size", "text_font_style", "text_color", "text_alpha", "text_align", "text_baseline")

## glyphs are simple enough we can get away with
## not having formal classes for them
## but here's some additional info about each
## lp = does this glyph have line properties?
## fp = does this glyph have fill properties?
## tp = does this glyph have text properties?
glyph_props <- list(
  ###### markers ######
  asterisk = list(lp = TRUE, fp = FALSE, tp = FALSE),
  circle = list(lp = TRUE, fp = TRUE, tp = FALSE),
  circle_cross = list(lp = TRUE, fp = TRUE, tp = FALSE),
  circle_x = list(lp = TRUE, fp = TRUE, tp = FALSE),
  cross = list(lp = TRUE, fp = FALSE, tp = FALSE),
  diamond = list(lp = TRUE, fp = TRUE, tp = FALSE),
  diamond_cross = list(lp = TRUE, fp = TRUE, tp = FALSE),
  inverted_triangle = list(lp = TRUE, fp = TRUE, tp = FALSE),
  square = list(lp = TRUE, fp = TRUE, tp = FALSE),
  square_cross = list(lp = TRUE, fp = TRUE, tp = FALSE),
  square_x = list(lp = TRUE, fp = TRUE, tp = FALSE),
  triangle = list(lp = TRUE, fp = TRUE, tp = FALSE),
  x = list(lp = TRUE, fp = TRUE, tp = FALSE),
  ###### glyphs ######
  annular_wedge = list(lp = TRUE, fp = TRUE, tp = FALSE),
  annulus = list(lp = TRUE, fp = TRUE, tp = FALSE),
  arc = list(lp = TRUE, fp = FALSE, tp = FALSE),
  bezier = list(lp = TRUE, fp = FALSE, tp = FALSE),
  image = list(lp = FALSE, fp = FALSE, tp = FALSE),
  image_RGBA = list(lp = FALSE, fp = FALSE, tp = FALSE),
  image_url = list(lp = FALSE, fp = FALSE, tp = FALSE),
  line = list(lp = TRUE, fp = FALSE, tp = FALSE),
  multi_line = list(lp = TRUE, fp = FALSE, tp = FALSE),
  oval = list(lp = TRUE, fp = TRUE, tp = FALSE),
  patch = list(lp = TRUE, fp = TRUE, tp = FALSE),
  patches = list(lp = TRUE, fp = TRUE, tp = FALSE),
  quad = list(lp = TRUE, fp = TRUE, tp = FALSE),
  quadratic = list(lp = TRUE, fp = FALSE, tp = FALSE),
  ray = list(lp = TRUE, fp = FALSE, tp = FALSE),
  rect = list(lp = TRUE, fp = TRUE, tp = FALSE),
  segment = list(lp = TRUE, fp = FALSE, tp = FALSE),
  text = list(lp = FALSE, fp = FALSE, tp = TRUE),
  wedge = list(lp = TRUE, fp = TRUE, tp = FALSE)
)

## list of conversions from R's "pch" to glyph / line / fill properties
marker_dict <- list(
   "0" =  list(glyph = "square",            line = TRUE,  fill = FALSE),
   "1" =  list(glyph = "circle",            line = TRUE,  fill = FALSE),
   "2" =  list(glyph = "triangle",          line = TRUE,  fill = FALSE),
   "3" =  list(glyph = "cross",             line = TRUE,  fill = FALSE),
   "4" =  list(glyph = "x",                 line = TRUE,  fill = FALSE),
   "5" =  list(glyph = "diamond",           line = TRUE,  fill = FALSE),
   "6" =  list(glyph = "inverted_triangle", line = TRUE,  fill = FALSE),
   "7" =  list(glyph = "square_x",          line = TRUE,  fill = FALSE),
   "8" =  list(glyph = "asterisk",          line = TRUE,  fill = FALSE),
   "9" =  list(glyph = "diamond_cross",     line = TRUE,  fill = FALSE),
   "10" = list(glyph = "circle_cross",      line = TRUE,  fill = FALSE),
   "12" = list(glyph = "square_cross",      line = TRUE,  fill = FALSE),
   "13" = list(glyph = "circle_x",          line = TRUE,  fill = FALSE),
   "15" = list(glyph = "square",            line = FALSE, fill = TRUE ),
   "16" = list(glyph = "circle",            line = FALSE, fill = TRUE ),
   "17" = list(glyph = "triangle",          line = FALSE, fill = TRUE ),
   "18" = list(glyph = "diamond",           line = FALSE, fill = TRUE ),
   "19" = list(glyph = "circle",            line = FALSE, fill = TRUE ),
   "20" = list(glyph = "circle",            line = FALSE, fill = TRUE ),
   "21" = list(glyph = "circle",            line = TRUE,  fill = TRUE ),
   "22" = list(glyph = "square",            line = TRUE,  fill = TRUE ),
   "23" = list(glyph = "diamond",           line = TRUE,  fill = TRUE ),
   "24" = list(glyph = "triangle",          line = TRUE,  fill = TRUE ),
   "25" = list(glyph = "inverted_triangle", line = TRUE,  fill = TRUE ),
   asterisk          = list(glyph = "asterisk", line = TRUE, fill = FALSE),
   circle            = list(glyph = "circle", line = TRUE, fill = TRUE ),
   circle_cross      = list(glyph = "circle_cross", line = TRUE, fill = TRUE ),
   circle_x          = list(glyph = "circle_x", line = TRUE, fill = TRUE ),
   cross             = list(glyph = "cross", line = TRUE, fill = FALSE),
   diamond           = list(glyph = "diamond", line = TRUE, fill = TRUE ),
   diamond_cross     = list(glyph = "diamond_cross", line = TRUE, fill = TRUE ),
   inverted_triangle = list(glyph = "inverted_triangle", line = TRUE, fill = TRUE ),
   square            = list(glyph = "square", line = TRUE, fill = TRUE ),
   square_cross      = list(glyph = "square_cross", line = TRUE, fill = TRUE ),
   square_x          = list(glyph = "square_x", line = TRUE, fill = TRUE ),
   triangle          = list(glyph = "triangle", line = TRUE, fill = TRUE ),
   x                 = list(glyph = "x", line = TRUE, fill = TRUE )
)

# 16, 19, 20 are the same
# 11 and 14 are missing

## list of conversions from R's "lty" to line_dash
lty_dict <- list(
  "1" = NULL,
  "2" = c(8, 8),
  "3" = c(2, 6),
  "4" = c(2, 6, 8, 6),
  "5" = c(14, 6),
  "6" = c(12, 4, 4, 4),
  "solid" = NULL,
  "dashed" = c(8, 8),
  "dotted" = c(2, 6),
  "dotdash" = c(2, 6, 8, 6),
  "longdash" = c(14, 6),
  "twodash" = c(12, 4, 4, 4)
)

lty_names <- names(lty_dict)

## convert ljoin to line_cap
ljoin_dict <- list(
  "1" = "round",
  "2" = "mitre",
  "3" = "bevel",
  "round" = "round",
  "mitre" = "mitre",
  "bevel" = "bevel"
)

marker_pch_types <- setdiff(0:25, c(11, 14))
marker_names <- c("asterisk", "circle", "circle_cross", "circle_x", "cross", "diamond", "diamond_cross", "inverted_triangle", "square", "square_cross", "square_x", "triangle", "x")

