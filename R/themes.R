## should integrate some of this with scales package

tableauColors <- list(BlueRed12 = c("#2C69B0", "#B5C8E2", "#F02720", "#FFB6B0", "#AC613C", "#E9C39B", "#6BA3D6", "#B5DFFD", "#AC8763", "#DDC9B4", "#BD0A36", "#F4737A"),
  BlueRed6 = c("#2C69B0", "#F02720", "#AC613C", "#6BA3D6", "#EA6B73", "#E9C39B"),
  ColorBlind10 = c("#006BA4", "#FF800E", "#ABABAB", "#595959", "#5F9ED1", "#C85200", "#898989", "#A2C8EC", "#FFBC79", "#CFCFCF"),
  Gray5 = c("#60636A", "#A5ACAF", "#414451", "#8F8782", "#CFCFCF"),
  GreenOrange12 = c("#32A251", "#ACD98D", "#FF7F0F", "#FFB977", "#3CB7CC", "#98D9E4", "#B85A0D", "#FFD94A", "#39737C", "#86B4A9", "#82853B", "#CCC94D"),
  GreenOrange6 = c("#32A251", "#FF7F0F", "#3CB7CC", "#FFD94A", "#39737C", "#B85A0D"),
  PurpleGray12 = c("#7B66D2", "#A699E8", "#DC5FBD", "#FFC0DA", "#5F5A41", "#B4B19B", "#995688", "#D898BA", "#AB6AD5", "#D098EE", "#8B7C6E", "#DBD4C5"),
  PurpleGray6 = c("#7B66D2", "#DC5FBD", "#94917B", "#995688", "#D098EE", "#D7D5C5"),
  Tableau10 = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"),
  Tableau10Light = c("#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5", "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5"),
  Tableau10Medium = c("#729ECE", "#FF9E4A", "#67BF5C", "#ED665D", "#AD8BC9", "#A8786E", "#ED97CA", "#A2A2A2", "#CDCC5D", "#6DCCDA"),
  Tableau20 = c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C", "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5", "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F", "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5"),
  TrafficLight = c("#B10318", "#DBA13A", "#309343", "#D82526", "#FFC156", "#69B764", "#F26C64", "#FFDD71", "#9FCD99")
)

#' @export
tableau_pal <- function(pal = "Tableau10") {
  pal <- tableauColors[[pal]]
  function(n) {
    if(n > length(pal))
      message("There are more levels to color than there are available colors in this palette... repeating colors")
    pal[(seq_len(n) - 1) %% length(pal) + 1]
  }
}
# show_col(tableau_pal("Tableau20")(20))

#' @export
glyph_scale <- function() {
  function(n) {
    pal <- c("circle", "square", "triangle", "diamond", "circle_cross", "circle_x", "cross", "diamond_cross", "inverted_triangle","square_cross", "square_x", "x", "asterisk")
    if(n > length(pal))
      message("There are more levels to color than there are available colors in this palette... repeating colors")
    pal[(seq_len(n) - 1) %% length(pal) + 1]
  }
}

#' @export
continuous_pal <- function() {
  colorRampPalette(c("#66C2A4", "#41AE76", "#238B45", "#006D2C", "#00441B"))
}

# add trans and inv
#' @export
size_scale <- function(min = 2, max = 20) {
  function(n) {
    seq(min, max, length = n)
  }
}

## theme
tableau_theme <- list(
  line = tableauColors$Tableau10,
  glyph = tableauColors$Tableau10
  ## more stuff here
)

bk_theme <- list(
  glyph = list(discrete = glyph_scale(), continuous = glyph_scale()),
  line_color = list(discrete = tableau_pal("Tableau10"), continuous = continuous_pal()),
  fill_color = list(discrete = tableau_pal("Tableau10"), continuous = continuous_pal()),
  text_color = list(discrete = tableau_pal("Tableau10"), continuous = continuous_pal()),
  size = list(discrete = size_scale(), continuous = size_scale())
  # line_type =
)



# http://www.w3.org/TR/css3-color/#svg-color
cssColors <- c("aliceblue", "antiquewhite", "aqua", "aquamarine", "azure", "beige", "bisque", "black", "blanchedalmond", "blue", "blueviolet", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornflowerblue", "cornsilk", "crimson", "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "darkgreen", "darkgrey", "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", "deeppink", "deepskyblue", "dimgray", "dimgrey", "dodgerblue", "firebrick", "floralwhite", "forestgreen", "fuchsia", "gainsboro", "ghostwhite", "gold", "goldenrod", "gray", "green", "greenyellow", "grey", "honeydew", "hotpink", "indianred", "indigo", "ivory", "khaki", "lavender", "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan", "lightgoldenrodyellow", "lightgray", "lightgreen", "lightgrey", "lightpink", "lightsalmon", "lightseagreen", "lightskyblue", "lightslategray", "lightslategrey", "lightsteelblue", "lightyellow", "lime", "limegreen", "linen", "magenta", "maroon", "mediumaquamarine", "mediumblue", "mediumorchid", "mediumpurple", "mediumseagreen", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "mintcream", "mistyrose", "moccasin", "navajowhite", "navy", "oldlace", "olive", "olivedrab", "orange", "orangered", "orchid", "palegoldenrod", "palegreen", "paleturquoise", "palevioletred", "papayawhip", "peachpuff", "peru", "pink", "plum", "powderblue", "purple", "red", "rosybrown", "royalblue", "saddlebrown", "salmon", "sandybrown", "seagreen", "seashell", "sienna", "silver", "skyblue", "slateblue", "slategray", "slategrey", "snow", "springgreen", "steelblue", "tan", "teal", "thistle", "tomato", "turquoise", "violet", "wheat", "white", "whitesmoke", "yellow", "yellowgreen")

