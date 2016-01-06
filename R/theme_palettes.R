#' Palettes for themes
#' @rdname palettes
#' @param pal palette name
#' @param min minimum value
#' @param max maximum value
#' @export
pal_tableau <- function(pal = "Tableau10") {
  pal <- tableau_colors[[pal]]
  function(n) {
    # if(n > length(pal))
    #   message("There are more levels to color than there are available colors in this palette... repeating colors")
    pal[(seq_len(n) - 1) %% length(pal) + 1]
  }
}
# show_col(pal_tableau("Tableau20")(20))

#' @rdname palettes
#' @export
pal_bk_glyph <- function() {
  function(n) {
    pal <- c("circle", "square", "triangle", "diamond", "circle_cross", "circle_x", "cross", "diamond_cross", "inverted_triangle","square_cross", "square_x", "x", "asterisk")
    # if(n > length(pal))
    #   message("There are more levels to color than there are available colors in this palette... repeating colors")
    pal[(seq_len(n) - 1) %% length(pal) + 1]
  }
}

#' @rdname palettes
#' @export
#' @importFrom grDevices colorRampPalette
pal_gradient <- function() {
  grDevices::colorRampPalette(c("#66C2A4", "#41AE76", "#238B45", "#006D2C", "#00441B"))
}

#' @rdname palettes
#' @export
pal_size <- function(min = 2, max = 20) {
  function(n) {
    seq(min, max, length = n)
  }
}

#' @rdname palettes
#' @export
pal_bk_line_dash <- function() {
  function(n) {
    dashes <- as.character(1:6)
    # if(n > 6)
    #   message("There are more levels for line dash than there are available line dash styles in this theme... repeating line dash")
    lty_dict[dashes[(seq_len(n) - 1) %% length(dashes) + 1]]
  }
}

#' @rdname palettes
#' @export
pal_bk_line_width <- function() {
  function(n) {
    dashes <- as.character(1:6)
    # if(n > 6)
    #   message("There are more levels for line dash than there are available line dash styles in this theme... repeating line dash")
    dashes[(seq_len(n) - 1) %% length(dashes) + 1]
  }
}

# http://bokeh.pydata.org/en/latest/docs/reference/palettes.html
bk_gradient_palettes <- list(
  Spectral3 = c("#99d594", "#ffffbf", "#fc8d59"),
  Spectral4 = c("#2b83ba", "#abdda4", "#fdae61", "#d7191c"),
  Spectral5 = c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
  Spectral6 = c("#3288bd", "#99d594", "#e6f598", "#fee08b", "#fc8d59", "#d53e4f"),
  Spectral7 = c("#3288bd", "#99d594", "#e6f598", "#ffffbf", "#fee08b", "#fc8d59", "#d53e4f"),
  Spectral8 = c("#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f"),
  Spectral9 = c("#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#ffffbf", "#fee08b", "#fdae61", "#f46d43", "#d53e4f"),
  Spectral10 = c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142"),
  Spectral11 = c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#ffffbf", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142"),
  RdYlGn3 = c("#91cf60", "#ffffbf", "#fc8d59"),
  RdYlGn4 = c("#1a9641", "#a6d96a", "#fdae61", "#d7191c"),
  RdYlGn5 = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
  RdYlGn6 = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"),
  RdYlGn7 = c("#1a9850", "#91cf60", "#d9ef8b", "#ffffbf", "#fee08b", "#fc8d59", "#d73027"),
  RdYlGn8 = c("#1a9850", "#66bd63", "#a6d96a", "#d9ef8b", "#fee08b", "#fdae61", "#f46d43", "#d73027"),
  RdYlGn9 = c("#1a9850", "#66bd63", "#a6d96a", "#d9ef8b", "#ffffbf", "#fee08b", "#fdae61", "#f46d43", "#d73027"),
  RdYlGn10 = c("#006837", "#1a9850", "#66bd63", "#a6d96a", "#d9ef8b", "#fee08b", "#fdae61", "#f46d43", "#d73027", "#a50026"),
  RdYlGn11 = c("#006837", "#1a9850", "#66bd63", "#a6d96a", "#d9ef8b", "#ffffbf", "#fee08b", "#fdae61", "#f46d43", "#d73027", "#a50026"),
  OrRd3 = c("#e34a33", "#fdbb84", "#fee8c8"),
  OrRd4 = c("#d7301f", "#fc8d59", "#fdcc8a", "#fef0d9"),
  OrRd5 = c("#b30000", "#e34a33", "#fc8d59", "#fdcc8a", "#fef0d9"),
  OrRd6 = c("#b30000", "#e34a33", "#fc8d59", "#fdbb84", "#fdd49e", "#fef0d9"),
  OrRd7 = c("#990000", "#d7301f", "#ef6548", "#fc8d59", "#fdbb84", "#fdd49e", "#fef0d9"),
  OrRd8 = c("#990000", "#d7301f", "#ef6548", "#fc8d59", "#fdbb84", "#fdd49e", "#fee8c8", "#fff7ec"),
  OrRd9 = c("#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59", "#fdbb84", "#fdd49e", "#fee8c8", "#fff7ec"),
  PuBu3 = c("#2b8cbe", "#a6bddb", "#ece7f2"),
  PuBu4 = c("#0570b0", "#74a9cf", "#bdc9e1", "#f1eef6"),
  PuBu5 = c("#045a8d", "#2b8cbe", "#74a9cf", "#bdc9e1", "#f1eef6"),
  PuBu6 = c("#045a8d", "#2b8cbe", "#74a9cf", "#a6bddb", "#d0d1e6", "#f1eef6"),
  PuBu7 = c("#034e7b", "#0570b0", "#3690c0", "#74a9cf", "#a6bddb", "#d0d1e6", "#f1eef6"),
  PuBu8 = c("#034e7b", "#0570b0", "#3690c0", "#74a9cf", "#a6bddb", "#d0d1e6", "#ece7f2", "#fff7fb"),
  PuBu9 = c("#023858", "#045a8d", "#0570b0", "#3690c0", "#74a9cf", "#a6bddb", "#d0d1e6", "#ece7f2", "#fff7fb"),
  BuPu3 = c("#8856a7", "#9ebcda", "#e0ecf4"),
  BuPu4 = c("#88419d", "#8c96c6", "#b3cde3", "#edf8fb"),
  BuPu5 = c("#810f7c", "#8856a7", "#8c96c6", "#b3cde3", "#edf8fb"),
  BuPu6 = c("#810f7c", "#8856a7", "#8c96c6", "#9ebcda", "#bfd3e6", "#edf8fb"),
  BuPu7 = c("#6e016b", "#88419d", "#8c6bb1", "#8c96c6", "#9ebcda", "#bfd3e6", "#edf8fb"),
  BuPu8 = c("#6e016b", "#88419d", "#8c6bb1", "#8c96c6", "#9ebcda", "#bfd3e6", "#e0ecf4", "#f7fcfd"),
  BuPu9 = c("#4d004b", "#810f7c", "#88419d", "#8c6bb1", "#8c96c6", "#9ebcda", "#bfd3e6", "#e0ecf4", "#f7fcfd"),
  RdBu3 = c("#67a9cf", "#f7f7f7", "#ef8a62"),
  RdBu4 = c("#0571b0", "#92c5de", "#f4a582", "#ca0020"),
  RdBu5 = c("#0571b0", "#92c5de", "#f7f7f7", "#f4a582", "#ca0020"),
  RdBu6 = c("#2166ac", "#67a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2182b"),
  RdBu7 = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"),
  RdBu8 = c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b"),
  RdBu9 = c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b"),
  RdBu10 = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
  RdBu11 = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
  Oranges3 = c("#e6550d", "#fdae6b", "#fee6ce"),
  Oranges4 = c("#d94701", "#fd8d3c", "#fdbe85", "#feedde"),
  Oranges5 = c("#a63603", "#e6550d", "#fd8d3c", "#fdbe85", "#feedde"),
  Oranges6 = c("#a63603", "#e6550d", "#fd8d3c", "#fdae6b", "#fdd0a2", "#feedde"),
  Oranges7 = c("#8c2d04", "#d94801", "#f16913", "#fd8d3c", "#fdae6b", "#fdd0a2", "#feedde"),
  Oranges8 = c("#8c2d04", "#d94801", "#f16913", "#fd8d3c", "#fdae6b", "#fdd0a2", "#fee6ce", "#fff5eb"),
  Oranges9 = c("#7f2704", "#a63603", "#d94801", "#f16913", "#fd8d3c", "#fdae6b", "#fdd0a2", "#fee6ce", "#fff5eb"),
  BuGn3 = c("#2ca25f", "#99d8c9", "#e5f5f9"),
  BuGn4 = c("#238b45", "#66c2a4", "#b2e2e2", "#edf8fb"),
  BuGn5 = c("#006d2c", "#2ca25f", "#66c2a4", "#b2e2e2", "#edf8fb"),
  BuGn6 = c("#006d2c", "#2ca25f", "#66c2a4", "#99d8c9", "#ccece6", "#edf8fb"),
  BuGn7 = c("#005824", "#238b45", "#41ae76", "#66c2a4", "#99d8c9", "#ccece6", "#edf8fb"),
  BuGn8 = c("#005824", "#238b45", "#41ae76", "#66c2a4", "#99d8c9", "#ccece6", "#e5f5f9", "#f7fcfd"),
  BuGn9 = c("#00441b", "#006d2c", "#238b45", "#41ae76", "#66c2a4", "#99d8c9", "#ccece6", "#e5f5f9", "#f7fcfd"),
  PiYG3 = c("#a1d76a", "#f7f7f7", "#e9a3c9"),
  PiYG4 = c("#4dac26", "#b8e186", "#f1b6da", "#d01c8b"),
  PiYG5 = c("#4dac26", "#b8e186", "#f7f7f7", "#f1b6da", "#d01c8b"),
  PiYG6 = c("#4d9221", "#a1d76a", "#e6f5d0", "#fde0ef", "#e9a3c9", "#c51b7d"),
  PiYG7 = c("#4d9221", "#a1d76a", "#e6f5d0", "#f7f7f7", "#fde0ef", "#e9a3c9", "#c51b7d"),
  PiYG8 = c("#4d9221", "#7fbc41", "#b8e186", "#e6f5d0", "#fde0ef", "#f1b6da", "#de77ae", "#c51b7d"),
  PiYG9 = c("#4d9221", "#7fbc41", "#b8e186", "#e6f5d0", "#f7f7f7", "#fde0ef", "#f1b6da", "#de77ae", "#c51b7d"),
  PiYG10 = c("#276419", "#4d9221", "#7fbc41", "#b8e186", "#e6f5d0", "#fde0ef", "#f1b6da", "#de77ae", "#c51b7d", "#8e0152"),
  PiYG11 = c("#276419", "#4d9221", "#7fbc41", "#b8e186", "#e6f5d0", "#f7f7f7", "#fde0ef", "#f1b6da", "#de77ae", "#c51b7d", "#8e0152"),
  YlOrBr3 = c("#d95f0e", "#fec44f", "#fff7bc"),
  YlOrBr4 = c("#cc4c02", "#fe9929", "#fed98e", "#ffffd4"),
  YlOrBr5 = c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4"),
  YlOrBr6 = c("#993404", "#d95f0e", "#fe9929", "#fec44f", "#fee391", "#ffffd4"),
  YlOrBr7 = c("#8c2d04", "#cc4c02", "#ec7014", "#fe9929", "#fec44f", "#fee391", "#ffffd4"),
  YlOrBr8 = c("#8c2d04", "#cc4c02", "#ec7014", "#fe9929", "#fec44f", "#fee391", "#fff7bc", "#ffffe5"),
  YlOrBr9 = c("#662506", "#993404", "#cc4c02", "#ec7014", "#fe9929", "#fec44f", "#fee391", "#fff7bc", "#ffffe5"),
  YlGn3 = c("#31a354", "#addd8e", "#f7fcb9"),
  YlGn4 = c("#238443", "#78c679", "#c2e699", "#ffffcc"),
  YlGn5 = c("#006837", "#31a354", "#78c679", "#c2e699", "#ffffcc"),
  YlGn6 = c("#006837", "#31a354", "#78c679", "#addd8e", "#d9f0a3", "#ffffcc"),
  YlGn7 = c("#005a32", "#238443", "#41ab5d", "#78c679", "#addd8e", "#d9f0a3", "#ffffcc"),
  YlGn8 = c("#005a32", "#238443", "#41ab5d", "#78c679", "#addd8e", "#d9f0a3", "#f7fcb9", "#ffffe5"),
  YlGn9 = c("#004529", "#006837", "#238443", "#41ab5d", "#78c679", "#addd8e", "#d9f0a3", "#f7fcb9", "#ffffe5"),
  RdPu3 = c("#c51b8a", "#fa9fb5", "#fde0dd"),
  RdPu4 = c("#ae017e", "#f768a1", "#fbb4b9", "#feebe2"),
  RdPu5 = c("#7a0177", "#c51b8a", "#f768a1", "#fbb4b9", "#feebe2"),
  RdPu6 = c("#7a0177", "#c51b8a", "#f768a1", "#fa9fb5", "#fcc5c0", "#feebe2"),
  RdPu7 = c("#7a0177", "#ae017e", "#dd3497", "#f768a1", "#fa9fb5", "#fcc5c0", "#feebe2"),
  RdPu8 = c("#7a0177", "#ae017e", "#dd3497", "#f768a1", "#fa9fb5", "#fcc5c0", "#fde0dd", "#fff7f3"),
  RdPu9 = c("#49006a", "#7a0177", "#ae017e", "#dd3497", "#f768a1", "#fa9fb5", "#fcc5c0", "#fde0dd", "#fff7f3"),
  Greens3 = c("#31a354", "#a1d99b", "#e5f5e0"),
  Greens4 = c("#238b45", "#74c476", "#bae4b3", "#edf8e9"),
  Greens5 = c("#006d2c", "#31a354", "#74c476", "#bae4b3", "#edf8e9"),
  Greens6 = c("#006d2c", "#31a354", "#74c476", "#a1d99b", "#c7e9c0", "#edf8e9"),
  Greens7 = c("#005a32", "#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0", "#edf8e9"),
  Greens8 = c("#005a32", "#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0", "#e5f5e0", "#f7fcf5"),
  Greens9 = c("#00441b", "#006d2c", "#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0", "#e5f5e0", "#f7fcf5"),
  PRGn3 = c("#7fbf7b", "#f7f7f7", "#af8dc3"),
  PRGn4 = c("#008837", "#a6dba0", "#c2a5cf", "#7b3294"),
  PRGn5 = c("#008837", "#a6dba0", "#f7f7f7", "#c2a5cf", "#7b3294"),
  PRGn6 = c("#1b7837", "#7fbf7b", "#d9f0d3", "#e7d4e8", "#af8dc3", "#762a83"),
  PRGn7 = c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7", "#e7d4e8", "#af8dc3", "#762a83"),
  PRGn8 = c("#1b7837", "#5aae61", "#a6dba0", "#d9f0d3", "#e7d4e8", "#c2a5cf", "#9970ab", "#762a83"),
  PRGn9 = c("#1b7837", "#5aae61", "#a6dba0", "#d9f0d3", "#f7f7f7", "#e7d4e8", "#c2a5cf", "#9970ab", "#762a83"),
  PRGn10 = c("#00441b", "#1b7837", "#5aae61", "#a6dba0", "#d9f0d3", "#e7d4e8", "#c2a5cf", "#9970ab", "#762a83", "#40004b"),
  PRGn11 = c("#00441b", "#1b7837", "#5aae61", "#a6dba0", "#d9f0d3", "#f7f7f7", "#e7d4e8", "#c2a5cf", "#9970ab", "#762a83", "#40004b"),
  YlGnBu3 = c("#2c7fb8", "#7fcdbb", "#edf8b1"),
  YlGnBu4 = c("#225ea8", "#41b6c4", "#a1dab4", "#ffffcc"),
  YlGnBu5 = c("#253494", "#2c7fb8", "#41b6c4", "#a1dab4", "#ffffcc"),
  YlGnBu6 = c("#253494", "#2c7fb8", "#41b6c4", "#7fcdbb", "#c7e9b4", "#ffffcc"),
  YlGnBu7 = c("#0c2c84", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4", "#ffffcc"),
  YlGnBu8 = c("#0c2c84", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4", "#edf8b1", "#ffffd9"),
  YlGnBu9 = c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4", "#edf8b1", "#ffffd9"),
  RdYlBu3 = c("#91bfdb", "#ffffbf", "#fc8d59"),
  RdYlBu4 = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c"),
  RdYlBu5 = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
  RdYlBu6 = c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", "#fc8d59", "#d73027"),
  RdYlBu7 = c("#4575b4", "#91bfdb", "#e0f3f8", "#ffffbf", "#fee090", "#fc8d59", "#d73027"),
  RdYlBu8 = c("#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fee090", "#fdae61", "#f46d43", "#d73027"),
  RdYlBu9 = c("#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027"),
  RdYlBu10 = c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"),
  RdYlBu11 = c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"),
  BrBG3 = c("#5ab4ac", "#f5f5f5", "#d8b365"),
  BrBG4 = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
  BrBG5 = c("#018571", "#80cdc1", "#f5f5f5", "#dfc27d", "#a6611a"),
  BrBG6 = c("#01665e", "#5ab4ac", "#c7eae5", "#f6e8c3", "#d8b365", "#8c510a"),
  BrBG7 = c("#01665e", "#5ab4ac", "#c7eae5", "#f5f5f5", "#f6e8c3", "#d8b365", "#8c510a"),
  BrBG8 = c("#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a"),
  BrBG9 = c("#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f5f5f5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a"),
  BrBG10 = c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a", "#543005"),
  BrBG11 = c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f5f5f5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a", "#543005"),
  Purples3 = c("#756bb1", "#bcbddc", "#efedf5"),
  Purples4 = c("#6a51a3", "#9e9ac8", "#cbc9e2", "#f2f0f7"),
  Purples5 = c("#54278f", "#756bb1", "#9e9ac8", "#cbc9e2", "#f2f0f7"),
  Purples6 = c("#54278f", "#756bb1", "#9e9ac8", "#bcbddc", "#dadaeb", "#f2f0f7"),
  Purples7 = c("#4a1486", "#6a51a3", "#807dba", "#9e9ac8", "#bcbddc", "#dadaeb", "#f2f0f7"),
  Purples8 = c("#4a1486", "#6a51a3", "#807dba", "#9e9ac8", "#bcbddc", "#dadaeb", "#efedf5", "#fcfbfd"),
  Purples9 = c("#3f007d", "#54278f", "#6a51a3", "#807dba", "#9e9ac8", "#bcbddc", "#dadaeb", "#efedf5", "#fcfbfd"),
  Reds3 = c("#de2d26", "#fc9272", "#fee0d2"),
  Reds4 = c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9"),
  Reds5 = c("#a50f15", "#de2d26", "#fb6a4a", "#fcae91", "#fee5d9"),
  Reds6 = c("#a50f15", "#de2d26", "#fb6a4a", "#fc9272", "#fcbba1", "#fee5d9"),
  Reds7 = c("#99000d", "#cb181d", "#ef3b2c", "#fb6a4a", "#fc9272", "#fcbba1", "#fee5d9"),
  Reds8 = c("#99000d", "#cb181d", "#ef3b2c", "#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2", "#fff5f0"),
  Reds9 = c("#67000d", "#a50f15", "#cb181d", "#ef3b2c", "#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2", "#fff5f0"),
  GnBu3 = c("#43a2ca", "#a8ddb5", "#e0f3db"),
  GnBu4 = c("#2b8cbe", "#7bccc4", "#bae4bc", "#f0f9e8"),
  GnBu5 = c("#0868ac", "#43a2ca", "#7bccc4", "#bae4bc", "#f0f9e8"),
  GnBu6 = c("#0868ac", "#43a2ca", "#7bccc4", "#a8ddb5", "#ccebc5", "#f0f9e8"),
  GnBu7 = c("#08589e", "#2b8cbe", "#4eb3d3", "#7bccc4", "#a8ddb5", "#ccebc5", "#f0f9e8"),
  GnBu8 = c("#08589e", "#2b8cbe", "#4eb3d3", "#7bccc4", "#a8ddb5", "#ccebc5", "#e0f3db", "#f7fcf0"),
  GnBu9 = c("#084081", "#0868ac", "#2b8cbe", "#4eb3d3", "#7bccc4", "#a8ddb5", "#ccebc5", "#e0f3db", "#f7fcf0"),
  Greys3 = c("#636363", "#bdbdbd", "#f0f0f0"),
  Greys4 = c("#525252", "#969696", "#cccccc", "#f7f7f7"),
  Greys5 = c("#252525", "#636363", "#969696", "#cccccc", "#f7f7f7"),
  Greys6 = c("#252525", "#636363", "#969696", "#bdbdbd", "#d9d9d9", "#f7f7f7"),
  Greys7 = c("#252525", "#525252", "#737373", "#969696", "#bdbdbd", "#d9d9d9", "#f7f7f7"),
  Greys8 = c("#252525", "#525252", "#737373", "#969696", "#bdbdbd", "#d9d9d9", "#f0f0f0", "#ffffff"),
  Greys9 = c("#000000", "#252525", "#525252", "#737373", "#969696", "#bdbdbd", "#d9d9d9", "#f0f0f0", "#ffffff"),
  RdGy3 = c("#999999", "#ffffff", "#ef8a62"),
  RdGy4 = c("#404040", "#bababa", "#f4a582", "#ca0020"),
  RdGy5 = c("#404040", "#bababa", "#ffffff", "#f4a582", "#ca0020"),
  RdGy6 = c("#4d4d4d", "#999999", "#e0e0e0", "#fddbc7", "#ef8a62", "#b2182b"),
  RdGy7 = c("#4d4d4d", "#999999", "#e0e0e0", "#ffffff", "#fddbc7", "#ef8a62", "#b2182b"),
  RdGy8 = c("#4d4d4d", "#878787", "#bababa", "#e0e0e0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b"),
  RdGy9 = c("#4d4d4d", "#878787", "#bababa", "#e0e0e0", "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b"),
  RdGy10 = c("#1a1a1a", "#4d4d4d", "#878787", "#bababa", "#e0e0e0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
  RdGy11 = c("#1a1a1a", "#4d4d4d", "#878787", "#bababa", "#e0e0e0", "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
  YlOrRd3 = c("#f03b20", "#feb24c", "#ffeda0"),
  YlOrRd4 = c("#e31a1c", "#fd8d3c", "#fecc5c", "#ffffb2"),
  YlOrRd5 = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c", "#ffffb2"),
  YlOrRd6 = c("#bd0026", "#f03b20", "#fd8d3c", "#feb24c", "#fed976", "#ffffb2"),
  YlOrRd7 = c("#b10026", "#e31a1c", "#fc4e2a", "#fd8d3c", "#feb24c", "#fed976", "#ffffb2"),
  YlOrRd8 = c("#b10026", "#e31a1c", "#fc4e2a", "#fd8d3c", "#feb24c", "#fed976", "#ffeda0", "#ffffcc"),
  YlOrRd9 = c("#800026", "#bd0026", "#e31a1c", "#fc4e2a", "#fd8d3c", "#feb24c", "#fed976", "#ffeda0", "#ffffcc"),
  PuOr3 = c("#998ec3", "#f7f7f7", "#f1a340"),
  PuOr4 = c("#5e3c99", "#b2abd2", "#fdb863", "#e66101"),
  PuOr5 = c("#5e3c99", "#b2abd2", "#f7f7f7", "#fdb863", "#e66101"),
  PuOr6 = c("#542788", "#998ec3", "#d8daeb", "#fee0b6", "#f1a340", "#b35806"),
  PuOr7 = c("#542788", "#998ec3", "#d8daeb", "#f7f7f7", "#fee0b6", "#f1a340", "#b35806"),
  PuOr8 = c("#542788", "#8073ac", "#b2abd2", "#d8daeb", "#fee0b6", "#fdb863", "#e08214", "#b35806"),
  PuOr9 = c("#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863", "#e08214", "#b35806"),
  PuOr10 = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#fee0b6", "#fdb863", "#e08214", "#b35806", "#7f3b08"),
  PuOr11 = c("#2d004b", "#542788", "#8073ac", "#b2abd2", "#d8daeb", "#f7f7f7", "#fee0b6", "#fdb863", "#e08214", "#b35806", "#7f3b08"),
  PuRd3 = c("#dd1c77", "#c994c7", "#e7e1ef"),
  PuRd4 = c("#ce1256", "#df65b0", "#d7b5d8", "#f1eef6"),
  PuRd5 = c("#980043", "#dd1c77", "#df65b0", "#d7b5d8", "#f1eef6"),
  PuRd6 = c("#980043", "#dd1c77", "#df65b0", "#c994c7", "#d4b9da", "#f1eef6"),
  PuRd7 = c("#91003f", "#ce1256", "#e7298a", "#df65b0", "#c994c7", "#d4b9da", "#f1eef6"),
  PuRd8 = c("#91003f", "#ce1256", "#e7298a", "#df65b0", "#c994c7", "#d4b9da", "#e7e1ef", "#f7f4f9"),
  PuRd9 = c("#67001f", "#980043", "#ce1256", "#e7298a", "#df65b0", "#c994c7", "#d4b9da", "#e7e1ef", "#f7f4f9"),
  Blues3 = c("#3182bd", "#9ecae1", "#deebf7"),
  Blues4 = c("#2171b5", "#6baed6", "#bdd7e7", "#eff3ff"),
  Blues5 = c("#08519c", "#3182bd", "#6baed6", "#bdd7e7", "#eff3ff"),
  Blues6 = c("#08519c", "#3182bd", "#6baed6", "#9ecae1", "#c6dbef", "#eff3ff"),
  Blues7 = c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#eff3ff"),
  Blues8 = c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#deebf7", "#f7fbff"),
  Blues9 = c("#08306b", "#08519c", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#deebf7", "#f7fbff"),
  PuBuGn3 = c("#1c9099", "#a6bddb", "#ece2f0"),
  PuBuGn4 = c("#02818a", "#67a9cf", "#bdc9e1", "#f6eff7"),
  PuBuGn5 = c("#016c59", "#1c9099", "#67a9cf", "#bdc9e1", "#f6eff7"),
  PuBuGn6 = c("#016c59", "#1c9099", "#67a9cf", "#a6bddb", "#d0d1e6", "#f6eff7"),
  PuBuGn7 = c("#016450", "#02818a", "#3690c0", "#67a9cf", "#a6bddb", "#d0d1e6", "#f6eff7"),
  PuBuGn8 = c("#016450", "#02818a", "#3690c0", "#67a9cf", "#a6bddb", "#d0d1e6", "#ece2f0", "#fff7fb"),
  PuBuGn9 = c("#014636", "#016c59", "#02818a", "#3690c0", "#67a9cf", "#a6bddb", "#d0d1e6", "#ece2f0", "#fff7fb")
)

bk_gradient_palette_names <- names(bk_gradient_palettes)

