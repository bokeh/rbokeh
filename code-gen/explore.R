source("code-gen/fns.R")

mods <- get_mod_json()

names(mods)
names(mods[[1]])
length(mods)

# look at base class lengths...
base_length <- unlist(lapply(mods, function(x) length(x$bases)))
zero_length <- which(base_length == 0)
multi_length <- which(base_length > 1)
# AbstractButton ButtonGroup Model

glphmk <- which(unlist(lapply(mods, function(x) any(x$bases2 %in% c("Glyph", "Marker")))))

## do all have type?
##---------------------------------------------------------

sort(unlist(lapply(mods, function(a)
  length(which(!unlist(lapply(a$props, function(x) length(x$type))) > 0)))))
# yes

## does each unique variable name have the same type?
##---------------------------------------------------------

type_res <- list()
for (aa in mods) {
  for (atr in aa$props) {
    nm <- atr$name
    if (is.null(type_res[[atr$name]])) {
      type_res[[atr$name]] <- list(atr$type)
    } else {
      type_res[[atr$name]] <- c(type_res[[atr$name]], list(atr$type))
    }
  }
}

sort(sapply(type_res, length))
sort(sapply(type_res, function(x) length(which(!duplicated(x)))))

type_res$bounds
type_res$xs
type_res$ys



##
##---------------------------------------------------------

# allvals_orig <- unlist(lapply(mods, function(el) {
allvals_orig <- unlist(lapply(unname(mods), function(el) {
  res <- lapply(el$props, function(x) x$type)
  names(res) <- sapply(el$props, function(x) x$name)
  res
}))

allvals <- allvals_orig

# treat all enums as one and all instances as one just to get an idea
idx <- grepl("^Enum", allvals)
enums <- allvals[idx]
enums <- gsub(" $", "", gsub("^Enum \\( (.*)\\ +)$", "\\1", enums))
enumstbl <- sort(table(enums))
allvals[idx] <- "Enum"

# look at instances
idx <- grepl("^Instance", allvals)
inst <- allvals[idx]
allvals[idx] <- "Instance"

# look at dicts
idx <- grepl("^Dict", allvals)
dict <- allvals[idx]
# it's always dict ( String ,) which is great (named list)
allvals[idx] <- "Dict"

# look at lists
idx <- grepl("^List", allvals)
lst <- allvals[idx]
allvals[idx] <- "List"

# look at either
idx <- grepl("^Either", allvals)
eth <- allvals[idx]
# it's always dict ( String ,) which is great (named list)
allvals[idx] <- "Either"

# look at tuple
idx <- grepl("^Tuple", allvals)
tpl <- allvals[idx]
# Tuples are always two of the same data type
# RelativeDelta???
allvals[idx] <- "Tuple"

# look at tuple
idx <- grepl("^RelativeDelta", allvals)
rld <- allvals[idx]
allvals[idx] <- "RelativeDelta"

# look at columndata
idx <- grepl("^ColumnData", allvals)
cld <- allvals[idx]
allvals[idx] <- "ColumnData"

# look at MinMaxBounds
idx <- grepl("^MinMaxBounds", allvals)
mmb <- allvals[idx]

# look at seq
idx <- grepl("^Seq", allvals)
sq <- allvals[idx]
allvals[idx] <- "Seq"

# look at spec
idx <- grepl("Spec\\(", allvals)
sp <- allvals[idx]
sort(table(sp))

# look at NumberSpec
idx <- grepl("^NumberSpec", allvals)
ns <- allvals[idx]
sort(table(ns))
# all NumberSpec are:
# NumberSpec(String, Dict(String, Either(String, Instance(Transform), Instance(ColorMapper), Float)), Float)
names(sort(table(names(ns)), decreasing = TRUE))
# [1] "line_alpha"             "line_width"             "x"
# [4] "y"                      "fill_alpha"             "background_fill_alpha"
# [7] "major_label_text_alpha" "major_tick_line_alpha"  "major_tick_line_width"
# ...

# "line_alpha": {
#   "value": 0.1
# }
# "x": {
#   "field": "weight"
# }
# "fill_color": {
#   "field": "label",
#   "transform": {
#     "id": "6d41a3c6-f278-4965-83be-9b34dbb59110",
#     "type": "CategoricalColorMapper"
#   }
# }


# look at ColorSpec
idx <- grepl("^ColorSpec", allvals)
cs <- allvals[idx]
sort(table(cs))
# all ColorSpec are:
# ColorSpec(String, Dict(String, Either(String, Instance(Transform), Instance(ColorMapper), Color)), Color)
names(sort(table(names(cs)), decreasing = TRUE))
# [1] "line_color"             "fill_color"             "background_fill_color"
# [4] "major_label_text_color" "major_tick_line_color"  "minor_tick_line_color"
# [7] "axis_label_text_color"  "axis_line_color"        "border_line_color"
# ...

# "line_color": {
#   "value": "black"
# }
# "line_color": {
#   "field": "line_color"
# }

# ScreenDistanceSpec
idx <- grepl("^ScreenDistanceSpec", allvals)
sds <- allvals[idx]
sort(table(sds))
# ScreenDistanceSpec(String, Dict(String, Either(String, Instance(Transform), Instance(ColorMapper), Float)), Float)
names(sort(table(names(sds)), decreasing = TRUE))
# [1] "size"
#  ...

# "size": {
#   "units": "screen",
#   "value": 8
# }

# FontSizeSpec
idx <- grepl("^FontSizeSpec", allvals)
fss <- allvals[idx]
sort(table(fss))
# FontSizeSpec(String, Dict(String, Either(String, Instance(Transform), Instance(ColorMapper), List(String))), List(String))
names(sort(table(names(fss)), decreasing = TRUE))
# [1] "major_label_text_font_size" "axis_label_text_font_size"
# [3] "text_font_size"             "label_text_font_size"
# [5] "title_text_font_size"
#  ...

# StringSpec
idx <- grepl("^StringSpec", allvals)
ss <- allvals[idx]
sort(table(ss))
# StringSpec(String, Dict(String, Either(String, Instance(Transform), Instance(ColorMapper), List(String))), List(String))
names(sort(table(names(ss)), decreasing = TRUE))
# [1] "text"  "label"

# DistanceSpec(units_default='data')
idx <- grepl("^DistanceSpec", allvals)
ds <- allvals[idx]
sort(table(ds))
# DistanceSpec(units_default='data')
names(sort(table(names(ds)), decreasing = TRUE))
# [1] "height"       "radius"       "width"        "dh"           "dw"
# [6] "inner_radius" "outer_radius" "h"            "length"       "w"
# ...

idx <- grepl("^AngleSpec", allvals)
ags <- allvals[idx]
sort(table(ags))
# AngleSpec(units_default='rad')
names(sort(table(names(ags)), decreasing = TRUE))
# [1] "angle"       "end_angle"   "start_angle"
# ...


# ColumnData(String, Seq(Any))


# RelativeDelta???


valtbl <- sort(table(unname(allvals)))
valtbl

excl <- c("Tuple", "Dict", "Seq", "ColumnData", "RelativeDelta", "Either", "List",
  "Instance", "Enum")
tmp <- valtbl[setdiff(names(valtbl), excl)]

# these are the basic types we need to be able to deal with
cat(paste(names(tmp), collapse = "\n"))

## see what defaults each attribute name has
##---------------------------------------------------------

dflt_res <- list()
for (aa in mods) {
  for (atr in aa$props) {
    nm <- atr$name
    if (is.null(dflt_res[[atr$type]])) {
      dflt_res[[atr$type]] <- list(atr$default)
    } else {
      dflt_res[[atr$type]] <- c(dflt_res[[atr$type]], list(atr$default))
    }
  }
}

dflt_res <- lapply(dflt_res, function(x) x[!duplicated(x)])

tmp <- unlist(lapply(dflt_res, length))

## look at graph of class inheritance
##---------------------------------------------------------

bases <- unlist(lapply(mods, function(el) el$base))
subs <- names(bases)
supers <- unname(bases)

get_bases <- function(x) {
  unname(sapply(strsplit(x, ",")[[1]], function(y) {
    tail(strsplit(y, "\\.")[[1]], 1)
  }))
}

supers <- lapply(supers, get_bases)
lns <- sapply(supers, length)
supers <- unlist(supers)
subs <- unlist(purrr::map2(subs, lns, function(x, y) rep(get_bases(x), y)))

allclasses <- sort(unique(c(subs, supers)))

allnames <- names(mods)
# these don't have base classes
leftover <- setdiff(allnames, allclasses)

allclasses <- c(allclasses, leftover)

# are we missing entries for any of these?
setdiff(allclasses, allnames)
#  [1] "AbstractButton1"         "AbstractButton2"
#  [3] "ButtonGroup1"            "ButtonGroup2"
#  [5] "ButtonLike"              "EventCallbackManager"
#  [7] "HasProps"                "Model1"
#  [9] "Model2"                  "Model3"
# [11] "PropertyCallbackManager"

nodes <- data.frame(label = allclasses, id = seq_along(allclasses))
edges <- data.frame(from = match(subs, allclasses), to = match(supers, allclasses))

nodes$has_entry <- nodes$label %in% allnames
nodes$color <- ifelse(nodes$has_entry, "#97c2fc", "gray")
edges$arrows <- "to"
nodes$highlight <- "blue"

library(visNetwork)
p <- visNetwork(nodes, edges, width = "100%", height = 1000) %>%
  visIgraphLayout(layout = "layout_nicely", randomSeed = 5432345) %>%
  # visIgraphLayout(layout = "layout_with_sugiyama") %>%
  visLayout(improvedLayout = TRUE) %>%
  visEdges(arrows = list(to = list(scaleFactor = 0.5)),
    color = list(color = "#97c2fc", alpha = 0.5, highlight = "blue")) %>%
  visNodes(color = list(highlight = "blue")) %>%
  visInteraction(dragNodes = FALSE)
  # visNodes(shape = "dot", size = 10) %>%
  # visHierarchicalLayout(direction = "LR", treeSpacing = 5000)

# visSave(p, file = "~/Desktop/bokehclasses.html", selfcontained = TRUE)
