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

allvals_orig <- unlist(lapply(mods, function(el) {
  lapply(el$props, function(x) x$type)
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

# look at seq
idx <- grepl("^Seq", allvals)
sq <- allvals[idx]
allvals[idx] <- "Seq"

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
