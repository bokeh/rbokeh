source("code-gen/fns.R")

mods <- get_mod_json("code-gen/spec_0.12.6.json")

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

# are we missing entries for any of these?
setdiff(allclasses, allnames)
# [1] "AbstractButton1" "AbstractButton2" "ButtonGroup1"    "ButtonGroup2"
# [5] "ButtonLike"

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

visSave(p, file = paste0(getwd(), "/docs-dev/bokehclasses.html"), selfcontained = TRUE)
