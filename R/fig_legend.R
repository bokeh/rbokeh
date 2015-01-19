# to add a legend:
# - create a legend model
#   - add a model reference
#   - add a list of legend items

# individual legend items are lists with a label
# and a list of glyphRenderers that the legend
# entry refers to

# should take a vector of group IDs
# for each group, find the layers that are in the group
# and add a legend entry for each
addLegend <- function(obj, legends) {

  id <- genId(obj, "legend")

  leg <- legendModel(id, obj$ref, legends)

  obj$model$plot$attributes$renderers[[id]] <- leg$ref
  obj$model[[id]] <- leg$model

  obj
}

legendModel <- function(id, plotRef, legends) {
  res <- baseModelObject("Legend", id)
  res$model$attributes$plot <- plotRef
  res$model$attributes$legends <- legends
  res
}

# addLegend(p)

# addLegend <- function(obj, groupIDs = NULL, groupNames = NULL) {

#   # for now
#   groupIDs <- names(which(sapply(obj$model, function(x) x$type) == "GlyphRenderer"))

#   groupNames <- letters[seq_along(groupIDs)]

#   id <- genId(obj, c(groupNames, "legend"))

#   legends <- lapply(seq_along(groupIDs), function(i) {
#     c(list(groupNames[i]), list(unname(lapply(obj$model[groupIDs[i]], function(x) {
#       list(type = x$type, id = x$id)
#     }))))
#   })

#   leg <- legendModel(id, obj$ref, legends)

#   obj$model$plot$attributes$renderers[[id]] <- leg$ref
#   obj$model[[id]] <- leg$model

#   obj
# }
