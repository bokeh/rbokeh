source("code-gen/fns.R")

mods <- get_mod_json()

bk_prop_types <- lapply(mods, function(x) {
  if (x$name == "Base")
    return(NULL)
  attrs <- x$props
  names(attrs) <- unlist(lapply(attrs, function(x) x$name))
  base_attrs <- mods[[get_inherit(x$base[[1]])]]$props
  names(base_attrs) <- unlist(lapply(base_attrs, function(x) x$name))
  attrs <- attrs[setdiff(names(attrs), names(base_attrs))]

  # lapply(attrs, function(x) x$type)
  lapply(attrs, function(x) list(type = x$type, default = x$default))
})
use_data(bk_prop_types, overwrite = TRUE)

cat(get_class_string("Model", mods), file = "R/model_autogen.R")
for (nm in setdiff(names(mods), "Model")) {
  message(nm)
  cat(get_class_string(nm, mods), file = "R/model_autogen.R", append = TRUE)
}

load_all()

# written <- 1
# bases <- unlist(lapply(mods, function(x) get_inherit(x$base)))
# level1 <- names(bases[bases == "Model"])
# for(nm in level1) {
#   cat(paste(paste0("## ", nm),
#   "##---------------------------------------------------------", sep = "\n"))
# }

# model <- Model$new()
# model$to_json(pretty = TRUE)

# cm <- ColorMapper$new()
# cm$to_json(pretty = TRUE)

# ccm <- CategoricalColorMapper$new(factors = letters)
# ccm$to_json(pretty = TRUE)

# axs <- LinearAxis$new()

# axs$to_json(pretty = TRUE)


# get_default_props <- function(x) {
#   res <- lapply(x$props, function(a) a$default)
#   names(res) <- unlist(lapply(x$props, function(a) a$name))
#   res[order(names(res))]
# }

# jsonlite::toJSON(get_default_props(mods$Model), auto_unbox = TRUE, null = "null", pretty = TRUE)
