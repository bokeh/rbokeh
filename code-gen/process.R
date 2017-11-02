# # run
# git fetch --all --tags --prune
# git checkout tags/0.12.10 -b 0.12.10
# python ~/Documents/Code/_cloned/bokeh/scripts/spec.py > ~/Documents/Code/rbokeh/code-gen/spec_0.12.10.json

source("code-gen/fns.R")

mods <- get_mod_json("code-gen/spec_0.12.10.json")

## write out R6 class code for all models
##---------------------------------------------------------

cat(get_class_string("Model", mods), file = "R/bk_model_autogen.R")
for (nm in setdiff(names(mods), "Model")) {
  message(nm)
  cat(get_class_string(nm, mods), file = "R/bk_model_autogen.R", append = TRUE)
}

# load_all()

## generate a different data structure to be used for
## parameter validation
##---------------------------------------------------------

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

bk_prop_types$BoxAnnotation$top$type <- "Float"
bk_prop_types$BoxAnnotation$bottom$type <- "Float"
bk_prop_types$BoxAnnotation$left$type <- "Float"
bk_prop_types$BoxAnnotation$right$type <- "Float"
# this is incorrect in the provided spec
bk_prop_types$ImageURL$url$type <- "StringSpec(String, Dict(Enum('field', 'value', 'transform'), Either(String, Instance(Transform), List(String))), List(String))"

save(bk_prop_types, file = "inst/bk_prop_types.rda", compress = "bzip2")
