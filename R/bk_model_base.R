get_specified_arg_names <- function(mc) {
  nm <- names(mc)
  if (length(nm) > 0)
    nm <- nm[-1]

  nm
}

#' @importFrom R6 R6Class
#' @importFrom jsonlite unbox toJSON
#' @importFrom digest digest

Base <- R6::R6Class("Base",
  public = list(
    initialize = function(id = NULL, name = NULL, tags = list(), js_callbacks = NULL) {
      if (is.null(id))
        id <- digest::digest(Sys.time())
      private$id <- validate_scalar(id, "character")
    },
    to_json = function(include_defaults = TRUE, pretty = TRUE) {
      els <- as.list(private)
      if (!include_defaults) {
        # types <- unlist(unname(bk_prop_types[setdiff(class(self), c("R6", "Base"))]),
        #   recursive = FALSE)
        # ind <- sapply(names(els), function (nm) !identical(els[[nm]], types[[nm]]$default))
        els <- els[self$specified_args]
      }
      jsonlite::toJSON(els[order(names(els))], auto_unbox = TRUE, null = "null", pretty = pretty)
    },
    set_prop = function(name, val) {
      types <- unlist(unname(bk_prop_types[setdiff(class(self), c("R6", "Base"))]),
        recursive = FALSE)
      if (name %in% names(types)) {
        private[[name]] <- validate(val, types[[name]]$type, name)
        self$specified_args <- union(self$specified_args, name)
      } else {
        message("Not able to set property \'", name, "\'")
      }
    },
    get_prop = function(name) {
      private[[name]]
    },
    get_instance = function() {
      type <- class(self)[1]
      res <- list(id = private$id, type = type)
      if (type == "Plot")
        res$subtype <- "Figure"
      res
    },
    get_all_props = function(id = TRUE) {
      res <- c(
        self$get_instance(),
        # list(attributes = as.list(private))
        list(attributes = as.list(private)[self$specified_args])
      )
      res$attributes$id <- NULL
      if (!id)
        res$id <- NULL
      res
    }
  ),
  private = list(id = NULL)
)
