
#' Export rbokeh plot to a gist
#'
#' @param fig a string containing R code to create an rbokeh figure
#' @param name name of the gist
#' @param created optional string for a "Created by" to preceed the README
#' @param description text to go in README.md
#' @note
#' This requires that you have a github personal access token stored as an environment variable \code{GITHUB_PAT}.  See \code{\link{gist_create}} for more information.
#'
#' Also note that this currently can't handle thumbnails but will find a way to do that soon.
#' @importFrom gistr gist_create
#' @export
rbokeh2gist <- function(fig, name, created = NULL, description = "") {

  # remove leading and trailing newlines
  fig <- gsub("\n+$", "", fig)
  fig <- gsub("^\n+", "", fig)

  p <- eval(parse(text = fig))

  dir <- tempfile(pattern = "rbokeh_gist_", fileext = "")
  dir.create(dir)

  cat(rbokeh2html(p), file = file.path(dir, "index.html"))

  # save_figure(p, file = file.path(dir, "thumbnail.png"))

  readme <- NULL
  if(!is.null(created))
    readme <- c(readme, paste0("Created by ", created), "")

  readme <- c(readme, description, "", "```r", fig, "```")

  writeLines(paste(readme, collapse = "\n"), file.path(dir, "README.md"))

  ff <- list.files(dir, full.names = TRUE)

  gistr::gist_create(files = ff, description = name)
}


