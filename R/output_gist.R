#' Export rbokeh plot to a gist
#'
#' @param fig_str a string containing R code to create an rbokeh figure
#' @param name name of the gist
#' @param created optional string for a "Created by" to preceed the README
#' @param description text to go in README.md
#' @param secure should https be used for cdn links?
#' @param view should the resulting gist be opened in the browser on bl.ocks.org?
#' @note
#' This requires that you have a github personal access token stored as an environment variable \code{GITHUB_PAT}.  See \code{\link[gistr]{gist_create}} for more information.
#'
#' Also note that this currently can't handle thumbnails but will find a way to do that soon.
#' @examples
#' \dontrun{
#' rbokeh2gist("figure() %>% ly_points(1:10)", name = "test")
#' }
#' @importFrom gistr gist_create
#' @export
rbokeh2gist <- function(fig_str, name, created = NULL, description = "", secure = TRUE, view = TRUE) {

  if(!is.character(fig_str))
    stop("Argument 'fig_str' must be a string specifying an rbokeh plot")

  # remove leading and trailing newlines
  fig_str <- gsub("\n+$", "", fig_str)
  fig_str <- gsub("^\n+", "", fig_str)

  p <- eval(parse(text = fig_str))

  if(!inherits(p, "rbokeh"))
    stop("Argument 'fig_str' must be a string specifying an rbokeh plot")

  dir <- tempfile(pattern = "rbokeh_gist_", fileext = "")
  dir.create(dir)

  rbokeh2html(p, file = file.path(dir, "index.html"), secure = secure)

  # save_figure(p, file = file.path(dir, "thumbnail.png"))

  readme <- NULL
  if(!is.null(created))
    readme <- c(readme, paste0("Created by ", created), "")

  readme <- c(readme, description, "", "```r", fig_str, "```")

  writeLines(paste(readme, collapse = "\n"), file.path(dir, "README.md"))

  ff <- list.files(dir, full.names = TRUE)

  gst <- gistr::gist_create(files = ff, description = name, browse = FALSE)

  message("* Browse this gist on github:")
  message("   ", gst$html_url)
  bl_ocks <- paste0("http://bl.ocks.org/", gst$owner$login, "/", gst$id)
  message("* View or share this gist on bl.ocks.org:")
  message("   ", bl_ocks)
  message("* Embed in an iframe:")
  # emb <- paste0("http://bl.ocks.org/", gst$owner$login, "/raw/", gst$id)
  index_raw <- gst$files[["index.html"]]$raw_url
  index_cdn <- gsub("gist\\.githubusercontent\\.com", "cdn.rawgit.com", index_raw)
  message(paste0('   <iframe width="', p$width + 20, '" height="', p$height + 20, '" frameBorder="0" webkitallowfullscreen="" mozallowfullscreen="" allowfullscreen="" sandbox="allow-forms allow-scripts allow-popups allow-same-origin allow-pointer-lock" src="', index_cdn,'"></iframe>'))

  if(view)
    browseURL(bl_ocks)

  return(invisible(gst))
}
