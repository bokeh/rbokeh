#' Export rbokeh plot to a gist
#'
#' @param fig_str a string containing R code to create an rbokeh figure
#' @param name name of the gist
#' @param created optional string for a "Created by" to preceed the README
#' @param description text to go in README.md
#' @param license license under which gist is released - one of those accepted here: \url{http://bl.ocks.org/licenses.txt}
#' @param border should the bl.ocks.org iframe have a border?
#' @param scrolling should the bl.ocks.org iframe scroll?
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
#' @importFrom utils browseURL
#' @export
rbokeh2gist <- function(fig_str, name,
  created = NULL, description = "",
  license = c("none", "apache-2.0", "bsd-2-clause", "bsd-3-clause", "cc-by-4.0", "cc-by-nc-4.0", "cc-by-nc-nd-4.0", "cc-by-nc-sa-4.0", "cc-by-nd-4.0", "cc-by-sa-4.0", "cddl-1.0", "epl-1.0", "gpl-2.0", "gpl-3.0", "lgpl-2.1", "lgpl-3.0", "mit", "mpl-2.0"),
  border = TRUE, scrolling = FALSE,
  secure = TRUE, view = TRUE) {

  if(!is.character(fig_str))
    stop("Argument 'fig_str' must be a string specifying an rbokeh plot")

  license <- match.arg(license)

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

  yaml <- c(
    paste0("license: ", license),
    paste0("height: ", p$height),
    paste0("scrolling: ", ifelse(scrolling, "yes", "no")),
    paste0("border: ", ifelse(border, "yes", "no"))
  )
  writeLines(yaml, file.path(dir, ".block"))

  ff <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
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
    utils::browseURL(bl_ocks)

  return(invisible(gst))
}
