#' Export htmlwidget plot to a gist
#'
#' @param widget_string a string containing R code to create an htmlwidget
#' @param name name of the gist
#' @param created optional string for a "Created by" to preceed the README
#' @param description optional text to go in README.md to describe the gist
#' @param license license under which gist is released - one of those accepted here: \url{http://bl.ocks.org/licenses.txt}
#' @param border should the bl.ocks.org iframe have a border?
#' @param scrolling should the bl.ocks.org iframe scroll?
#' @param secure should https be used for cdn links?
#' @param view should the resulting gist be opened in the browser on bl.ocks.org?
#' @note
#' This requires that you have a github personal access token stored as an environment variable \code{GITHUB_PAT}.  See \code{\link[gistr]{gist_create}} for more information.
#'
#' Also note that this currently can't handle thumbnails but we are looking into ways to do that.
#' @examples
#' \dontrun{
#' widget2gist("figure() %>% ly_points(1:10)", name = "test")
#' }
#' @importFrom gistr gist_create
#' @importFrom utils browseURL
#' @export
widget2gist <- function(widget_string, name,
  created = NULL, description = "",
  license = c("none", "apache-2.0", "bsd-2-clause", "bsd-3-clause", "cc-by-4.0", "cc-by-nc-4.0", "cc-by-nc-nd-4.0", "cc-by-nc-sa-4.0", "cc-by-nd-4.0", "cc-by-sa-4.0", "cddl-1.0", "epl-1.0", "gpl-2.0", "gpl-3.0", "lgpl-2.1", "lgpl-3.0", "mit", "mpl-2.0"),
  border = TRUE, scrolling = FALSE,
  secure = TRUE, view = TRUE) {

  if(!is.character(widget_string))
    stop("Argument 'widget_string' must be a string specifying an htmlwidget")

  license <- match.arg(license)

  # remove leading and trailing newlines
  widget_string <- gsub("\n+$", "", widget_string)
  widget_string <- gsub("^\n+", "", widget_string)

  p <- eval(parse(text = widget_string))

  dir <- tempfile(pattern = "widget_gist_", fileext = "")
  dir.create(dir)

  if(inherits(p, "rbokeh")) {
    rbokeh2html(p, file = file.path(dir, "index.html"), secure = secure)
    # widget2png(p, file = file.path(dir, "thumbnail.png"))
  } else if(inherits(p, "htmlwidget")) {
    try_res <- try(htmlwidgets::saveWidget(p,
      file = file.path(dir, "index.html"),
      selfcontained = TRUE))
    if(inherits(try_res, "error"))
      stop("Widget could not be saved as a self contained file.")
    unlink(file.path(dir, "index_files"), recursive = TRUE)
  }

  readme <- NULL
  if(!is.null(created))
    readme <- c(readme, paste0("Created by ", created), "")
  readme <- c(readme, description, "", "```r", widget_string, "```")
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

  if(view) {
    Sys.sleep(0.5) # wait so it has time to populate
    utils::browseURL(bl_ocks)
  }

  return(invisible(gst))
}
