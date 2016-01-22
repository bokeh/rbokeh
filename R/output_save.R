#' Make a static png file for an rbokeh figure
#'
#' @param p Bokeh figure object
#' @param file where to save png file
#' @param timeout plot render timeout in milliseconds (see details)
#' @details This uses phantomjs (\url{http://phantomjs.org}) to render your rbokeh figure in a headless browser and take a screenshot of it, creating a static output.  This assumes that phantomjs has been installed on your machine and is available as a system call.  For plots that take longer to load and render, you may need to increase the value of \code{timeout}.  Note that this function is experimental.
#' @examples
#' \donttest{
#' figure(tools = NULL) %>%
#'   ly_points(1:10) %>%
#'   save_figure("/tmp/test.png")
#' }
#' @export
save_figure <- function(p, file, timeout = 500) {
  phantom <- find_phantom()
  file <- path.expand(file)

  if(phantom == "") {
    message("** phantomjs dependency could not be found - static plot cannot be generated (run phantom_install() for details)")
  } else {
    res <- try({
      ff <- tempfile(fileext = ".html")
      ffjs <- tempfile(fileext = ".js")

      # don't want any padding
      p$sizingPolicy$padding <- 0
      # suppressMessages(saveWidget(p, ff, selfcontained = FALSE))
      suppressMessages(rbokeh2html(p, file = ff))

      js <- paste0("var page = require('webpage').create();
page.open('file://", ff, "', function() {
  // $('html').style.zoom = 2;
  window.setTimeout(function () {
    page.render('", file, "');
    phantom.exit();
  }, ", timeout, ");
});")
      cat(js, file = ffjs)
      system2(phantom, ffjs)
    })
    if(inherits(res, "try-error"))
      message("** could not create static plot...")

    # system(paste("open ", ffjs))
    # system(paste("open ", dirname(ffjs)))
  }
}

#' Instructions for installing phantomjs
#' @export
phantom_install <- function() {
  message("Please visit this page to install phantomjs on your system: http://phantomjs.org/download.html")
}

# similar to webshot
find_phantom <- function() {
  phantom <- Sys.which("phantomjs")
  if(Sys.which("phantomjs") == "") {
    if(identical(.Platform$OS.type, "windows")) {
      phantom <- Sys.which(file.path(Sys.getenv("APPDATA"), "npm", "phantomjs.cmd"))
    }
  }
  phantom
}
