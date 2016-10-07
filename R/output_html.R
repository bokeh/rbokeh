#' Get the HTML content required to embed a Bokeh figure
#' @param fig figure
#' @param file html file name to write the figure to
#' @param pretty should the json model be pretty printed to the html file?
#' @param secure should https be used for cdn links?
#' @examples
#' p <- figure() %>% ly_points(1:10)
#' rbokeh2html(p)
#' @export
rbokeh2html <- function(fig, file = tempfile(fileext = ".html"), pretty = FALSE,
  secure = TRUE) {

  if (is.null(fig$width))
    fig$width <- 500
  if (is.null(fig$height))
    fig$height <- 500

  width <- fig$width
  height <- fig$height

  fig <- rbokeh_prerender(fig)
  modelid <- fig$x$modelid
  elementid <- fig$x$elementid
  docid <- fig$x$docid

  fig <- jsonlite::toJSON(fig$x$docs_json, pretty = pretty,
    dataframe = "columns", null = "null", na = "null", auto_unbox = TRUE,
    digits = getOption("shiny.json.digits", 16), use_signif = TRUE,
    force = TRUE, POSIXt = "ISO8601", UTC = TRUE, rownames = FALSE,
    keep_vec_names = TRUE)

  ver <- get_bokeh_version()

  sc <- ifelse(secure, "s", "")

  a <- paste0("<!DOCTYPE html>
<html>
<head>
<script src='http", sc, "://cdn.pydata.org/bokeh/release/bokeh-", ver, ".min.js'></script>
<link href='http", sc, "://cdn.pydata.org/bokeh/release/bokeh-", ver, ".min.css' rel='stylesheet'>
</head>
<body>
<div class='bk-root' class='plotdiv' style=' width: ",
  width, "px; height: ", height, "px;'>
<div id='", elementid, "' class='plotdiv'></div>
<div>
<script type='text/javascript'>
Bokeh.$(function() {
  var modelid = '", modelid, "';
  var elementid = '", elementid, "';
  var docid = '", docid, "';
  var docs_json = ", fig, ";
  var refkey = Object.keys(docs_json)[0]
  var refs = docs_json[refkey].roots.references
  function traverseObject(obj) {
    for (var key in obj) {
      if (obj[key].constructor === Object) {
        traverseObject(obj[key]);
      } else if (obj[key].constructor === Array) {
        for (var i = 0; i < obj[key].length; i++) {
          if (obj[key][i] === null)
            obj[key][i] = NaN;
        };
      }
    };
  }
  for (var i = 0; i < refs.length; i++) {
    if (refs[i].type === 'ColumnDataSource')
      traverseObject(refs[i].attributes.data);
  };
  var render_items = [{
    'docid': docid,
    'elementid': elementid,
    'modelid': modelid
  }];
  Bokeh.set_log_level('info');
  Bokeh.embed.embed_items(docs_json, render_items);
});
</script>
</body>
</html>")

  cat(a, file = file)
  message("html file written to: ", file)

  return(invisible(file))
}
