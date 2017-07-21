#' Specify an Ajax data source
#'
#' @param data_url The URL to the endpoint for the data.
#' @param polling_interval Polling interval for updating data source in milliseconds.
#' @param method http method - "GET" or "POST"
#' @param mode Whether to append new data to existing data (up to "max_size"), or to replace existing data entirely. Must be one of "replace" or "append".
#' @param max_size Maximum size of the data array being kept after each pull requests. Larger than that size, the data will be right shifted.
#' @param content_type Set the "contentType" parameter for the Ajax request.
#' @param if_modified Whether to include an "If-Modified-Since" header in AJAX requests to the server. If this header is supported by the server, then only new data since the last request will be returned.
# > Enum('replace', 'append')
#' @param http_headers Named list of HTTP headers to set for the Ajax request.
#' @examples
#' d <- ajax_data("http://127.0.0.1:8080")
#' \dontrun{
#' figure() %>%
#'   ly_points(x, y, data = d)
#' }
#' @export
ajax_data <- function(data_url, polling_interval = 100, method = "GET",
  mode = "replace", max_size = NULL, content_type = NULL, if_modified = NULL,
  http_headers = NULL) {

  # TODO: add col_types argument so users can specify if it should be treated as a date/time
  # then honor this in get_ajax_sample so we can get correct axis types, etc.

  args <- list()

  args$data_url <- data_url
  args$polling_interval <- polling_interval
  args$method <- method
  args$mode <- mode
  args$max_size <- max_size
  args$content_type <- content_type
  args$if_modified <- if_modified
  args$http_headers <- http_headers

  class(args) <- c("ajax_data", "list")
  args
}

#' Get a Sample of Data from Ajax Data Source
#'
#' @param x object obtained from \code{\link{ajax_data}}.
#' @examples
#' d <- ajax_data("http://127.0.0.1:8080")
#' \dontrun{
#' get_ajax_sample(d)
#' }
#' @export
#' @importFrom curl new_handle handle_setheaders curl_fetch_memory
get_ajax_sample <- function(x) {
  hdr <- x$http_headers
  if (is.null(hdr))
    hdr <- list()
  if (!is.null(x$content_type))
    hdr[["Content-Type"]] <- x$content_type

  h <- curl::new_handle()
  if (length(hdr) > 0)
    do.call(curl::handle_setheaders, c(list(h = h), hdr))
  # handle_data(h)

  req <- try(curl::curl_fetch_memory(x$data_url, handle = h), silent = TRUE)

  if (inherits(req, "try-error")) {
    # TODO: check status_code
    # parse_headers(req$headers)
    stop("Could not connect to Ajax data source: ", x$data_url, call. = FALSE)
  }

  data.frame(jsonlite::fromJSON(rawToChar(req$content)), stringsAsFactors = FALSE)
}
