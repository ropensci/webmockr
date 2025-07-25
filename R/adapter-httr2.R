httr2_headers <- function(x) {
  structure(x %||% list(), class = "httr2_headers")
}

tryx <- function(exp, give = NULL) {
  z <- tryCatch(exp, error = function(e) e)
  if (inherits(z, "error")) give else z
}

#' Build a httr2 response (`httr2_response`)
#' @export
#' @keywords internal
#' @param req a request
#' @param resp a response
#' @return an httr2 response (`httr2_response`)
build_httr2_response <- function(req, resp) {
  bd <- resp$body %||% resp$content
  lst <- list(
    method = req_method_get_w(req),
    url = tryCatch(resp$url, error = function(e) e) %|s|% req$url,
    status_code = as.integer(
      tryx(resp$status_code$status_code) %||%
        tryx(resp$status_code) %||%
        resp$status$status_code
    ),
    headers = {
      if (grepl("^ftp://", resp$url %||% "")) {
        # in case uri_regex only
        httr2_headers(list())
      } else {
        httr2_headers(resp$headers %||% resp$response_headers)
      }
    },
    body = tryx(charToRaw(bd)) %||% bd,
    request = req,
    cache = new.env()
  )
  structure(lst, class = "httr2_response")
}

req_method_get_w <- function(req) {
  if (!is.null(req$method)) {
    req$method
  } else if ("nobody" %in% names(req$options)) {
    "HEAD"
  } else if (!is.null(req$body)) {
    "POST"
  } else {
    "GET"
  }
}

#' Build an httr2 request
#' @export
#' @keywords internal
#' @param x an unexecuted httr2 request object
#' @return a `httr2_request`
build_httr2_request <- function(x) {
  headers <- as.list(x$headers) %||% NULL
  auth <- check_user_pwd(x$options$userpwd) %||% NULL
  if (!is.null(auth)) {
    auth_header <- prep_auth(auth)
    headers <- c(headers, auth_header)
  }
  RequestSignature$new(
    method = req_method_get_w(x),
    uri = x$url,
    options = list(
      body = x$body$data,
      headers = headers,
      proxies = x$proxies %||% NULL,
      auth = auth,
      disk = x$disk %||% NULL,
      fields = x$fields %||% NULL,
      output = x$output %||% NULL
    )
  )
}

#' Turn on `httr2` mocking
#'
#' Sets a callback that routes `httr2` requests through `webmockr`
#'
#' @export
#' @param on (logical) `TRUE` to turn on, `FALSE` to turn off. default: `TRUE`
#' @return Silently returns `TRUE` when enabled and `FALSE` when disabled.
httr2_mock <- function(on = TRUE) {
  check_installed("httr2")
  if (on) {
    options(httr2_mock = function(req) Httr2Adapter$new()$handle_request(req))
  } else {
    options(httr2_mock = NULL)
  }
  invisible(on)
}

#' @rdname Adapter
#' @export
#' @keywords internal
Httr2Adapter <- R6::R6Class(
  "Httr2Adapter",
  inherit = Adapter,
  public = list(
    #' @field client HTTP client package name
    client = "httr2",
    #' @field name adapter name
    name = "Httr2Adapter"
  ),
  private = list(
    pluck_url = function(request) request$url,
    mock = function(on) httr2_mock(on),
    build_request = build_httr2_request,
    build_response = build_httr2_response,
    fetch_request = function(request) httr2::req_perform(request)
  )
)
