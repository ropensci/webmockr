#' Build a crul response
#' @export
#' @keywords internal
#' @param req a request
#' @param resp a response
#' @return a crul response
build_crul_response <- function(req, resp) {
  # prep headers
  if (grepl("^ftp://", resp$url %||% "")) {
    # in case uri_regex only
    headers <- list()
  } else {
    hds <- resp$headers
    if (is.null(hds)) {
      hds <- resp$response_headers
      headers <- if (is.null(hds)) {
        list()
      } else {
        stopifnot(is.list(hds))
        stopifnot(is.character(hds[[1]]))
        hds
      }
    } else {
      hh <- rawToChar(hds %||% raw(0))
      if (is.null(hh) || nchar(hh) == 0) {
        headers <- list()
      } else {
        headers <- lapply(
          curl::parse_headers(hh, multiple = TRUE),
          crul_headers_parse
        )
      }
    }
  }

  crul::HttpResponse$new(
    method = req$method,
    # if resp URL is empty, use URL from request
    url = resp$url %||% req$url$url,
    status_code = resp$status_code,
    request_headers = c("User-Agent" = req$options$useragent, req$headers),
    response_headers = {
      if (all(hz_namez(headers))) headers else last(headers)
    },
    response_headers_all = headers,
    modified = resp$modified %||% NA,
    times = resp$times,
    content = resp$content,
    handle = req$url$handle,
    request = req
  )
}

#' Build a crul request
#' @keywords internal
#' @param x an unexecuted crul request object
#' @return a crul request
build_crul_request <- function(x) {
  headers <- x$headers %||% NULL
  auth <- check_user_pwd(x$options$userpwd) %||% NULL
  if (!is.null(auth)) {
    auth_header <- prep_auth(auth)
    headers <- c(headers, auth_header)
  }
  RequestSignature$new(
    method = x$method,
    uri = x$url$url,
    options = list(
      body = pluck_body(x),
      headers = headers,
      proxies = x$proxies %||% NULL,
      auth = auth,
      disk = x$disk %||% NULL
    )
  )
}

crul_mock <- function(on = TRUE) {
  check_installed("crul")
  if (on) {
    options(crul_mock = function(req) {
      webmockr::CrulAdapter$new()$handle_request(req)
    })
  } else {
    options(crul_mock = NULL)
  }
  invisible(on)
}

#' @rdname Adapter
#' @export
#' @keywords internal
CrulAdapter <- R6::R6Class(
  "CrulAdapter",
  inherit = Adapter,
  public = list(
    #' @field client HTTP client package name
    client = "crul",
    #' @field name adapter name
    name = "CrulAdapter"
  ),
  private = list(
    pluck_url = function(request) request$url$url,
    # mock = function(on) crul::mock(on),
    mock = function(on) crul_mock(on),
    build_request = build_crul_request,
    build_response = build_crul_response,
    fetch_request = function(request) {
      private$build_response(request, webmockr_crul_fetch(request))
    }
  )
)
