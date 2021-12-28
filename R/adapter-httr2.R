#' Build a httr2 response (`httr2_response`)
#' @export
#' @param req a request
#' @param resp a response
#' @return an httr2 response (`httr2_response`)

build_httr2_response <- function(req, resp) {
  lst <- list(
    method = req$method,
    url = tryCatch(resp$url, error = function(e) e) %|s|% req$url,
    status_code = as.integer(resp$status_code),
    headers = {
      if (grepl("^ftp://", resp$url %||% "")) { # in case uri_regex only
        list()
      } else {
        unclass(resp$headers)
      }
    },
    body = resp$body
  )
  structure(lst, class = "httr2_response")
}

pluck_httr2_body <- function(x) {

}

#' Build a httr2 request
#' @export
#' @param x an unexecuted httr request object
#' @return a httr request
build_httr2_request = function(x) {
  headers <- as.list(x$headers) %||% NULL
  auth <- check_user_pwd(x$options$userpwd) %||% NULL
  if (!is.null(auth)) {
    auth_header <- prep_auth(auth)
    headers <- c(headers, auth_header)
  }
  RequestSignature$new(
    method = x$method,
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

# library(httr2)
# req <- request("https://r-project.org")
# req = req %>% req_body_json(list(x = 1, y = 2))
# req$method <- 'POST'
# enable()
# stub_registry_clear()
# stub_request("post", "https://r-project.org") %>% 
#   to_return(status = 200, body = "{a: 5}")
# getOption("httr2_mock", NULL)
# options(httr2_mock = webmockr_handle)
# getOption("httr2_mock", NULL)
# httr2::req_perform(req)

#' Turn on httr2 mocking
#' @export
#' @param on (logical) `TRUE` to turn on, `FALSE` to turn off. default: `TRUE`
#' @return Silently returns `TRUE` when enabled and `FALSE` when disabled.
httr2_mock <- function(on = TRUE) {
  check_for_pkg("httr2")
  webmockr_handle <- function(req) {
    webmockr::Httr2Adapter$new()$handle_request(req)
  }
  if (on) {
    httr2::local_mock(~ webmockr_handle())
    httr2::local_mock(~ webmockr::Httr2Adapter$new()$handle_request(req))
    httr2::with_mock(webmockr_handle, httr2::req_perform(req))
    # rlang::as_function(webmockr_handle)
  } else {
    httr::with_mock("request", NULL)
  }
  invisible(on)
}

#' @rdname Adapter
#' @export
Httr2Adapter <- R6::R6Class("Httr2Adapter",
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

    build_request   = build_httr2_request,
    build_response  = build_httr2_response,

    request_handler = function(request) vcr::RequestHandlerHttr$new(request),

    fetch_request = function(request) httr2::req_perform(request)
  )
)
