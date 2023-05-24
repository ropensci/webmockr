#' Build a httr2 response (`httr2_response`)
#' @export
#' @param req a request
#' @param resp a response
#' @return an httr2 response (`httr2_response`)
#' @examples
#' x <- Httr2Adapter$new()
#' library(httr2)
#' req <- request("https://r-project.org")
#' req = req %>% req_body_json(list(x = 1, y = 2))
#' #req$method <- 'POST'
#' stub_request("post", "https://r-project.org") %>%
#'  to_return(status = 418, body = list(a = 5))
#' stub = webmockr_stub_registry$request_stubs[[1]]
#' stub$counter$.__enclos_env__$private$total <- 1
#' resp = x$.__enclos_env__$private$build_stub_response(stub)
#' resp = x$.__enclos_env__$private$build_response(req, resp)
#' resp = x$.__enclos_env__$private$add_response_sequences(stub, resp)
#' out
#' out$body
#' out$content
build_httr2_response <- function(req, resp) {
  lst <- list(
    method = req_method_get_w(req),
    url = tryCatch(resp$url, error = function(e) e) %|s|% req$url,
    status_code = as.integer(resp$status_code),
    headers = {
      if (grepl("^ftp://", resp$url %||% "")) { # in case uri_regex only
        list()
      } else {
        unclass(resp$headers)
      }
    },
    body = resp$content
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
#' @param x an unexecuted httr2 request object
#' @return a `httr2_request`
build_httr2_request = function(x) {
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

#' @noRd
mock_httr2 <- function(req) {
  Httr2Adapter$new()$handle_request(req)
}

#' Turn on httr2 mocking
#' @export
#' @param on (logical) `TRUE` to turn on, `FALSE` to turn off. default: `TRUE`
#' @return Silently returns `TRUE` when enabled and `FALSE` when disabled.
httr2_mock <- function(on = TRUE) {
  check_for_pkg("httr2")
  if (on) {
    httr2::local_mock(~ mock_httr2(req), env = globalenv())
    # options(httr2_mock = ~ webmockr_handle(req))
    # httr2::local_mock(~ webmockr_handle(req))
    # httr2::local_mock(~ webmockr::Httr2Adapter$new()$handle_request(req))
    # httr2::with_mock(webmockr_handle, httr2::req_perform(req))
    # rlang::as_function(webmockr_handle)
  } else {
    httr2::local_mock(NULL, env = globalenv())
    # options(httr2_mock = NULL)
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
