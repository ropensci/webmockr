#' Get a diff of a stub request body and a request body from an http request
#'
#' Requires the Suggested package `diffobj`
#'
#' @export
#' @param stub object of class `StubbedRequest`. required. default is to
#' call [last_stub()], which gets the last stub created
#' @param request object of class `RequestSignature`. required. default is to
#' call [last_request()], which gets the last stub created
#' @return object of class `Diff` from the \pkg{diffobj} package
#' @details Returns error message if either `stub` or `request` are `NULL`.
#' Even though you may not intentionally pass in a `NULL`, the return values
#' of [last_stub()] and [last_request()] when there's nothing found is `NULL`.
#'
#' Under the hood the Suggested package `diffobj` is used to do the comparison.
#' @seealso [webmockr_configure()] to toggle `webmockr` showing request body
#' diffs when there's not a match. `stub_body_diff()` is offered as a manual
#' way to compare requests and stubs - whereas turning on with
#' [webmockr_configure()] will do the diff for you.
#' @examplesIf interactive()
#' # stops with error if no stub and request
#' request_registry_clear()
#' stub_registry_clear()
#' stub_body_diff()
#'
#' # Gives diff when there's a stub and request found - however, no request body
#' stub_request("get", "https://hb.opencpu.org/get")
#' enable()
#' library(crul)
#' HttpClient$new("https://hb.opencpu.org")$get(path = "get")
#' stub_body_diff()
#'
#' # Gives diff when there's a stub and request found - with request body
#' stub_request("post", "https://hb.opencpu.org/post") %>%
#'   wi_th(body = list(apple = "green"))
#' enable()
#' library(crul)
#' HttpClient$new("https://hb.opencpu.org")$post(
#'   path = "post", body = list(apple = "red")
#' )
#' stub_body_diff()
#'
#' # Gives diff when there's a stub and request found - with request body
#' stub_request("post", "https://hb.opencpu.org/post") %>%
#'   wi_th(body = "the quick brown fox")
#' HttpClient$new("https://hb.opencpu.org")$post(
#'   path = "post", body = "the quick black fox"
#' )
#' stub_body_diff()
stub_body_diff <- function(stub = last_stub(), request = last_request()) {
  check_installed("diffobj")
  if (is_empty(stub) || is_empty(request)) {
    abort(c(
      "`stub` and/or `request` are NULL or otherwise empty",
      "see `?stub_body_diff`"
    ))
  }
  assert_is(stub, "StubbedRequest")
  assert_is(request, "RequestSignature")
  diffobj::diffObj(stub$body, request$body)
}
