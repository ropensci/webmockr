#' Get a diff of a stub request body and a request body from an http request
#' 
#' @export
#' @param stub object of class `StubbedRequest`. required. default is to 
#' call [last_stub()], which gets the last stub created
#' @param request object of class `RequestSignature`. required. default is to
#' call [last_request()], which gets the last stub created
#' @return object of class `Diff` from the \pkg{diffobj} package
#' @details If either `stub` or `request` are `NULL`, this function will
#' return an error message. You may not intentionally pass in a `NULL`, but
#' the return value of [last_stub()] and [last_request()] when there's 
#' nothing found is `NULL`.
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
#' 	wi_th(body = list(apple = "green"))
#' enable()
#' library(crul)
#' HttpClient$new("https://hb.opencpu.org")$post(
#' 	path = "post", body = list(apple = "red"))
#' stub_body_diff()
#' 
#' # Gives diff when there's a stub and request found - with request body
#' stub_request("post", "https://hb.opencpu.org/post") %>% 
#' 	wi_th(body = "the quick brown fox")
#' HttpClient$new("https://hb.opencpu.org")$post(
#' 	path = "post", body = "the quick black fox")
#' stub_body_diff()
stub_body_diff <- function(stub = last_stub(), request = last_request()) {
	check_installed("diffobj")
	if (is_empty(stub) || is_empty(request)) {
		abort(c("`stub` and/or `request` are NULL or otherwise empty",
			"see `?stub_body_diff`"))
	}
	assert_is(stub, "StubbedRequest")
	assert_is(request, "RequestSignature")
  diffobj::diffObj(stub$body, request$body)
}
