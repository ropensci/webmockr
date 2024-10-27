#' Get the last HTTP request made
#' 
#' @export
#' @return `NULL` if no requests registered; otherwise the last
#' registered request made as a `RequestSignature` class
#' @examplesIf interactive()
#' # no requests
#' request_registry_clear()
#' last_request()
#' 
#' # a request is found
#' enable()
#' stub_request("head", "https://nytimes.com")
#' library(crul)
#' crul::ok("https://nytimes.com")
#' last_request()
#' 
#' # cleanup
#' request_registry_clear()
#' stub_registry_clear()
last_request <- function() {
	last(webmockr_request_registry$request_signatures$hash)$sig
}

#' Get the last stub created
#' 
#' @export
#' @return `NULL` if no stubs found; otherwise the last stub created 
#' as a `StubbedRequest` class
#' @examplesIf interactive()
#' # no requests
#' stub_registry_clear()
#' last_stub()
#' 
#' # a stub is found
#' stub_request("head", "https://nytimes.com")
#' last_stub()
#' 
#' stub_request("post", "https://nytimes.com/stories")
#' last_stub()
#' 
#' # cleanup
#' stub_registry_clear()
last_stub <- function() {
	tmp <- last(webmockr_stub_registry$request_stubs)
	if (rlang::is_empty(tmp)) {
		return(NULL)
	}
	tmp
}
