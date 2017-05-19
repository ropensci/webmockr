#' Remove a request stub
#'
#' @export
#' @param stub a request stub, of class `StubbedRequest`
#' @return logical, `TRUE` if removed, `FALSE` if not removed
#' @family stub-registry
#' @examples
#' (x <- stub_request("get", url="https://httpbin.org/get"))
#' stub_registry()
#' remove_request_stub(x)
#' stub_registry()
remove_request_stub <- function(stub) {
  stopifnot(inherits(stub, "StubbedRequest"))
  webmockr_stub_registry$remove_request_stub(stub = stub)
}
