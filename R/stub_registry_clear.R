#' @title stub_registry_clear
#' @description Clear all stubs in the stub registry
#' @export
#' @return an empty list invisibly
#' @family stub-registry
#' @examples
#' (x <- stub_request("get", "https://httpbin.org/get"))
#' stub_registry()
#' stub_registry_clear()
#' stub_registry()
stub_registry_clear <- function() {
  invisible(webmockr_stub_registry$remove_all_request_stubs())
}
