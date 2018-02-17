#' Clear the stub registry
#'
#' Clear all stubs
#'
#' @export
#' @return nothing, well technically an empty list, but
#' it's not anything useful
#' @family stub-registry
#' @examples
#' (x <- stub_request("get", "https://httpbin.org/get"))
#' stub_registry()
#' stub_registry_clear()
#' stub_registry()
stub_registry_clear <- function() {
  webmockr_stub_registry$remove_all_request_stubs()
}
