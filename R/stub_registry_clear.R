#' Clear the stub registry
#'
#' Clear all stubs
#'
#' @export
#' @return nothing
#' @family stub-registry
stub_registry_clear <- function() {
  webmockr_stub_registry$remove_all_request_stubs()
}
