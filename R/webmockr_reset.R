#' @title webmockr_reset
#' @description Clear all stubs and the request counter
#' @export
#' @return nothing
#' @seealso [stub_registry_clear()] [request_registry_clear()]
#' @details this function runs [stub_registry_clear()] and
#' [request_registry_clear()] - so you can run those two yourself
#' to achieve the same thing
#' @examples
#' # webmockr_reset()
webmockr_reset <- function() {
  stub_registry_clear()
  request_registry_clear()
  invisible(NULL)
}
