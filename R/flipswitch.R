webmockr_lightswitch <- new.env()
#webmockr_lightswitch$httr <- FALSE
webmockr_lightswitch$crul <- FALSE

#' Enable or disable webmockr
#'
#' @export
#' @param options list of options - ignored for now.
enable <- function(options = list()) {
  invisible(vapply(http_lib_adapter_registry$adapters, function(z) {
    z$enable()
  }, logical(1)))
}

#' @export
#' @rdname enable
disable <- function(options = list()) {
  invisible(unlist(lapply(http_lib_adapter_registry$adapters, function(z) {
    z$disable()
  })))
}
