webmockr_lightswitch <- new.env()
#webmockr_lightswitch$httr <- FALSE
webmockr_lightswitch$crul <- FALSE

#' Enable or disable webmockr
#'
#' @export
#' @details `enable()` enables \pkg{webmockr} for all adapters. 
#' `disable()` disables \pkg{webmockr} for all adapters.  `enabled()` 
#' answers whether \pkg{webmockr} is enabled for a given adapter
#' @return `enable()` and `disable()` invisibly returns booleans for 
#' each adapter (currently only \pkg{crul}), as a result of running 
#' enable or disable, respectively, on each [HttpLibAdapaterRegistry]
#' object. `enabled` returns a single boolean
#' @param options list of options - ignored for now.
#' @param adapter (character) the adapter to enable, only 'crul' for now
enable <- function(options = list()) {
  invisible(vapply(http_lib_adapter_registry$adapters, function(z) {
    z$enable()
  }, logical(1)))
}

#' @export
#' @rdname enable
enabled <- function(adapter = "crul") {
  adapters <- c('crul')
  if (!adapter %in% adapters) {
    stop("'adapter' must be in the set ", 
      paste0(adapters, collapse = ", "))
  }
  webmockr_lightswitch$crul
}

#' @export
#' @rdname enable
disable <- function(options = list()) {
  invisible(unlist(lapply(http_lib_adapter_registry$adapters, function(z) {
    z$disable()
  })))
}
