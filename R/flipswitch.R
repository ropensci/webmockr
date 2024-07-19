webmockr_lightswitch <- new.env()
webmockr_lightswitch$httr <- FALSE
webmockr_lightswitch$httr2 <- FALSE
webmockr_lightswitch$crul <- FALSE
webmockr_adapters <- c('crul', 'httr', 'httr2')

#' Enable or disable webmockr
#'
#' @export
#' @param adapter (character) the adapter name, 'crul', 'httr', or 'httr2'.
#' one or the other. if none given, we attempt to enable both 
#' adapters
#' @param options list of options - ignored for now.
#' @param quiet (logical) suppress messages? default: `FALSE`
#' @details 
#' - `enable()` enables \pkg{webmockr} for all adapters
#' - `disable()` disables \pkg{webmockr} for all adapters
#' - `enabled()` answers whether \pkg{webmockr} is enabled for a given adapter
#' @return `enable()` and `disable()` invisibly returns booleans for 
#' each adapter, as a result of running enable or disable, respectively,
#' on each [HttpLibAdapaterRegistry] object. `enabled` returns a 
#' single boolean
enable <- function(adapter = NULL, options = list(), quiet = FALSE) {
  adnms <- vapply(http_lib_adapter_registry$adapters, function(w) w$client, "")
  if (!is.null(adapter)) {
    if (!adapter %in% webmockr_adapters) {
      stop("adapter must be one of 'crul', 'httr', or 'httr2'")
    }
    if (!requireNamespace(adapter, quietly = TRUE)) {
      message(adapter, " not installed, skipping enable")
      return(invisible(FALSE))
    }
    http_lib_adapter_registry$adapters[[which(adnms == adapter)]]$enable(quiet)
  } else {
    invisible(vapply(http_lib_adapter_registry$adapters, function(z) {
      pkgname <- z$client
      # check if package installed first
      if (!requireNamespace(pkgname, quietly = TRUE)) {
        message(pkgname, " not installed, skipping enable")
        FALSE
      } else {
        # if instaled, enable
        z$enable(quiet)
      }
    }, logical(1)))
  }
}

#' @export
#' @rdname enable
enabled <- function(adapter = "crul") {
  if (!adapter %in% webmockr_adapters) {
    stop("'adapter' must be in the set ", 
      paste0(webmockr_adapters, collapse = ", "))
  }
  webmockr_lightswitch[[adapter]]
}

#' @export
#' @rdname enable
disable <- function(adapter = NULL, options = list(), quiet = FALSE) {
  adnms <- vapply(http_lib_adapter_registry$adapters, function(w) w$client, "")
  if (!is.null(adapter)) {
    if (!adapter %in% webmockr_adapters) {
      stop("adapter must be one of 'crul', 'httr', or 'httr2'")
    }
    if (!requireNamespace(adapter, quietly = TRUE)) {
      message(adapter, " not installed, skipping disable")
      return(invisible(FALSE))
    }
    http_lib_adapter_registry$adapters[[which(adnms == adapter)]]$disable(quiet)
  } else {
    invisible(vapply(http_lib_adapter_registry$adapters, function(z) {
      pkgname <- z$client
      # check if package installed first
      if (!requireNamespace(pkgname, quietly = TRUE)) {
        message(pkgname, " not installed, skipping disable")
        FALSE
      } else {
        # if instaled, disable
        z$disable(quiet)
      }
    }, logical(1)))
  }
}
