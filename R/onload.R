http_lib_adapter_registry <- NULL # nocov start
.onLoad <- function(libname, pkgname) {
  # set defaults for webmockr
  webmockr_configure()

  # assign crul, httr, and curl adapters
  # which doesn't require those packages loaded yet
  x <- HttpLibAdapaterRegistry$new()
  x$register(CrulAdapter$new())
  x$register(HttrAdapter$new())
  x$register(CurlAdapter$new())
  http_lib_adapter_registry <<- x
} # nocov end
