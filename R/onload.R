http_lib_adapter_registry <- NULL # nocov start
.onLoad <- function(libname, pkgname) {
  # set defaults for webmockr
  webmockr_configure()

  # assign crul and httr adapters
  # which doesn't require those packages loaded yet
  x <- HttpLibAdapaterRegistry$new()
  x$register(CrulAdapter$new())
  x$register(HttrAdapter$new())
  http_lib_adapter_registry <<- x
} # nocov end
