http_lib_adapter_registry <- NULL # nocov start
webmockr_stub_registry <- NULL
webmockr_request_registry <- NULL

.onLoad <- function(libname, pkgname) {
  # set defaults for webmockr
  webmockr_configure()

  # assign crul and httr adapters
  # which doesn't require those packages loaded yet
  x <- HttpLibAdapaterRegistry$new()
  x$register(CrulAdapter$new())
  x$register(HttrAdapter$new())
  http_lib_adapter_registry <<- x

  # initialize empty stub registry on package load
  webmockr_stub_registry <<- StubRegistry$new()

  # initialize empty request registry on package load
  webmockr_request_registry <<- RequestRegistry$new()
} # nocov end
