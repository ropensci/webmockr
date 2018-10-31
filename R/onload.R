http_lib_adapter_registry <- NULL
.onLoad <- function(libname, pkgname) {
  # set defaults for webmockr
  webmockr_configure()

  # assign crul adapter for now by default
  ##  because it's the only http lib supported for now
  ##  later change to making user set the adapter themselves
  x <- HttpLibAdapaterRegistry$new()
  x$register(CrulAdapter$new())
  x$register(HttrAdapter$new())
  http_lib_adapter_registry <<- x
}
