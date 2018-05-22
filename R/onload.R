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

  # initialize empty stub registry on package load
  # webmockr_stub_registry <<- new.env()
  # webmockr_stub_registry <- webmockr::StubRegistry$new()
}

# .onAttach <- function(libname, pkgname) {
#   #base::unlockBinding("request_perform", as.environment("package:httr"))
#   utils::assignInNamespace("request_perform", request_perform, "httr")
#   #base::lockBinding("request_perform", as.environment("package:httr"))
# }

# .onAttach <- function(libname, pkgname) {
#   when_attached("httr", {
#     utils::assignInNamespace("request_perform", request_perform, "httr")
#   })
# }
#
# when_attached <- function(pkg, action) {
#   if (is_attached(pkg)) {
#     action
#   } else {
#     setHook(packageEvent(pkg, "attach"), function(...) action)
#   }
# }
#
# is_attached <- function(pkg) paste0("package:", pkg) %in% search()
