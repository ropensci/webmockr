.onAttach <- function(libname, pkgname) {
  webmockr::webmockr_configure()
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
