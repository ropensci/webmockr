webmockr_lightswitch <- new.env()
#webmockr_lightswitch$httr <- FALSE
webmockr_lightswitch$crul <- FALSE

#' Enable or disable webmockr
#'
#'
#' @export
#' @param options list of options
enable <- function(options = list()) {
  lapply(http_lib_adapter_registry$adapters, function(z) {
    z$enable
  })
}

#' @export
#' @rdname enable
disable <- function(options = list()) {
  lapply(http_lib_adapter_registry$adapters, function(z) {
    z$disable
  })
}

# def self.disable!(options = {})
# except = [options[:except]].flatten.compact
# HttpLibAdapterRegistry.instance.each_adapter do |name, adapter|
#   adapter.enable!
#   adapter.disable! unless except.include?(name)
# end
# end
#
# def self.enable!(options = {})
# except = [options[:except]].flatten.compact
# HttpLibAdapterRegistry.instance.each_adapter do |name, adapter|
#   adapter.disable!
#   adapter.enable! unless except.include?(name)
# end
# end
