# Check if request is in cache
request_is_in_cache <- function(request_signature) {
  webmockr_stub_registry$is_registered(request_signature)
}
