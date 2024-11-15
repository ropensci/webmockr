check_redirect_setting <- function() {
  cs <- vcr::current_cassette()
  stopifnot(
    "record_separate_redirects must be logical" =
      is.logical(cs$record_separate_redirects)
  )
  return(cs)
}

handle_separate_redirects <- function(req) {
  cs <- check_redirect_setting()
  if (cs$record_separate_redirects) {
    req$options$followlocation <- 0L
    if (is.list(req$url)) {
      curl::handle_setopt(req$url$handle, followlocation = 0L)
    }
  }
  return(req)
}

redirects_request <- function(x) {
  cs <- check_redirect_setting()
  if (cs$record_separate_redirects) {
    return(cs$request_handler$request_original)
  }
  x
}

redirects_response <- function(x) {
  cs <- check_redirect_setting()
  if (cs$record_separate_redirects) {
    return(last(cs$redirect_pool)[[1]])
  }
  x
}
