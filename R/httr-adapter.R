#' httr library adapter
#'
#' @keywords internal
#' @family http_lib_adapters
#' @param x input
#' @details This adapter modifies \pkg{httr} to allow mocking HTTP requests
#' when one is using \pkg{httr} in their code
HttrAdapter <- R6::R6Class(
  'HttrAdapter',
  public = list(
    enable = function() {
      message("enabled!")
    },

    disable = function() {
      message("disabled!")
    }
  )
)

# httr methods to override

## request_perform -> changes:
## - look in cache for matching request (given user specified matchers)
## - if it's a match, return the response (body, headers, etc.)
## - if no match, proceed with http request as normal
request_perform <- function(req, handle, refresh = TRUE) {
  stopifnot(httr:::is.request(req), inherits(handle, "curl_handle"))
  req <- httr:::request_prepare(req)

  curl::handle_setopt(handle, .list = req$options)
  if (!is.null(req$fields))
    curl::handle_setform(handle, .list = req$fields)
  curl::handle_setheaders(handle, .list = req$headers)
  on.exit(curl::handle_reset(handle), add = TRUE)

  if (request_is_in_cache(request_signature)) {
    StubRegistry$find_stubbed_request(req)
  } else {
    resp <- httr:::request_fetch(req$output, req$url, handle)

    # If return 401 and have auth token, refresh it and then try again
    needs_refresh <- refresh && resp$status_code == 401L &&
      !is.null(req$auth_token) && req$auth_token$can_refresh()
    if (needs_refresh) {
      message("Auto-refreshing stale OAuth token.")
      req$auth_token$refresh()
      return(httr:::request_perform(req, handle, refresh = FALSE))
    }

    all_headers <- httr:::parse_headers(resp$headers)
    headers <- httr:::last(all_headers)$headers
    if (!is.null(headers$date)) {
      date <- httr:::parse_http_date(headers$Date)
    } else {
      date <- Sys.time()
    }

    httr:::response(
      url = resp$url,
      status_code = resp$status_code,
      headers = headers,
      all_headers = all_headers,
      cookies = curl::handle_cookies(handle),
      content = resp$content,
      date = date,
      times = resp$times,
      request = req,
      handle = handle
    )
  }
}

request_is_in_cache <- function(request_signature) {
  StubRegistry$is_registered(request_signature)
}
