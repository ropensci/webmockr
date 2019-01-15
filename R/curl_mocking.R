curl_mock_env <- new.env()

#' Mocking HTTP requests
#' @noRd
#' @param on (logical) turn mocking on with `TRUE` or turn off with 
#' `FALSE` (default)
curl_mock <- function(on = TRUE) curl_mock_env$mock <- on

#' mock a curl request
#' @export
#' @param url (character) a url, see [curl::curl_fetch]
#' @param h a curl handle, see [curl::new_handle()]
#' @param path a path. default: `NULL`. see [curl::curl_fetch]
#' @param called the expression called, used in webmockr
#' internals
#' @examples
#' library(curl)
#' h <- new_handle()
#' handle_setheaders(h, a = 'b')
#' handle_setheaders(h, foo = 'bar')
#' # curl_mock_req("https://httpbin.org/get", h, "")
#' stub_request("get", "https://httpbin.org/get")
#' curl_mock_req("https://httpbin.org/get", h, "")
curl_mock_req <- function(url, h, path = NULL, called = "") {
  res <- curl::curl_echo(h, progress = FALSE, file = path)
  req <- list(url = url, handle = h, called = called)
  req$method <- res$method
  if (!is.null(res$http_authorization)) {
    req$auth <- 
      as.list(stats::setNames(
        strsplit(res$http_authorization, "\\s")[[1]], 
        c('type', 'user_pwd')
      ))
  }
  req$headers <- as.list(res$headers)
  if (!is.null(res$body)) req$body <- rawToChar(res$body)
  adap <- webmockr::CurlAdapter$new()
  adap$handle_request(req)
}
