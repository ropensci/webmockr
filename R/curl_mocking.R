curl_mock_env <- new.env()

#' Mocking HTTP requests
#' @noRd
#' @param on (logical) turn mocking on with `TRUE` or turn off with 
#' `FALSE` (default)
curl_mock <- function(on = TRUE) curl_mock_env$mock <- on

#' mock a curl request
#' @export
#' @param url (character) a url
#' @param h a curl handle
#' @param called the expression called. maybe remove this.
#' @examples
#' library(curl)
#' h <- new_handle()
#' handle_setheaders(h, a = 'b')
#' handle_setheaders(h, foo = 'bar')
#' # curl_mock_req("https://httpbin.org/get", h, "")
#' stub_request("get", "https://httpbin.org/get")
#' curl_mock_req("https://httpbin.org/get", h, "")
curl_mock_req <- function(url, h, called = "") {
  res <- curl::curl_echo(h, progress = FALSE)
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
