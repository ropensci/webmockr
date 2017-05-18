#' Stub an http request
#'
#' @export
#' @param method (character) HTTP method, one of "get", "post", "put", "patch",
#' "head", "delete", "options" - or the special "any" (for any method)
#' @param url (character) The request url. Can be a full url, partial, or a
#' regular expression to match many incantations of a url. required.
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
#' @examples \dontrun{
#' # basic stubbing
#' stub_request("get", url="https://httpbin.org/get")
#' stub_request("post", url="https://httpbin.org/post")
#'
#' # list stubs
#' stub_registry()
#'
#' # add header
#' stub_request("get", url="https://httpbin.org/get") %>%
#'    wi_th(headers = list('User-Agent' = 'R'))
#'
#' # add expectation with to_return
#' stub_request("get", url="https://httpbin.org/get") %>%
#'   wi_th(
#'     query = list(hello = "world"),
#'     headers = list('User-Agent' = 'R')) %>%
#'   to_return(status = 200, body = "stuff", headers = list(a = 5))
#'
#' # list stubs again
#' stub_registry()
#'
#' # RFC 6570 templates
#' stub_request("get", "www.example.com/{id}/")
#' stub_request("get", "/.*example.*/")
#' stub_request("get", "www.example.com/thing/{id}.json{?x,y,z}{&other*}")
#'
#' # clear all stubs
#' stub_registry_clear()
#' }
stub_request <- function(method = "get", url) {
  if (missing(url)) stop("url is a required parameter", call. = FALSE)
  tmp <- StubbedRequest$new(method = method, uri = url)
  webmockr_stub_registry$register_stub(tmp)
  return(tmp)
}
