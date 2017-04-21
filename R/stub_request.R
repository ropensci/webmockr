#' Stub an http request
#'
#' @export
#' @param method (character) HTTP method, one of "get", "post", "put", "patch",
#' "head", "delete", "options" - or the special "any" (for any method)
#' @param url (character) The request url. Can be a full url, partial, or a
#' regular expression to match many incantations of a url
#' @template stub_egs
#' @examples \dontrun{
#' # basic stubbing
#' stub_request("get", url="https://httpbin.org/get")
#' stub_request("post", url="https://httpbin.org/post")
#'
#' # list stubs
#' webmockr_stub_registry
#'
#' # add header
#' stub_request("get", url="https://httpbin.org/get") %>%
#'    wi_th(response_headers = list('User-Agent' = 'R'))
#'
#' # add expectation with to_return
#' stub_request("get", url="https://httpbin.org/get") %>%
#'   wi_th(
#'     query = list(hello = "world"),
#'     request_headers = list('User-Agent' = 'R')) %>%
#'   to_return(status = 200, body = "", response_headers = list())
#'
#' # list stubs again
#' webmockr_stub_registry
#'
#' # RFC 6570 templates
#' stub_request("get", "www.example.com/{id}/")
#' stub_request("get", "/.*example.*/")
#' stub_request("get", "www.example.com/thing/{id}.json{?x,y,z}{&other*}")
#' # httr::GET('www.example.com/thing/5.json?x=1&y=2&z=3&anyParam=4')
#' }
stub_request <- function(method = "get", url) {
  tmp <- StubbedRequest$new(method = method, uri = url)
  #tmp$with(query = query, body = body, request_headers = request_headers)
  webmockr_stub_registry$register_stub(tmp)
  return(tmp)
}

# query = NULL,
# body = NULL, request_headers = NULL
