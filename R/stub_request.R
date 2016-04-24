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
#' stub_request("get", url="http://httpbin.org/get")
#' stub_request("post", url="http://httpbin.org/post")
#'
#' # add header
#' stub_request("get", url="http://httpbin.org/get") %>%
#'    wi_th(headers = list('User-Agent' = 'R'))
#'
#' # add expectation with to_return
#' stub_request("get", url="http://httpbin.org/get") %>%
#'   wi_th(query = list(hello = "world"), headers = list('User-Agent' = 'R')) %>%
#'   to_return(status = 200, body = "", headers = list())
#'
#' # RFC 6570 templates
#' stub_request("get", "www.example.com/{id}/")
#' stub_request("get", "/.*example.*/")
#' stub_request("get", "www.example.com/thing/{id}.json{?x,y,z}{&other*}")
#' httr::GET('www.example.com/thing/5.json?x=1&y=2&z=3&anyParam=4')
#' }
stub_request <- function(method = "get", url) {
  StubbedRequest$new(method = method, uri = url)
}
