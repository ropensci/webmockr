#' Stub an http request
#'
#' @export
#' @param method (character) HTTP method, one of "get", "post", "put", "patch",
#' "head", "delete", "options" - or the special "any" (for any method)
#' @param url (character) The request url. Can be a full url, partial, or a
#' regular expression to match many incantations of a url
#' @examples \dontrun{
#' # basic stubbing
#' stub_request("get", url="http://google.com/")
#'
#' # add header
#' stub_request("get", url="http://google.com/") %>%
#'    wi_th(headers = list('User-Agent' = 'R'))
#'
#' # add expectation with to_return
#' stub_request("get", url="http://google.com/") %>%
#'   wi_th(query = list(hello = "world"), headers = list('User-Agent' = 'R')) %>%
#'   to_return(status = 200, body = "", headers = list())
#' }
stub_request <- function(method = "get", url) {
  StubbedRequest$new(method = method, uri = url)
}
