#' Stub an http request
#'
#' @export
#' @param method (character) HTTP method, one of "get", "post", "put", "patch",
#' "head", "delete", "options" - or the special "any" (for any method)
#' @param uri (character) The request uri. Can be a full or partial uri.
#' \pkg{webmockr} can match uri's without the "http" scheme, but does
#' not match if the scheme is "https". required, unless `uri_regex` given.
#' See [UriPattern] for more. See the "uri vs. uri_regex" section
#' @param uri_regex (character) A URI represented as regex. required, if `uri`
#' not given. See examples and the "uri vs. uri_regex" section
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub.
#' @details Internally, this calls [StubbedRequest] which handles the logic
#'
#' See [stub_registry()] for listing stubs, [stub_registry_clear()]
#' for removing all stubs and [remove_request_stub()] for removing specific
#' stubs
#'
#' If multiple stubs match the same request, we use the first stub. So if you
#' want to use a stub that was created after an earlier one that matches,
#' remove the earlier one(s).
#'
#' Note on `wi_th()`: If you pass `query` values are coerced to character
#' class in the recorded stub. You can pass numeric, integer, etc., but
#' all will be coerced to character.
#'
#' See [wi_th()] for details on request body/query/headers and
#' [to_return()] for details on how response status/body/headers
#' are handled
#'
#' @note Trailing slashes are dropped from stub URIs before matching
#'
#' @section uri vs. uri_regex:
#' When you use `uri`, we compare the URIs without query params AND
#' also the query params themselves without the URIs.
#'
#' When you use `uri_regex` we don't compare URIs and query params;
#' we just use your regex string defined in `uri_regex` as the pattern
#' for a call to [grepl]
#'
#' @section Mocking writing to disk:
#' See [mocking-disk-writing]
#' @seealso [wi_th()], [to_return()], [to_timeout()], [to_raise()],
#' [mock_file()]
#' @examples \dontrun{
#' # basic stubbing
#' stub_request("get", "https://httpbin.org/get")
#' stub_request("post", "https://httpbin.org/post")
#'
#' # any method, use "any"
#' stub_request("any", "https://httpbin.org/get")
#'
#' # list stubs
#' stub_registry()
#'
#' # request headers
#' stub_request("get", "https://httpbin.org/get") %>%
#'   wi_th(headers = list("User-Agent" = "R"))
#'
#' # request body
#' stub_request("post", "https://httpbin.org/post") %>%
#'   wi_th(body = list(foo = "bar"))
#' stub_registry()
#' library(crul)
#' x <- crul::HttpClient$new(url = "https://httpbin.org")
#' crul::mock()
#' x$post("post", body = list(foo = "bar"))
#'
#' # add expectation with to_return
#' stub_request("get", "https://httpbin.org/get") %>%
#'   wi_th(
#'     query = list(hello = "world"),
#'     headers = list("User-Agent" = "R")
#'   ) %>%
#'   to_return(status = 200, body = "stuff", headers = list(a = 5))
#'
#' # list stubs again
#' stub_registry()
#'
#' # regex
#' stub_request("get", uri_regex = ".+ample\\..")
#'
#' # set stub an expectation to timeout
#' stub_request("get", "https://httpbin.org/get") %>% to_timeout()
#' x <- crul::HttpClient$new(url = "https://httpbin.org")
#' res <- x$get("get")
#'
#' # raise exception
#' library(fauxpas)
#' stub_request("get", "https://httpbin.org/get") %>% to_raise(HTTPAccepted)
#' stub_request("get", "https://httpbin.org/get") %>% to_raise(HTTPAccepted, HTTPGone)
#'
#' x <- crul::HttpClient$new(url = "https://httpbin.org")
#' stub_request("get", "https://httpbin.org/get") %>% to_raise(HTTPBadGateway)
#' crul::mock()
#' x$get("get")
#'
#' # pass a list to .list
#' z <- stub_request("get", "https://httpbin.org/get")
#' wi_th(z, .list = list(query = list(foo = "bar")))
#'
#' # just body
#' stub_request("any", uri_regex = ".+") %>%
#'   wi_th(body = list(foo = "bar"))
#' ## with crul
#' library(crul)
#' x <- crul::HttpClient$new(url = "https://httpbin.org")
#' crul::mock()
#' x$post("post", body = list(foo = "bar"))
#' x$put("put", body = list(foo = "bar"))
#' ## with httr
#' library(httr)
#' httr_mock()
#' POST("https://example.com", body = list(foo = "bar"))
#' PUT("https://google.com", body = list(foo = "bar"))
#'
#'
#' # just headers
#' headers <- list(
#'   "Accept-Encoding" = "gzip, deflate",
#'   "Accept" = "application/json, text/xml, application/xml, */*"
#' )
#' stub_request("any", uri_regex = ".+") %>% wi_th(headers = headers)
#' library(crul)
#' x <- crul::HttpClient$new(url = "https://httpbin.org", headers = headers)
#' crul::mock()
#' x$post("post")
#' x$put("put", body = list(foo = "bar"))
#' x$get("put", query = list(stuff = 3423234L))
#'
#' # many responses
#' ## the first response matches the first to_return call, and so on
#' stub_request("get", "https://httpbin.org/get") %>%
#'   to_return(status = 200, body = "foobar", headers = list(a = 5)) %>%
#'   to_return(status = 200, body = "bears", headers = list(b = 6))
#' con <- crul::HttpClient$new(url = "https://httpbin.org")
#' con$get("get")$parse("UTF-8")
#' con$get("get")$parse("UTF-8")
#'
#' ## OR, use times with to_return() to repeat the same response many times
#' library(fauxpas)
#' stub_request("get", "https://httpbin.org/get") %>%
#'   to_return(status = 200, body = "apple-pie", times = 2) %>%
#'   to_raise(HTTPUnauthorized)
#' con <- crul::HttpClient$new(url = "https://httpbin.org")
#' con$get("get")$parse("UTF-8")
#' con$get("get")$parse("UTF-8")
#' con$get("get")$parse("UTF-8")
#'
#' # partial matching
#' ## query parameters
#' library(httr)
#' enable()
#' ### matches
#' stub_request("get", "https://hb.opencpu.org/get") %>%
#'   wi_th(query = including(list(fruit = "pear"))) %>%
#'   to_return(body = "matched on partial query!")
#' resp <- GET("https://hb.opencpu.org/get",
#'   query = list(fruit = "pear", bread = "scone"))
#' rawToChar(content(resp))
#' ### doesn't match
#' stub_registry_clear()
#' stub_request("get", "https://hb.opencpu.org/get") %>%
#'   wi_th(query = list(fruit = "pear")) %>%
#'   to_return(body = "didn't match, ugh!")
#' # GET("https://hb.opencpu.org/get",
#' #  query = list(fruit = "pear", meat = "chicken"))
#'
#' ## request body
#' ### matches - including
#' stub_request("post", "https://hb.opencpu.org/post") %>%
#'   wi_th(body = including(list(fruit = "pear"))) %>%
#'   to_return(body = "matched on partial body!")
#' resp <- POST("https://hb.opencpu.org/post",
#'   body = list(fruit = "pear", meat = "chicken"))
#' rawToChar(content(resp))
#' ### matches - excluding
#' stub_request("post", "https://hb.opencpu.org/post") %>%
#'   wi_th(body = excluding(list(fruit = "pear"))) %>%
#'   to_return(body = "matched on partial body!")
#' res <- POST("https://hb.opencpu.org/post",
#'   body = list(color = "blue"))
#' rawToChar(content(res))
#' # POST("https://hb.opencpu.org/post",
#' #  body = list(fruit = "pear", meat = "chicken"))
#'
#' # clear all stubs
#' stub_registry()
#' stub_registry_clear()
#' }
stub_request <- function(method = "get", uri = NULL, uri_regex = NULL) {
  if (is.null(uri) && is.null(uri_regex)) {
    abort("one of uri or uri_regex is required")
  }
  tmp <- StubbedRequest$new(method = method, uri = uri, uri_regex = uri_regex)
  webmockr_stub_registry$register_stub(tmp)
  return(tmp)
}
