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
#' Note on `wi_th()`: If you pass `query`, values are coerced to character
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
#' @section Error handling:
#' To construct stubs, one uses [stub_request()] first - which registers
#' the stub in the stub registry. Any additional calls to modify the stub
#' with for example [wi_th()] or [to_return()] can error. In those error
#' cases we ideally want to remove (unregister) the stub because you
#' certainly don't want a registered stub that is not exactly what you
#' intended.
#'
#' When you encounter an error creating a stub you should see a warning
#' message that the stub has been removed, for example:
#'
#' ```
#' stub_request("get", "https://httpbin.org/get") %>%
#'   wi_th(query = mtcars)
#' #> Error in `wi_th()`:
#' #> ! z$query must be of class list or partial
#' #> Run `rlang::last_trace()` to see where the error occurred.
#' #> Warning message:
#' #> Encountered an error constructing stub
#' #> • Removed stub
#' #> • To see a list of stubs run stub_registry()
#' ```
#'
#'
#' @seealso [wi_th()], [to_return()], [to_timeout()], [to_raise()],
#' [mock_file()]
#' @examples
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
#' # clear all stubs
#' stub_registry()
#' stub_registry_clear()
stub_request <- function(method = "get", uri = NULL, uri_regex = NULL) {
  if (is_null(uri) && is_null(uri_regex)) {
    abort("one of uri or uri_regex is required")
  }
  tmp <- StubbedRequest$new(method = method, uri = uri, uri_regex = uri_regex)
  webmockr_stub_registry$register_stub(tmp)
  return(tmp)
}
