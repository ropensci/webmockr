#' Expectation for what's returned from a stubbed request
#'
#' Set response status code, response body, and/or response headers
#'
#' @export
#' @param .data input. Anything that can be coerced to a `StubbedRequest` class
#' object
#' @param ... Comma separated list of named variables. accepts the following:
#' `status`, `body`, `headers`. See Details for more.
#' @param .list named list, has to be one of 'status', 'body',
#' and/or 'headers'. An alternative to passing in via `...`. Don't pass the 
#' same thing to both, e.g. don't pass 'status' to `...`, and also 'status' to
#' this parameter
#' @param times (integer) number of times the given response should be
#' returned; default: 1. value must be greater than or equal to 1. Very large
#' values probably don't make sense, but there's no maximum value. See 
#' Details.
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
#' @note see more examples in [stub_request()]
#' @details Values for status, body, and headers:
#'
#' - status: (numeric/integer) three digit status code
#' - body: various: `character`, `json`, `list`, `raw`, `numeric`,
#'  `NULL`, `FALSE`, a file connection (other connetion types
#'  not supported), or a `mock_file` function call (see [mock_file()])
#' - headers: (list) a named list, must be named
#'
#' response headers are returned with all lowercase names and the values
#' are all of type character. if numeric/integer values are given
#' (e.g., `to_return(headers = list(a = 10))`), we'll coerce any
#' numeric/integer values to character.
#' 
#' @section multiple `to_return()`:
#' You can add more than one `to_return()` to a webmockr stub (including
#' [to_raise()], [to_timeout()]). Each one is a HTTP response returned. 
#' That is, you'll match to an HTTP request based on `stub_request()` and
#' `wi_th()`; the first time the request is made, the first response
#' is returned; the second time the reqeust is made, the second response
#' is returned; and so on.
#' 
#' Be aware that webmockr has to track number of requests
#' (see [request_registry()]), and so if you use multiple `to_return()`
#' or the `times` parameter, you must clear the request registry
#' in order to go back to mocking responses from the start again.
#' [webmockr_reset()] clears the stub registry and  the request registry,
#' after which you can use multiple responses again (after creating
#' your stub(s) again of course)
#' 
#' @inheritSection to_raise Raise vs. Return
#' 
#' @examples
#' # first, make a stub object
#' foo <- function() {
#'   stub_request("post", "https://httpbin.org/post")
#' }
#'
#' # add status, body and/or headers
#' foo() %>% to_return(status = 200)
#' foo() %>% to_return(body = "stuff")
#' foo() %>% to_return(body = list(a = list(b = "world")))
#' foo() %>% to_return(headers = list(a = 5))
#' foo() %>% 
#'   to_return(status = 200, body = "stuff", headers = list(a = 5))
#' 
#' # .list - pass in a named list instead
#' foo() %>% to_return(.list = list(body = list(foo = "bar")))
#' 
#' # multiple responses using chained `to_return()`
#' foo() %>% to_return(body = "stuff") %>% to_return(body = "things")
#' 
#' # many of the same response using the times parameter
#' foo() %>% to_return(body = "stuff", times = 3)
to_return <- function(.data, ..., .list = list(), times = 1) {
  assert(.data, "StubbedRequest")
  assert(.list, "list")
  assert(times, c("integer", "numeric"))
  assert_gte(times, 1)
  z <- list(...)
  if (length(z) == 0) z <- NULL
  z <- c(z, .list)
  if (
    !any(c("status", "body", "headers") %in% names(z)) &&
    length(z) != 0
  ) {
    stop("'to_return' only accepts status, body, headers")
  }
  assert(z$status, c("numeric", "integer"))
  assert(z$headers, "list")
  if (!all(hz_namez(z$headers))) stop("'headers' must be a named list")
  replicate(times,
    .data$to_return(status = z$status, body = z$body, headers = z$headers))
  return(.data)
}
