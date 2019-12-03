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
#' @examples
#' # first, make a stub object
#' (req <- stub_request("post", "https://httpbin.org/post"))
#'
#' # add status, body and/or headers
#' to_return(req, status = 200)
#' to_return(req, body = "stuff")
#' to_return(req, body = list(a = list(b = "world")))
#' to_return(req, headers = list(a = 5))
#' to_return(req, status = 200, body = "stuff", headers = list(a = 5))
#' 
#' # .list - pass in a named list instead
#' to_return(req, .list = list(body = list(foo = "bar")))
to_return <- function(.data, ..., .list = list()) {
  assert(.data, "StubbedRequest")
  assert(.list, "list")
  z <- list(...)
  if (length(z) == 0) z <- NULL
  z <- c(z, .list)
  if (
    !any(c("status", "body", "headers") %in% names(z)) &&
    length(z) != 0
  ) {
    stop("'to_return' only accepts status, body, headers")
  }
  assert(z$status, "numeric")
  assert(z$headers, "list")
  if (!all(hz_namez(z$headers))) stop("'headers' must be a named list")
  .data$to_return(
    status = z$status,
    body = z$body,
    headers = z$headers
  )
  return(.data)
}
