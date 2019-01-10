#' Expectation for what's returned from a stubbed request
#'
#' Set response status code, response body, and/or response headers
#'
#' @export
#' @param .data input. Anything that can be coerced to a `StubbedRequest` class
#' object
#' @param ... Comma separated list of variable names, passed on
#' to [lazyeval::lazy_dots()]. accepts the following: status, body,
#' headers. See Details for more.
#' @param .dots	Used to work around non-standard evaluation
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
#' @note see examples in [stub_request()]
#' @details Values for status, body, and headers:
#'
#' - status: (numeric/integer) three digit status code
#' - body: various, including character string, list, raw, numeric, etc
#' - headers: (list) a named list
#' 
#' response headers are returned with all lowercase names and the values
#' are all of type character. if numeric/integer values are given 
#' (e.g., `to_return(headers = list(a = 10))`), we'll coerce any 
#' numeric/integer values to character.
to_return <- function(.data, ...) {
  to_return_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname to_return
to_return_ <- function(.data, ..., .dots) {
  assert(.data, 'StubbedRequest')
  tmp <- lazyeval::all_dots(.dots, ...)
  if (length(tmp) == 0) {
    z <- NULL
  } else {
    z <- lapply(tmp, function(x) eval(x$expr))
  }

  # lint user inputs
  assert(z$status, 'numeric')
  assert(z$headers, 'list')
  if (!all(hz_namez(z$headers))) {
    stop("'headers' must be a named list")
  }

  .data$to_return(
    status = z$status,
    body = z$body,
    headers = z$headers
  )
  return(.data)
}
