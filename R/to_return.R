#' Expectation for what's returned from a stubbed request
#'
#' Set response status code, response body, and/or response headers
#'
#' @export
#' @param .data input. Anything that can be coerced to a `StubbedRequest` class
#' object
#' @param ... Comma separated list of variable names, passed on
#' to [lazyeval::lazy_dots()]. accepts the following: status, body,
#' headers
#' @param .dots	Used to work around non-standard evaluation
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
to_return <- function(.data, ...) {
  to_return_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname to_return
to_return_ <- function(.data, ..., .dots) {
  tmp <- lazyeval::all_dots(.dots, ...)
  if (length(tmp) == 0) {
    z <- NULL
  } else {
    z <- lapply(tmp, function(x) eval(x$expr))
  }
  .data$to_return(
    status = z$status,
    body = z$body,
    headers = z$headers
  )
  return(.data)
}
