#' Expection for a stubbed request
#'
#' @export
#' @param .data input. Anything that can be coerced to a StubbedRequest class
#' object
#' @param ... Comma separated list of unquoted variable names, passed on
#' to [lazyeval::lazy_dots()]
#' @param .dots	Used to work around non-standard evaluation
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
    response_headers = z$response_headers
  )
  return(.data)
}
