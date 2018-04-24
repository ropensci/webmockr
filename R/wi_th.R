#' Set additional parts of a stubbed request
#'
#' Set query params, request body, and/or request headers
#'
#' @export
#' @param .data input. Anything that can be coerced to a `StubbedRequest` class
#' object
#' @param ... Comma separated list of variable names, passed on
#' to [lazyeval::lazy_dots()]. accepts the following: query, body,
#' headers
#' @param .dots	Used to work around non-standard evaluation
#' @param .list named list of things, has to be one of 'query', 'body', 
#' and/or 'headers'. This is an escape hatch to avoid NSE (non-standard
#' evaluation), so don't pass the same thing to both, e.g. don't pass
#' 'query' via NSE, and also 'query' to this parameter
#' @details `with` is a function in the `base` package, so we went with
#' `wi_th`
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
#' @note see examples in [stub_request()]
#' @details Values for status, body, and headers:
#'
#' - query: (list) a named list
#' - body: various, including character string, list, raw, numeric, etc
#' - headers: (list) a named list
wi_th <- function(.data, ..., .list = list()) {
  wi_th_(.data, .dots = lazyeval::lazy_dots(...), .list = .list)
}

#' @export
#' @rdname wi_th
wi_th_ <- function(.data, ..., .dots, .list) {
  assert(.data, 'StubbedRequest')
  tmp <- lazyeval::all_dots(.dots, ...)
  if (length(tmp) == 0) {
    z <- NULL
  } else {
    z <- lapply(tmp, function(x) eval(x$expr))
  }

  z <- c(z, .list)
  if (any(duplicated(names(z)))) {
    stop("can not have duplicated names")
  }

  # lint user inputs
  assert(z$query, 'list')
  if (!all(hz_namez(z$query))) {
    stop("'query' must be a named list")
  }
  assert(z$body, 'list')
  if (!all(hz_namez(z$headers))) {
    stop("'headers' must be a named list")
  }

  .data$with(
    query = z$query,
    body = z$body,
    headers = z$headers
  )
  return(.data)
}
