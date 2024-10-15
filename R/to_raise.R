#' Set raise error condition
#'
#' @export
#' @param .data input. Anything that can be coerced to a `StubbedRequest`
#' class object
#' @param ... One or more HTTP exceptions from the \pkg{fauxpas} package. Run
#' `grep("HTTP*", getNamespaceExports("fauxpas"), value = TRUE)` for a list of
#' possible exceptions
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
#' @section Raise vs. Return:
#' `to_raise()` always raises a stop condition, while `to_return(status=xyz)` only
#' sets the status code on the returned HTTP response object. So if you want to
#' raise a stop condition then `to_raise()` is what you want. But if you
#' don't want to raise a stop condition use `to_return()`. Use cases for each
#' vary. For example, in a unit test you may have a test expecting a 503 error;
#' in this case `to_raise()` makes sense. In another case, if a unit test
#' expects to test some aspect of an HTTP response object that httr, httr2,
#' or crul typically returns, then you'll want `to_return()`.
#'
#' @details The behavior in the future will be:
#'
#' When multiple exceptions are passed, the first is used on the first
#' mock, the second on the second mock, and so on. Subsequent mocks use the
#' last exception
#'
#' But for now, only the first exception is used until we get that fixed
#' @note see examples in [stub_request()]
to_raise <- function(.data, ...) {
  assert(.data, "StubbedRequest")
  tmp <- list(...)
  if (!all(vapply(
    tmp, function(x) inherits(x, "R6ClassGenerator"),
    logical(1)
  ))) {
    abort("all objects must be error classes from fauxpas")
  }
  if (!all(vapply(tmp, function(x) grepl("HTTP", x$classname), logical(1)))) {
    abort("all objects must be error classes from fauxpas")
  }
  .data$to_raise(tmp)
  return(.data)
}
