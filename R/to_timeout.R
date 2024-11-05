#' Set timeout as an expected return on a match
#'
#' @export
#' @param .data input. Anything that can be coerced to a `StubbedRequest` class
#' object
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
#' @note see examples in [stub_request()]
to_timeout <- function(.data) {
  handle_stub_removal(.data, {
    assert_is(.data, "StubbedRequest")
    assert_stub_registered(.data)
    .data$to_timeout()
  })
  return(.data)
}
