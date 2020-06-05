#' Partially match query parameters or request bodies
#' 
#' @export
#' @param x various objects
#' @return same as `x`, but with attributes added
#' @examples
#' including(list(foo = "bar"))
#' # just keys by setting values as NULL
#' including(list(foo = NULL, bar = NULL))
#' 
#' # in a stub
#' req <- stub_request("get", "https://httpbin.org/get")
#' ## body
#' wi_th(req, body = list(foo = "bar"))
#' wi_th(req, body = including(list(foo = "bar")))
#' wi_th(req, body = excluding(list(foo = "bar")))
#' 
#' ## query
#' wi_th(req, query = list(foo = "bar"))
#' wi_th(req, query = including(list(foo = "bar")))
#' wi_th(req, query = excluding(list(foo = "bar")))
#' 
#' # cleanup
#' stub_registry_clear()
including <- function(x) {
  attr(x, "partial_match") <- TRUE
  attr(x, "partial_type") <- "include"
  return(x)
}

#' @export
#' @rdname including
excluding <- function(x) {
  attr(x, "partial_match") <- TRUE
  attr(x, "partial_type") <- "exclude"
  return(x)
}
