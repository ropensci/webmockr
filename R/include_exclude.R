#' Partially match request query parameters or request bodies
#'
#' For use inside [wi_th()]
#'
#' @export
#' @param x (list) a list; may support other classes in the future
#' @return same as `x`, but with two attributes added:
#' - partial_match: always `TRUE`
#' - partial_type: the type of match, one of `include` or `exclude`
#' @aliases partial
#' @section Headers:
#' Matching on headers already handles partial matching. That is,
#' `wi_th(headers = list(Fruit = "pear"))` matches any request
#' that has any request header that matches - the request can have
#' other request headers, but those don't matter as long as there is
#' a match. These helpers (`including`/`excluding`) are needed
#' for query parameters and bodies because by default matching must be
#' exact for those.
#' @examples
#' including(list(foo = "bar"))
#' excluding(list(foo = "bar"))
#'
#' # get just keys by setting values as NULL
#' including(list(foo = NULL, bar = NULL))
#'
#' # in a stub
#' req <- stub_request("get", "https://httpbin.org/get")
#' req
#'
#' ## query
#' wi_th(req, query = list(foo = "bar"))
#' wi_th(req, query = including(list(foo = "bar")))
#' wi_th(req, query = excluding(list(foo = "bar")))
#'
#' ## body
#' wi_th(req, body = list(foo = "bar"))
#' wi_th(req, body = including(list(foo = "bar")))
#' wi_th(req, body = excluding(list(foo = "bar")))
#'
#' # cleanup
#' stub_registry_clear()
including <- function(x) {
  assert(x, "list")
  class(x) <- "partial"
  attr(x, "partial_match") <- TRUE
  attr(x, "partial_type") <- "include"
  return(x)
}

#' @export
#' @rdname including
excluding <- function(x) {
  assert(x, "list")
  class(x) <- "partial"
  attr(x, "partial_match") <- TRUE
  attr(x, "partial_type") <- "exclude"
  return(x)
}

#' @export
print.partial <- function(x, ...) {
  cat("<partial match>", sep = "\n")
  cat(paste0("  partial type: ", attr(x, "partial_type")), sep = "\n")
  cat(paste0("  length: ", length(x)), sep = "\n")
}
