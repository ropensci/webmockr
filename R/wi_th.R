#' Set additional parts of a stubbed request
#'
#' Set query params, request body, and/or request headers
#'
#' @export
#' @param .data input. Anything that can be coerced to a `StubbedRequest` class
#' object
#' @param ... Comma separated list of named variables. accepts the following: 
#' `query`, `body`, `headers`. See Details.
#' @param .list named list, has to be one of 'query', 'body',
#' and/or 'headers'. An alternative to passing in via `...`. Don't pass the 
#' same thing to both, e.g. don't pass 'query' to `...`, and also 'query' to 
#' this parameter
#' @details `with` is a function in the `base` package, so we went with
#' `wi_th`
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
#' @note see more examples in [stub_request()]
#' @details Values for query, body, and headers:
#'
#' - query: (list) a named list. values are coerced to character
#' class in the recorded stub. You can pass numeric, integer, etc., but
#' all will be coerced to character.
#' - body: various, including character string, list, raw, numeric,
#' upload (`crul::upload` or `httr::upload_file`, they both create the
#' same object in the end)
#' - headers: (list) a named list
#'
#' @examples
#' # first, make a stub object
#' req <- stub_request("post", "https://httpbin.org/post")
#'
#' # add body
#' # list
#' wi_th(req, body = list(foo = "bar"))
#' # string
#' wi_th(req, body = '{"foo": "bar"}')
#' # raw
#' wi_th(req, body = charToRaw('{"foo": "bar"}'))
#' # numeric
#' wi_th(req, body = 5)
#' # an upload
#' wi_th(req, body = crul::upload(system.file("CITATION")))
#' # wi_th(req, body = httr::upload_file(system.file("CITATION")))
#'
#' # add query - has to be a named list
#' wi_th(req, query = list(foo = "bar"))
#'
#' # add headers - has to be a named list
#' wi_th(req, headers = list(foo = "bar"))
#' wi_th(req, headers = list(`User-Agent` = "webmockr/v1", hello="world"))
#'
#' # .list - pass in a named list instead
#' wi_th(req, .list = list(body = list(foo = "bar")))
wi_th <- function(.data, ..., .list = list()) {
  assert(.data, "StubbedRequest")
  assert(.list, "list")
  z <- list(...)
  if (length(z) == 0) z <- NULL
  z <- c(z, .list)
  if (
    !any(c("query", "body", "headers") %in% names(z)) &&
    length(z) != 0
  ) {
    stop("'wi_th' only accepts query, body, headers")
  }
  if (any(duplicated(names(z)))) stop("can not have duplicated names")
  assert(z$query, "list")
  if (!all(hz_namez(z$query))) stop("'query' must be a named list")
  assert(z$headers, "list")
  if (!all(hz_namez(z$headers))) stop("'headers' must be a named list")
  .data$with(
    query = z$query,
    body = z$body,
    headers = z$headers
  )
  return(.data)
}
