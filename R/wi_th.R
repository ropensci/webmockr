#' Set additional parts of a stubbed request
#'
#' Set query params, request body, request headers and/or basic_auth
#'
#' @export
#' @param .data input. Anything that can be coerced to a `StubbedRequest` class
#' object
#' @param ... Comma separated list of named variables. accepts the following:
#' `query`, `body`, `headers`, `basic_auth`. See Details.
#' @param .list named list, has to be one of `query`, `body`,
#' `headers` and/or `basic_auth`. An alternative to passing in via `...`.
#' Don't pass the same thing to both, e.g. don't pass 'query' to `...`, and
#' also 'query' to this parameter
#' @details `with` is a function in the `base` package, so we went with
#' `wi_th`
#' @return an object of class `StubbedRequest`, with print method describing
#' the stub
#' @note see more examples in [stub_request()]
#' @seealso [including()]
#' @details
#' Values for query, body, headers, and basic_auth:
#'
#' - query: (list) a named list. values are coerced to character
#' class in the recorded stub. You can pass numeric, integer, etc., but
#' all will be coerced to character.
#' - body: various, including character string, list, raw, numeric,
#' upload ([crul::upload()], [httr::upload_file()], [curl::form_file()], or
#' [curl::form_data()] they both create the same object in the end). for the
#' special case of an empty request body use `NA` instead of `NULL` because
#' with `NULL` we can't determine if the user did not supply a body or
#' they supplied `NULL` to indicate an empty body.
#' - headers: (list) a named list
#' - basic_auth: (character) a length two vector, username and password.
#' We don't do any checking of the username/password except to detect
#' edge cases where for example, the username/password
#' were probably not set by the user on purpose (e.g., a URL is
#' picked up by an environment variable). Only basic authentication
#' supported <https://en.wikipedia.org/wiki/Basic_access_authentication>.
#'
#' Note that there is no regex matching on query, body, or headers. They
#' are tested for matches in the following ways:
#'
#' - query: compare stubs and requests with `identical()`. this compares
#' named lists, so both list names and values are compared
#' - body: varies depending on the body format (list vs. character, etc.)
#' - headers: compare stub and request values with `==`. list names are
#' compared with `%in%`. `basic_auth` is included in headers (with the name
#' Authorization)
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
#' wi_th(req, headers = list(`User-Agent` = "webmockr/v1", hello = "world"))
#'
#' # .list - pass in a named list instead
#' wi_th(req, .list = list(body = list(foo = "bar")))
#'
#' # basic authentication
#' wi_th(req, basic_auth = c("user", "pass"))
#' wi_th(req, basic_auth = c("user", "pass"), headers = list(foo = "bar"))
#'
#' # partial matching, query params
#' ## including
#' wi_th(req, query = including(list(foo = "bar")))
#' ## excluding
#' wi_th(req, query = excluding(list(foo = "bar")))
#'
#' # partial matching, body
#' ## including
#' wi_th(req, body = including(list(foo = "bar")))
#' ## excluding
#' wi_th(req, body = excluding(list(foo = "bar")))
#'
#' # basic auth
#' ## including
#' wi_th(req, body = including(list(foo = "bar")))
#' ## excluding
#' wi_th(req, body = excluding(list(foo = "bar")))
wi_th <- function(.data, ..., .list = list()) {
  handle_stub_removal(.data, {
    assert_is(.data, "StubbedRequest")
    assert_stub_registered(.data)
    assert_is(.list, "list")
    z <- list(...)
    if (length(z) == 0) {
      z <- NULL
    }
    z <- c(z, .list)
    if (
      !any(c("query", "body", "headers", "basic_auth") %in% names(z)) &&
        length(z) != 0
    ) {
      abort("'wi_th' only accepts query, body, headers, basic_auth")
    }
    if (any(duplicated(names(z)))) {
      abort("can not have duplicated names")
    }
    assert_is(z$query, c("list", "partial"))
    if (!all(hz_namez(z$query))) {
      abort("'query' must be a named list")
    }
    assert_is(z$headers, "list")
    if (!all(hz_namez(z$headers))) {
      abort("'headers' must be a named list")
    }
    assert_is(z$basic_auth, "character")
    assert_length(z$basic_auth, 2)
    assert_not_function(z)

    .data$with(
      query = z$query,
      body = z$body,
      headers = z$headers,
      basic_auth = z$basic_auth
    )
  })
  return(.data)
}
