convert_registry_to_list <- function() {
  data <- request_registry()
  reqs <- unname(data$request_signatures$hash)
  out <- lapply(reqs, \(x) {
    tmp <- c(key = x$key, count = x$count, x$sig$as_list(), x)
    names(tmp)[names(tmp) == "sig"] <- "request"
    tmp
  })
  out
}

#' Request registry filter
#'
#' If no filters are given, returns all requests
#'
#' @export
#' @param method (character) http method. default: `NULL`
#' @param url (character) a url. default: `NULL`
#' @param body (various) a request body. default: `NULL`.
#' Can be any of: `character`, `json`, `list`, `raw`, `numeric`,
#' `NULL`, `FALSE`
#' @param headers (list) if given, must be a named list. default: `NULL`
#' @return list, length of number of unique requests recorded, or
#' those requests matching filters supplied by parameters
#' @examples
#' enable(adapter="httr")
#'
#' stub_request("any", uri_regex = ".+")
#'
#' library(httr)
#' GET("http://example.com")
#' GET("https://hb.cran.dev/get")
#' POST("https://hb.cran.dev/post", body = list(fruit = "apple"))
#' POST("https://hb.cran.dev/post", body = list(cheese = "swiss"))
#' GET("https://hb.cran.dev/get", add_headers(Accept = "application/json"))
#'
#' request_registry_filter()
#' request_registry_filter(method="get")
#' request_registry_filter(method="post")
#' request_registry_filter(method="get", url="http://example.com")
#' request_registry_filter(method="post", body=list(fruit = "apple"))
#' request_registry_filter(method="post", body=list(cheese = "swiss"))
#' request_registry_filter(method="post", body=list(cheese = "cheddar"))
#' request_registry_filter(method="get", headers=list(Accept = "application/json"))
#'
#' match <- request_registry_filter(method="post")[[1]]
#' match$request
#' match$request$to_s()
#' match$count
#'
#' disable()
request_registry_filter <- function(
  method = NULL,
  url = NULL,
  body = NULL,
  headers = NULL
) {
  assert_is(url, "character")
  assert_is(method, "character")
  assert_is(headers, "list")
  if (!all(hz_namez(headers))) {
    abort("'headers' must be a named list")
  }
  requests <- convert_registry_to_list()

  if (!is.null(method)) {
    requests <- Filter(
      \(x) MethodPattern$new(x$method)$matches(method),
      requests
    )
  }
  if (!is.null(url)) {
    requests <- Filter(\(x) UriPattern$new(x$uri)$matches(url), requests)
  }
  if (!is.null(body)) {
    requests <- Filter(\(x) BodyPattern$new(x$body)$matches(body), requests)
  }
  if (!is.null(headers)) {
    requests <- Filter(
      \(x) HeadersPattern$new(x$headers)$matches(headers),
      requests
    )
  }

  requests
}
