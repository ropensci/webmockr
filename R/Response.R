#' @title Response
#' @description custom webmockr http response class
#' @export
#' @examples \dontrun{
#' (x <- Response$new())
#'
#' x$set_url("https://httpbin.org/get")
#' x
#'
#' x$set_request_headers(list("Content-Type" = "application/json"))
#' x
#' x$request_headers
#'
#' x$set_response_headers(list("Host" = "httpbin.org"))
#' x
#' x$response_headers
#'
#' x$set_status(404)
#' x
#' x$get_status()
#'
#' x$set_body("hello world")
#' x
#' x$get_body()
#' # raw body
#' x$set_body(charToRaw("hello world"))
#' x
#' x$get_body()
#'
#' x$set_exception("exception")
#' x
#' x$get_exception()
#' }
Response <- R6::R6Class(
  "Response",
  public = list(
    #' @field url (character) a url
    url = NULL,
    #' @field body (various) list, character, etc
    body = NULL,
    #' @field content (various) response content/body
    content = NULL,
    #' @field request_headers (list) a named list
    request_headers = NULL,
    #' @field response_headers (list) a named list
    response_headers = NULL,
    #' @field options (character) list
    options = NULL,
    #' @field status_code (integer) an http status code
    status_code = 200,
    #' @field exception (character) an exception message
    exception = NULL,
    #' @field should_timeout (logical) should the response timeout?
    should_timeout = NULL,

    #' @description Create a new `Response` object
    #' @param options (list) a list of options
    #' @return A new `Response` object
    initialize = function(options = list()) {
      if (inherits(options, "file") || inherits(options, "character")) {
        self$options <- read_raw_response(options)
      } else {
        self$options <- options
      }
    },

    #' @description print method for the `Response` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat_line("<webmockr response> ")
      cat_line(paste0("  url: ", self$url))
      cat_line(paste0("  status: ", self$status_code))
      cat_line("  headers: ")
      cat_line("    request headers: ")
      for (i in seq_along(self$request_headers)) {
        cat_line(paste0(
          "     ",
          paste(names(self$request_headers)[i], self$request_headers[[i]],
            sep = ": "
          )
        ))
      }
      cat_line("    response headers: ")
      for (i in seq_along(self$response_headers)) {
        cat_line(paste0(
          "     ",
          paste(names(self$response_headers)[i], self$response_headers[[i]],
            sep = ": "
          )
        ))
      }
      cat_line(paste0("  exception: ", self$exception))
      cat_line(paste0("  body length: ", length(self$body)))
    },

    #' @description set the url for the response
    #' @param url (character) a url
    #' @return nothing returned; sets url
    set_url = function(url) {
      self$url <- url
    },
    #' @description get the url for the response
    #' @return (character) a url
    get_url = function() self$url,

    #' @description set the request headers for the response
    #' @param headers (list) named list
    #' @param capitalize (logical) whether to capitalize first letters of
    #' each header; default: `TRUE`
    #' @return nothing returned; sets request headers on the response
    set_request_headers = function(headers, capitalize = TRUE) {
      self$request_headers <- private$normalize_headers(headers, capitalize)
    },
    #' @description get the request headers for the response
    #' @return (list) request headers, a named list
    get_request_headers = function() self$request_headers,

    #' @description set the response headers for the response
    #' @param headers (list) named list
    #' @param capitalize (logical) whether to capitalize first letters of
    #' each header; default: `TRUE`
    #' @return nothing returned; sets response headers on the response
    set_response_headers = function(headers, capitalize = TRUE) {
      self$response_headers <- private$normalize_headers(headers, capitalize)
    },
    #' @description get the response headers for the response
    #' @return (list) response headers, a named list
    get_respone_headers = function() self$response_headers,

    #' @description set the body of the response
    #' @param body (various types)
    #' @param disk (logical) whether its on disk; default: `FALSE`
    #' @return nothing returned; sets body on the response
    set_body = function(body, disk = FALSE) {
      self$body <- self$content <- if (is.character(body)) {
        stopifnot(length(body) <= 1)
        if (disk) body else charToRaw(body)
      } else if (is.raw(body)) {
        body
      } else {
        raw(0)
      }
    },
    #' @description get the body of the response
    #' @return various
    get_body = function() self$body %||% "",

    #' @description set the http status of the response
    #' @param status (integer) the http status
    #' @return nothing returned; sets the http status of the response
    set_status = function(status) {
      self$status_code <- status
    },
    #' @description get the http status of the response
    #' @return (integer) the http status
    get_status = function() self$status_code %||% 200,

    #' @description set an exception
    #' @param exception (character) an exception string
    #' @return nothing returned; sets an exception
    set_exception = function(exception) {
      self$exception <- exception
    },
    #' @description get the exception, if set
    #' @return (character) an exception
    get_exception = function() self$exception
  ),
  private = list(
    normalize_headers = function(x, capitalize = TRUE) {
      normalize_headers(x, capitalize)
    }
  )
)
