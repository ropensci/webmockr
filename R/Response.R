#' Response class
#'
#' @export
#' @param options (list) a list of options
#' @details
#' **Methods**
#'   \describe{
#'     \item{`set_request_headers(headers)`}{
#'       set request headers
#'       - headers: a list of key-value pair headers
#'     }
#'     \item{`get_request_headers()`}{
#'       get request headers
#'     }
#'     \item{`set_response_headers(headers)`}{
#'       set response headers
#'       - headers: a list of key-value pair headers
#'     }
#'     \item{`get_response_headers()`}{
#'       get response headers
#'     }
#'     \item{`set_body(body)`}{
#'       - body: must be a string
#'     }
#'     \item{`get_body()`}{
#'       get body
#'     }
#'     \item{`set_status()`}{
#'       - body: must be an integer status code
#'     }
#'     \item{`get_status()`}{
#'       get status code
#'     }
#'     \item{`set_exception()`}{
#'       set exception
#'     }
#'     \item{`get_exception()`}{
#'       get exception
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' (x <- Response$new())
#'
#' x$set_url("https://httpbin.org/get")
#' x
#'
#' x$set_request_headers(list('Content-Type' = "application/json"))
#' x
#' x$request_headers
#'
#' x$set_response_headers(list('Host' = "httpbin.org"))
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
#'
#' x$set_exception("exception")
#' x
#' x$get_exception()
#' }
Response <- R6::R6Class(
  'Response',
  public = list(
    url = NULL,
    body = NULL,
    content = NULL,
    request_headers = NULL,
    response_headers = NULL,
    options = NULL,
    status_code = 200,
    exception = NULL,
    should_timeout = NULL,

    initialize = function(options = list()) {
      if (inherits(options, "file") || inherits(options, "character")) {
        self$options <- read_raw_response(options)
      } else {
        self$options <- options
      }
    },

    print = function(x, ...) {
      cat("<webmockr response> ", sep = "\n")
      cat(paste0("  url: ", self$url), sep = "\n")
      cat(paste0("  status: ", self$status_code), sep = "\n")
      cat("  headers: ", sep = "\n")
      for (i in seq_along(self$request_headers)) {
        cat("    request headers: ", sep = "\n")
        cat(paste0("     ",
            paste(names(self$request_headers)[i], self$request_headers[[i]],
                  sep = ": ")), sep = "\n")
      }
      for (i in seq_along(self$response_headers)) {
        cat("    response headers: ", sep = "\n")
        cat(paste0("     ",
            paste(names(self$response_headers)[i], self$response_headers[[i]],
                   sep = ": ")), sep = "\n")
      }
      cat(paste0("  exception: ", self$exception), sep = "\n")
      cat(paste0("  body length: ", length(self$body)), sep = "\n")
    },

    set_url = function(url) {
      self$url <- url
    },
    get_url = function() self$url,

    set_request_headers = function(headers, capitalize = TRUE) {
      self$request_headers <- private$normalize_headers(headers, capitalize)
    },
    get_request_headers = function() self$request_headers,

    set_response_headers = function(headers, capitalize = TRUE) {
      self$response_headers <- private$normalize_headers(headers, capitalize)
    },
    get_respone_headers = function() self$response_headers,

    set_body = function(body) {
      self$body <- body
      self$content <- if (!is.null(body) && is.character(body)) {
        charToRaw(body)
      } else {
        raw(0)
      }
    },
    get_body = function() self$body %||% '',

    set_status = function(status) {
      self$status_code <- status
    },
    get_status = function() self$status_code %||% 200,

    set_exception = function(exception) {
      self$exception <- exception
    },
    get_exception = function() self$exception
  ),

  private = list(
    normalize_headers = function(x, capitalize = TRUE) normalize_headers(x, capitalize)
  )
)
