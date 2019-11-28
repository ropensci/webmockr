#' StubbedRequest class
#'
#' @export
#' @param method the HTTP method (any, head, get, post, put,
#' patch, or delete). "any" matches any HTTP method. required.
#' @param uri (character) request URI. either this or `uri_regex`
#' required
#' @param uri_regex (character) request URI as regex. either this or `uri`
#' required
#' @details
#' **Methods**
#'   \describe{
#'     \item{`with(query, body, headers)`}{
#'       Set expectations for what's given in HTTP request
#'       \itemize{
#'        \item query (list) request query params, as a named list. optional
#'        \item body (list) request body, as a named list. optional
#'        \item headers (list) request headers as a named list. optional.
#'       }
#'     }
#'     \item{`to_return(status, body, headers)`}{
#'       Set expectations for what's returned in HTTP response
#'       \itemize{
#'        \item status (numeric) an HTTP status code
#'        \item body (list) response body, one of: `character`, `json`,
#'        `list`, `raw`, `numeric`, `NULL`, `FALSE`, or a file connection
#'        (other connetion types not supported)
#'        \item headers (list) named list, response headers. optional.
#'       }
#'     }
#'     \item{`to_timeout()`}{
#'       Response should time out
#'     }
#'     \item{`to_raise(x)`}{
#'       Response should raise an exception `x`
#'       \itemize{
#'        \item x (numeric) an HTTP status code
#'       }
#'     }
#'     \item{`to_s()`}{
#'       Response as a character string
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @seealso [stub_request()]
#' @examples \dontrun{
#' x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' x$method
#' x$uri
#' x$with(headers = list('User-Agent' = 'R', apple = "good"))
#' x$to_return(status = 200, body = "foobar", headers = list(a = 5))
#' x
#' x$to_s()
#'
#' # raw body
#' x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' x$to_return(status = 200, body = raw(0), headers = list(a = 5))
#' x$to_s()
#'
#' # file path
#' x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' f <- tempfile()
#' x$to_return(status = 200, body = file(f), headers = list(a = 5))
#' x
#' x$to_s()
#' unlink(f)
#' 
#' # to_file(): file path and payload to go into the file
#' #   payload written to file during mocked response creation
#' x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' f <- tempfile()
#' x$to_return(status = 200, body = mock_file(f, "{\"foo\": \"bar\"}"),
#'   headers = list(a = 5))
#' x
#' x$to_s()
#' unlink(f)
#'
#' # uri_regex
#' (x <- StubbedRequest$new(method = "get", uri_regex = ".+ossref.org"))
#' x$method
#' x$uri
#' x$to_s()
#'
#' # to timeout
#' (x <- StubbedRequest$new(method = "get", uri_regex = ".+ossref.org"))
#' x$to_s()
#' x$to_timeout()
#' x$to_s()
#'
#' # to raise
#' library(fauxpas)
#' (x <- StubbedRequest$new(method = "get", uri_regex = ".+ossref.org"))
#' x$to_s()
#' x$to_raise(HTTPBadGateway)
#' x$to_s()
#' }
StubbedRequest <- R6::R6Class(
  "StubbedRequest",
  public = list(
    method = NULL,
    uri = NULL,
    uri_regex = NULL,
    uri_parts = NULL,
    host = NULL,
    query = NULL,
    body = NULL,
    request_headers = NULL,
    response_headers = NULL,
    responses_sequences = NULL,
    status_code = NULL,
    timeout = FALSE,
    exceptions = list(),
    raise = FALSE,

    initialize = function(method, uri = NULL, uri_regex = NULL) {
      if (!missing(method)) {
        verb <- match.arg(tolower(method), http_verbs)
        self$method <- verb
      }
      if (is.null(uri) && is.null(uri_regex)) {
        stop("one of uri or uri_regex is required", call. = FALSE)
      }
      # self$uri <- if (!is.null(uri)) uri else uri_regex
      self$uri <- uri
      self$uri_regex <- uri_regex
      if (!is.null(uri)) self$uri_parts <- parseurl(self$uri)
    },

    print = function(x, ...) {
      cat("<webmockr stub> ", sep = "\n")
      cat(paste0("  method: ", self$method), sep = "\n")
      cat(paste0("  uri: ", self$uri %||% self$uri_regex), sep = "\n")
      cat("  with: ", sep = "\n")
      cat(paste0("    query: ", hdl_lst(self$query)), sep = "\n")
      cat(paste0("    body: ", hdl_lst(self$body)), sep = "\n")
      cat(paste0("    request_headers: ", hdl_lst(self$request_headers)),
          sep = "\n")
      cat("  to_return: ", sep = "\n")
      cat(paste0("    status: ", hdl_lst(self$responses_sequences$status)),
          sep = "\n")
      cat(paste0("    body: ", hdl_lst(self$responses_sequences$body)),
          sep = "\n")
      cat(paste0("    response_headers: ",
        hdl_lst(self$responses_sequences$headers)),
          sep = "\n")
      cat(paste0("  should_timeout: ", self$timeout), sep = "\n")
      cat(paste0("  should_raise: ",
        if (self$raise)
          paste0(vapply(self$exceptions, "[[", "", "classname"),
            collapse = ", ")
        else "FALSE"
      ), sep = "\n")
    },

    with = function(query = NULL, body = NULL, headers = NULL) {
      self$query <- query
      self$body <- body
      self$request_headers <- headers
    },

    to_return = function(status, body, headers) {
      body <- if (inherits(body, "connection")) {
        bod_sum <- summary(body)
        close.connection(body)
        if (bod_sum$class != "file")
          stop("'to_return' only supports connections of type 'file'")
        structure(bod_sum$description, type = "file")
      } else {
        body
      }
      self$response_headers <- headers
      self$responses_sequences <- list(
        status = status,
        body = body,
        headers = headers
      )
      self$responses_sequences$body_raw <- {
        if (inherits(body, "mock_file")) {
          body
        } else if (inherits(body, "logical")) {
          if (!body) {
            raw()
          } else {
            webmockr_stub_registry$remove_request_stub(self)
            stop(paste0("Unknown type of `body`: ",
              "must be NULL, FALSE, character, raw or list; stub removed"),
            call. = FALSE)
          }
        } else if (inherits(body, "raw")) {
          body
        } else if (is.null(body)) {
          raw()
        } else if (is.character(body) || inherits(body, "json")) {
          if (!is.null(attr(body, "type"))) {
            stopifnot(attr(body, "type") == "file")
            body
          } else {
            charToRaw(body)
          }
        } else if (!is.list(body)) {
          webmockr_stub_registry$remove_request_stub(self)
          stop(paste0("Unknown type of `body`: ",
            "must be numeric, NULL, FALSE, character, json, ",
            "raw, list, or file connection; stub removed"),
            call. = FALSE)
        } else {
          charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
        }
      }
    },

    to_timeout = function() {
      self$timeout <- TRUE
    },

    to_raise = function(x) {
      self$exceptions <- if (inherits(x, "list")) x else list(x)
      self$raise <- TRUE
    },

    to_s = function() {
      toret <- c(
        make_body(self$responses_sequences$body),
        make_status(self$responses_sequences$status),
        make_headers(self$responses_sequences$headers)
      )
      gsub("^\\s+|\\s+$", "", sprintf(
        "  %s: %s %s %s %s %s %s",
        toupper(self$method),
        url_builder(self$uri %||% self$uri_regex, self$query),
        make_body(self$body),
        make_headers(self$request_headers),
        if (any(nchar(toret) > 0)) {
          sprintf("| to_return: %s %s %s", toret[1], toret[2], toret[3])
        } else {
          ""
        },
        if (self$timeout) "| should_timeout: TRUE" else "",
        if (self$raise)
          paste0("| to_raise: ",
            paste0(vapply(self$exceptions, "[[", "", "classname"),
            collapse = ", ")) else ""
      ))
    }
  )
)
