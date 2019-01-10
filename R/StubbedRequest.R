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
#'       Set expectations for what's returned in HTTP resonse
#'       \itemize{
#'        \item status (numeric) an HTTP status code
#'        \item body (list) response body, as a list. optional
#'        \item headers (list) named list, response headers. optional.
#'       }
#'     }
#'     \item{`to_s()`}{
#'       Response as a string
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @seealso [stub_request()]
#' @examples \dontrun{
#' x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' x$method
#' x$uri
#' x$with(headers = list('User-Agent' = 'R'))
#' x$to_return(status = 200, body = "foobar", headers = list(a = 5))
#' x
#' x$to_s()
#'
#' # uri_regex
#' (x <- StubbedRequest$new(method = "get", uri_regex = ".+ossref.org"))
#' x$method
#' x$uri
#' x$to_s()
#'
#' (x <- StubbedRequest$new(method = "get", uri_regex = ".+ossref.org"))
#' x$to_s()
#' x$timeout <- TRUE
#' x$to_s()
#' }
StubbedRequest <- R6::R6Class(
  'StubbedRequest',
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
    response = NULL,
    responses_sequences = NULL,
    status_code = NULL,
    timeout = FALSE,
    exceptions = NULL,
    raise = FALSE,

    initialize = function(method, uri = NULL, uri_regex = NULL) {
      if (!missing(method)) {
        verb <- match.arg(tolower(method), http_verbs)
        self$method <- verb
      }
      if (is.null(uri) && is.null(uri_regex)) {
        stop("one of uri or uri_regex is required", call. = FALSE)
      }
      self$uri <- if (!is.null(uri)) uri else uri_regex
      self$uri_parts <- parseurl(self$uri)
    },

    print = function(x, ...) {
      cat("<webmockr stub> ", sep = "\n")
      cat(paste0("  method: ", self$method), sep = "\n")
      cat(paste0("  uri: ", self$uri), sep = "\n")
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
      cat(paste0("    response_headers: ", hdl_lst(self$responses_sequences$headers)),
          sep = "\n")
      cat(paste0("  should_timeout: ", self$timeout), sep = "\n")
      cat(paste0("  should_raise: ",
        if (self$raise) paste0(vapply(self$exceptions, "[[", "", "classname"), collapse = ", ") else "FALSE"
      ), sep = "\n")
    },

    with = function(query = NULL, body = NULL, headers = NULL) {
      self$query <- query
      self$body <- body
      self$request_headers <- headers
    },

    to_return = function(status, body, headers) {
      self$response_headers <- headers
      self$responses_sequences <- list(
        status = status,
        body = body,
        headers = headers
      )
      self$responses_sequences$body_raw = {
        if (inherits(body, "logical")) {
          if (!body) raw() else stop("Unknown type of `body`: must be NULL, FALSE, character, raw or list",
                                     call. = FALSE)
        } else if (inherits(body, "raw")) {
          body
        } else if (is.null(body)) {
          raw()
        } else if (is.character(body)) {
          charToRaw(body)
        } else if (!is.list(body)) {
          stop("Unknown type of `body`: must be NULL, FALSE, character, raw or list",
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
      self$exceptions <- x
      self$raise <- TRUE
    },

    to_s = function() {
      toret <- c(
        make_body(self$responses_sequences$body),
        make_status(self$responses_sequences$status),
        make_headers(self$responses_sequences$headers)
      )
      gsub("^\\s+|\\s+$", "", sprintf(
        "  %s: %s %s %s %s %s",
        toupper(self$method),
        url_builder(self$uri, self$query),
        make_body(self$body),
        make_headers(self$request_headers),
        # response data
        if (any(nchar(toret) > 0)) {
          sprintf("| to_return: %s %s %s", toret[1], toret[2], toret[3])
        } else {
          ""
        },
        # timeout info
        if (self$timeout) "| should_timeout: TRUE" else ""
      ))
    }
  )
)
