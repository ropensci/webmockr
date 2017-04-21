#' StubbedRequest class
#'
#' @export
#' @keywords internal
#' @param method the HTTP method (any, head, options, get, post, put,
#' patch, trace, or delete). "any" matches any HTTP method. required.
#' @param uri (character) request URI. required.
#' @param body (list) request body, as a list. optional
#' @param query (list) query parameters, as a list. optional
#' @param headers (list) request headers. optional.
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{with()}}{
#'       with slots for:
#'       \itemize{
#'        \item status
#'        \item body
#'        \item request_headers
#'        \item response_headers
#'       }
#'     }
#'     \item{\code{to_return()}}{
#'       Stubbed response
#'     }
#'   }
#' @examples \dontrun{
#' x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' x$method
#' x$uri
#' x$with(request_headers = list('User-Agent' = 'R'))
#' x$to_return(status = 200, body = "foobar", response_headers = list())
#' x$to_s()
#' }
StubbedRequest <- R6::R6Class(
  'StubbedRequest',
  public = list(
    method = NULL,
    uri = NULL,
    uri_parts = NULL,
    host = NULL,
    query = NULL,
    body = NULL,
    request_headers = NULL,
    response_headers = NULL,
    response = NULL,
    responses_sequences = NULL,

    initialize = function(method, uri) {
      if (!missing(method)) {
        verb <- match.arg(tolower(method), http_verbs)
        self$method <- verb
      }
      if (!missing(uri)) {
        self$uri <- uri
        self$uri_parts <- parseurl(self$uri)
      }
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
      cat(paste0("    response_headers: ", hdl_lst(self$response_headers)),
          sep = "\n")
      cat("  to_return: ", sep = "\n")
      cat(paste0("    status: ", hdl_lst(self$responses_sequences$status)),
          sep = "\n")
      cat(paste0("    body: ", hdl_lst(self$responses_sequences$body)),
          sep = "\n")
      cat(paste0("    response_headers: ", hdl_lst(self$responses_sequences$headers)),
          sep = "\n")
    },

    with = function(query = NULL, body = NULL, request_headers = NULL) {
      self$query <- query
      self$body <- body
      self$request_headers <- request_headers
    },

    to_return = function(status, body, response_headers) {
      self$responses_sequences <- list(
        status = status,
        body = body,
        response_headers = response_headers
      )
    },

    to_s = function() {
      gsub("^\\s+|\\s+$", "", sprintf(
        "  %s: %s %s %s",
        self$method,
        url_build(
          self$uri,
          self$query
        ),
        make_body(self$body),
        make_headers(self$request_headers)
      ))
    }
  )
)
