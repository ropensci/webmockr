#' StubbedRequest class
#'
#' @export
#' @param method the HTTP method (any, head, get, post, put,
#' patch, or delete). "any" matches any HTTP method. required.
#' @param uri (character) request URI. required.
#' @details
#' **Methods**
#'   \describe{
#'     \item{`with(query, body, request_headers)`}{
#'       Set expectations for what's given in HTTP request
#'       \itemize{
#'        \item query (list) request query params, as a named list. optional
#'        \item body (list) request body, as a named list. optional
#'        \item headers (list) request headers as a named list. optional.
#'       }
#'     }
#'     \item{`to_return(status, body, response_headers)`}{
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
#' @examples \dontrun{
#' x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' x$method
#' x$uri
#' x$with(headers = list('User-Agent' = 'R'))
#' x$to_return(status = 200, body = "foobar", headers = list(a = 5))
#' x
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
      # cat(paste0("    response_headers: ", hdl_lst(self$response_headers)),
      #     sep = "\n")
      cat("  to_return: ", sep = "\n")
      cat(paste0("    status: ", hdl_lst(self$responses_sequences$status)),
          sep = "\n")
      cat(paste0("    body: ", hdl_lst(self$responses_sequences$body)),
          sep = "\n")
      cat(paste0("    response_headers: ", hdl_lst(self$responses_sequences$headers)),
          sep = "\n")
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
    },

    to_s = function() {
      toret <- c(
        make_body(self$responses_sequences$body),
        make_status(self$responses_sequences$status),
        make_headers(self$responses_sequences$headers)
      )
      gsub("^\\s+|\\s+$", "", sprintf(
        "  %s: %s %s %s %s",
        self$method,
        url_build(self$uri, self$query),
        make_body(self$body),
        make_headers(self$request_headers),
        # response data
        if (any(nchar(toret) > 0)) {
          sprintf("| to_return: %s %s %s", toret[1], toret[2], toret[3])
        } else {
          ""
        }
      ))
    }
  )
)
