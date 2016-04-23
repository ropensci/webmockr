#' StubbedRequest class
#'
#' @keywords internal
#' @param method the HTTP method (i.e. :head, :options, :get, :post, :put, :patch or :delete)
#' @param uri the request URI
#' @param body the request body
#' @param headers the request headers
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{to_hash()}}{
#'       Create a hash.
#'     }
#'     \item{\code{from_hash()}}{
#'       Get a hash back to an R list.
#'     }
#'   }
#' @examples \dontrun{
#' x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' x$method
#' x$uri
#' x$with(headers = list('User-Agent' = 'R'))
#' x$to_return(status = 200, body = "", headers = list())
#' }
StubbedRequest <- R6::R6Class(
  'StubbedRequest',
  public = list(
    method = NULL,
    uri = NULL,
    host = NULL,
    query = NULL,
    body = NULL,
    headers = NULL,
    responses_sequences = NULL,

    initialize = function(method, uri) {
      if (!missing(method)) {
        verb <- match.arg(tolower(method), http_verbs)
        self$method <- verb
      }
      if (!missing(uri)) self$uri <- uri
    },

    print = function(x, ...) {
      cat("<webmockr stub> ", sep = "\n")
      cat(paste0("  method: ", self$method), sep = "\n")
      cat(paste0("  uri: ", self$uri), sep = "\n")
      cat("  with: ", sep = "\n")
      cat(paste0("    query: ", hdl_lst(self$query)), sep = "\n")
      cat(paste0("    body: ", hdl_lst(self$body)), sep = "\n")
      cat(paste0("    headers: ", hdl_lst(self$headers)), sep = "\n")
      cat("  to_return: ", sep = "\n")
      cat(paste0("    status: ", hdl_lst(self$responses_sequences$status)), sep = "\n")
      cat(paste0("    body: ", hdl_lst(self$responses_sequences$body)), sep = "\n")
      cat(paste0("    headers: ", hdl_lst(self$responses_sequences$headers)), sep = "\n")
    },

    with = function(query = NULL, body = NULL, headers = NULL) {
      self$query <- query
      self$body <- body
      self$headers <- headers
    },

    to_return = function(status, body, headers) {
      self$responses_sequences <- list(
        status = status,
        body = body,
        headers = headers
      )
    }
  )
)
