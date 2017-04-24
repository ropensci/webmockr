#' General purpose request signature builder
#'
#' @export
#' @param method the HTTP method (any, head, options, get, post, put,
#' patch, trace, or delete). "any" matches any HTTP method. required.
#' @param uri (character) request URI. required.
#' @param options (list) options. optional
#' @details
#' **Methods**
#'   \describe{
#'     \item{`to_s()`}{
#'       Request signature to a string
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples
#' # make request signature
#' x <- RequestSignature$new(method = "get", uri = "https:/httpbin.org/get")
#' # method
#' x$method
#' # uri
#' x$uri
#' # request signature to string
#' x$to_s()

RequestSignature <- R6::R6Class(
  'RequestSignature',
  public = list(
    method = NULL,
    uri = NULL,
    body = NULL,
    headers = NULL,
    proxies = NULL,
    auth = NULL,

    initialize = function(method, uri, options = list()) {
      verb <- match.arg(tolower(method), http_verbs)
      self$method <- verb
      self$uri <- uri
      if (length(options)) private$assign_options(options)
    },

    to_s = function() {
      gsub("^\\s+|\\s+$", "", paste(
        toupper(self$method),
        self$uri,
        if (!is.null(self$body) && length(self$body)) {
          paste0(" with body ", self$body)
        },
        if (!is.null(self$headers) && length(self$headers)) {
          paste0(
            " with headers ",
            sprintf("{%s}",
                    paste(names(self$headers),
                          unlist(unname(self$headers)), sep = ": ",
                          collapse = ", "))
          )
        }
      ))
    }
  ),

  private = list(
    assign_options = function(options) {
      if ('body' %in% names(options)) {
        if (!is.null(options$body) && length(options)) {
          self$body <- options$body
        }
      }
      if ('headers' %in% names(options)) {
        if (!is.null(options$headers) && length(options)) {
          self$headers <- options$headers
        }
      }
    }
  )
)
