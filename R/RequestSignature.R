#' general purpose request signature builder
#' @export
#' @keywords internal
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
      self$method <- method
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
