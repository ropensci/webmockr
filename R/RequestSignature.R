#' general purpose request signature builder
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
        if (!is.null(self$body)) paste0(" with body ", self$body),
        if (!is.null(self$headers)) paste0(" with headers ", self$headers)
      ))
    }
  ),

  private = list(
    assign_options = function(options) {
      self$body <- if ('body' %in% names(options)) options['body']
      self$headers <- if ('headers' %in% names(options)) options['headers']
    }
  )
)
