#' @title HttpLibAdapaterRegistry
#' @description http lib adapter registry
#' @export
#' @examples
#' x <- HttpLibAdapaterRegistry$new()
#' x$register(CrulAdapter$new())
#' x
#' x$adapters
#' x$adapters[[1]]$name
HttpLibAdapaterRegistry <- R6::R6Class(
  'HttpLibAdapaterRegistry',
  public = list(
    #' @field adapters list
    adapters = NULL,

    #' @description print method for the `HttpLibAdapaterRegistry` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat("<HttpLibAdapaterRegistry> ", sep = "\n")
      for (i in seq_along(self$adapters)) {
        cat(sprintf("  %s: webmockr:::%s", self$adapters[[i]]$name,
                    class(self$adapters[[i]])[1]), sep = "\n")
      }
    },

    #' @description Register an http library adapter
    #' @param x an http lib adapter, e.g., [CrulAdapter]
    #' @return nothing, registers the library adapter
    register = function(x) {
      # FIXME: when other adapters supported, change this inherits test
      if (!inherits(x, c("CrulAdapter", "HttrAdapter"))) {
        stop("'x' must be an adapter, such as CrulAdapter", call. = FALSE)
      }
      self$adapters <- c(self$adapters, x)
    }
  )
)
