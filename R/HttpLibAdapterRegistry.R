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
      cat_line("<HttpLibAdapaterRegistry> ")
      for (i in seq_along(self$adapters)) {
        cat_line(
          sprintf("  %s: webmockr:::%s", 
            self$adapters[[i]]$name,
            class(self$adapters[[i]])[1]
          )
        )
      }
    },

    #' @description Register an http library adapter
    #' @param x an http lib adapter, e.g., [CrulAdapter]
    #' @return nothing, registers the library adapter
    register = function(x) {
      if (!inherits(x, c("CrulAdapter", "HttrAdapter", "Httr2Adapter"))) {
        abort("'x' must be an adapter, such as CrulAdapter")
      }
      self$adapters <- c(self$adapters, x)
    }
  )
)
