#' http lib adapter registry
#'
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
    adapters = NULL,

    print = function(x, ...) {
      cat("<HttpLibAdapaterRegistry> ", sep = "\n")
      for (i in seq_along(self$adapters)) {
        cat(sprintf("  %s: webmockr:::%s", self$adapters[[i]]$name, class(self$adapters[[i]])[1]), sep = "\n")
      }
    },

    register = function(x) {
      self$adapters <- cc(list(self$adapters, x))
    }
  )
)

http_lib_adapter_registry <- HttpLibAdapaterRegistry$new()
