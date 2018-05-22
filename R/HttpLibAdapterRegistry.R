#' http lib adapter registry
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`register(x)`}{
#'       Register an http library adapter
#'       x: an http lib adapter, e.g., [CrulAdapter]
#'       return: nothing, registers the library adapter
#'     }
#'   }
#' @format NULL
#' @usage NULL
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
        cat(sprintf("  %s: webmockr:::%s", self$adapters[[i]]$name,
                    class(self$adapters[[i]])[1]), sep = "\n")
      }
    },

    register = function(x) {
      # FIXME: when other adapters supported, change this inherits test
      if (!inherits(x, c("CrulAdapter", "HttrAdapter"))) {
        stop("'x' must be an adapter, such as CrulAdapter", call. = FALSE)
      }
      self$adapters <- cc(list(self$adapters, x))
    }
  )
)
