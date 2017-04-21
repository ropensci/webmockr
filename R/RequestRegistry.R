# hash with counter, to store requests, and count each time it is used
HashCounter <- R6::R6Class(
  'HashCounter',
  public = list(
    hash = list(),

    put = function(key) {
      self$hash[key] <- (self$hash[[key]] %||% 0) + 1
    },

    get = function(key) {
      self$hash[[key]] %||% 0
    }
  )
)

#' Request registry
#'
#' @keywords internal
#' @param request A request - an object of class \code{RegisteredRequest}
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{register_request(request)}}{
#'       Register a request
#'         - request: a character string of the request, serialized from
#'         \code{CrulAdapter} or other adapter
#'     }
#'     \item{\code{reset()}}{
#'       Reset the registry to no registered requests
#'     }
#'   }
#' @examples \dontrun{
#' x <- RequestRegistry$new()
#' x$register_request(request = "GET http://scottchamberlain.info")
#' x$register_request(request = "GET http://scottchamberlain.info")
#' x$register_request(request = "POST https://httpbin.org/post")
#' # print method to list requests
#' x
#'
#' # reset the request registry
#' x$reset()
#' }
RequestRegistry <- R6::R6Class(
  'RequestRegistry',
  public = list(
    request = NULL,
    request_signatures = HashCounter$new(),

    print = function(x, ...) {
      cat("<webmockr request registry> ", sep = "\n")
      cat("  Registered Requests", sep = "\n")
      for (i in seq_along(self$request_signatures$hash)) {
        cat(
          sprintf(
            "  %s was made %s times\n",
            names(self$request_signatures$hash)[i],
            self$request_signatures$hash[[i]]
          ),
          sep = "\n"
        )
      }
      invisible(self$request_signatures$hash)
    },

    reset = function() {
      self$request_signatures <- HashCounter$new()
    },

    register_request = function(request) {
      self$request_signatures$put(request)
    }
  )
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# initialize empty request registry on package load
webmockr_request_registry <- new.env()
webmockr_request_registry <- RequestRegistry$new()
