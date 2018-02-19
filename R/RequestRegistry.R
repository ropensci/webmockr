#' hash with counter, to store requests, and count each time it is used
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`put(key)`}{
#'       Register a request by it's key
#'       - key: a character string of the request, serialized from
#'        [CrulAdapter] or other adapter
#'     }
#'     \item{`get(key)`}{
#'       Get a request by key
#'       - key: a character string of the request, serialized from
#'        [CrulAdapter] or other adapter
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @family request-registry
#' @examples
#' x <- HashCounter$new()
#' x$put("foo bar")
#' x$put("foo bar")
#' x$put("hello world")
#' x$put("hello world")
#' x$put("hello world")
#' x$hash
HashCounter <- R6::R6Class(
  'HashCounter',
  public = list(
    hash = list(),

    put = function(key) {
      if (missing(key)) stop("'key' required")
      self$hash[key] <- (self$hash[[key]] %||% 0) + 1
    },

    get = function(key) {
      if (missing(key)) stop("'key' required")
      self$hash[[key]] %||% 0
    }
  )
)

#' Request registry
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`register_request(request)`}{
#'       Register a request
#'         - request: a character string of the request, serialized from
#'         [CrulAdapter] or other adapter
#'     }
#'     \item{`reset()`}{
#'       Reset the registry to no registered requests
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @family request-registry
#' @examples
#' x <- RequestRegistry$new()
#' x$register_request(request = "GET http://scottchamberlain.info")
#' x$register_request(request = "GET http://scottchamberlain.info")
#' x$register_request(request = "POST https://httpbin.org/post")
#' # print method to list requests
#' x
#'
#' # hashes, and number of times each requested
#' x$request_signatures$hash
#'
#' # reset the request registry
#' x$reset()
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

# initialize empty request registry on package load
webmockr_request_registry <- new.env()
webmockr_request_registry <- RequestRegistry$new()
