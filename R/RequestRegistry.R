#' @title HashCounter
#' @description hash with counter, to store requests, and count each time
#' it is used
#' @export
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
    #' @field hash (list) a list for internal use only
    hash = list(),

    #' @description Register a request by it's key
    #' @param key a character string of the request, serialized from
    #' [CrulAdapter] or another adapter
    #' @return nothing returned; registers request and iterates
    #' internal counter
    put = function(key) {
      if (missing(key)) stop("'key' required")
      self$hash[key] <- (self$hash[[key]] %||% 0) + 1
    },

    #' @description Get a request by key
    #' @param key a character string of the request, serialized from
    #' [CrulAdapter] or another adapter
    #' @return (character) an http request as a string
    get = function(key) {
      if (missing(key)) stop("'key' required")
      self$hash[[key]] %||% 0
    }
  )
)

#' @title RequestRegistry
#' @description keeps track of HTTP requests
#' @export
#' @family request-registry
#' @seealso [stub_registry()] and [StubRegistry]
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
     #' @field request_signatures a HashCounter object
    request_signatures = HashCounter$new(),

    #' @description print method for the `RequestRegistry` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat("<webmockr request registry> ", sep = "\n")
      cat(" Registered Requests", sep = "\n")
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

    #' @description Reset the registry to no registered requests
    #' @return nothing returned; ressets registry to no requests
    reset = function() {
      self$request_signatures <- HashCounter$new()
    },

    #' @description Register a request
    #' @param request a character string of the request, serialized from
    #' [CrulAdapter] or another adapter
    #' @return nothing returned; registers the request
    register_request = function(request) {
      self$request_signatures$put(request)
    }
  )
)

# initialize empty request registry on package load
webmockr_request_registry <- new.env()
webmockr_request_registry <- RequestRegistry$new()
