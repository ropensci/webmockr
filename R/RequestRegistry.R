#' @title HashCounter
#' @description hash with counter, to store requests, and count each time
#' it is used
#' @export
#' @family request-registry
#' @examples
#' x <- HashCounter$new()
#' x$hash
#' z <- RequestSignature$new(method = "get", uri = "https:/httpbin.org/get")
#' x$put(z)
#' x$hash
#' x$get(z)
#' x$put(z)
#' x$get(z)
HashCounter <- R6::R6Class(
  "HashCounter",
  public = list(
    #' @field hash (list) a list for internal use only, with elements
    #' `key`, `sig`, and `count`
    hash = list(),

    #' @description Register a request by it's key
    #' @param req_sig an object of class `RequestSignature`
    #' @return nothing returned; registers request and iterates
    #' internal counter
    put = function(req_sig) {
      assert_is(req_sig, "RequestSignature")
      key <- req_sig$to_s()
      self$hash[[key]] <- list(
        key = key,
        sig = req_sig,
        count = (self$hash[[key]]$count %||% 0) + 1
      )
    },

    #' @description Get a request by key
    #' @param req_sig an object of class `RequestSignature`
    #' @return (integer) the count of how many times the request has been made
    get = function(req_sig) {
      assert_is(req_sig, "RequestSignature")
      self$hash[[req_sig$to_s()]]$count %||% 0
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
#' z1 <- RequestSignature$new("get", "http://scottchamberlain.info")
#' z2 <- RequestSignature$new("post", "https://httpbin.org/post")
#' x$register_request(request = z1)
#' x$register_request(request = z1)
#' x$register_request(request = z2)
#' # print method to list requests
#' x
#'
#' # more complex requests
#' w <- RequestSignature$new(
#'   method = "get",
#'   uri = "https:/httpbin.org/get",
#'   options = list(headers = list(`User-Agent` = "foobar", stuff = "things"))
#' )
#' w$to_s()
#' x$register_request(request = w)
#' x
#'
#'
#' # hashes, and number of times each requested
#' x$request_signatures$hash
#'
#' # times_executed method
#' pat <- RequestPattern$new(
#'   method = "get",
#'   uri = "https:/httpbin.org/get",
#'   headers = list(`User-Agent` = "foobar", stuff = "things")
#' )
#' pat$to_s()
#' x$times_executed(pat)
#' z <- RequestPattern$new(method = "get", uri = "http://scottchamberlain.info")
#' x$times_executed(z)
#' w <- RequestPattern$new(method = "post", uri = "https://httpbin.org/post")
#' x$times_executed(w)
#'
#' ## pattern with no matches - returns 0 (zero)
#' pat <- RequestPattern$new(
#'   method = "get",
#'   uri = "http://recology.info/"
#' )
#' pat$to_s()
#' x$times_executed(pat)
#'
#' # reset the request registry
#' x$reset()
RequestRegistry <- R6::R6Class(
  "RequestRegistry",
  public = list(
    #' @field request_signatures a HashCounter object
    request_signatures = HashCounter$new(),

    #' @description print method for the `RequestRegistry` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat_line("<webmockr request registry> ")
      cat_line(" Registered Requests")
      for (i in seq_along(self$request_signatures$hash)) {
        cat_line(
          sprintf(
            "  %s was made %s times\n",
            names(self$request_signatures$hash)[i],
            self$request_signatures$hash[[i]]$count
          )
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
    #' a `RequestSignature$new(...)$to_s()`
    #' @return nothing returned; registers the request
    register_request = function(request) {
      self$request_signatures$put(request)
    },

    #' @description How many times has a request been made
    #' @param request_pattern an object of class `RequestPattern`
    #' @return integer, the number of times the request has been made
    #' @details if no match is found for the request pattern, 0 is returned
    times_executed = function(request_pattern) {
      bools <- c()
      for (i in seq_along(self$request_signatures$hash)) {
        bools[i] <- request_pattern$matches(
          self$request_signatures$hash[[i]]$sig
        )
      }
      if (all(!bools)) {
        return(0)
      }
      self$request_signatures$hash[bools][[1]]$count
    }
  )
)
