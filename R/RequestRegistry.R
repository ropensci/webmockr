#' @title HashCounter
#' @description hash with counter, to store requests, and count each time
#' it is used
#' @keywords internal
#' @family request-registry
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
#' @keywords internal
#' @family request-registry
#' @seealso [stub_registry()] and [StubRegistry]
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
    #' @return nothing returned; resets registry to no requests
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
      if (missing(request_pattern)) {
        cli_abort("{.arg request_pattern} is required")
      }
      if (!inherits(request_pattern, "RequestPattern")) {
        cli_abort("{.arg request_pattern} must be of class 'RequestPattern'")
      }
      if (is_empty(self$request_signatures$hash)) {
        return(0)
      }
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
