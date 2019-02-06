#' Stub registry
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`register_stub(stub)`}{
#'       Register a stub
#'       - stub: an object of class [StubbedRequest]
#'     }
#'     \item{`find_stubbed_request(req)`}{
#'       Find a stubbed request
#'       - req: an object of class [RequestSignature]
#'     }
#'     \item{`response_for_request(request_signature)`}{
#'       Find a stubbed request
#'       - request_signature: an object of class [RequestSignature]
#'     }
#'     \item{`request_stub_for(request_signature)`}{
#'       Find a stubbed request
#'       - request_signature: an object of class [RequestSignature]
#'     }
#'     \item{`remove_request_stub(stub)`}{
#'       Remove a stubbed request by matching request signature
#'       - stub: an object of class [StubbedRequest]
#'     }
#'     \item{`remove_all_request_stubs()`}{
#'       Remove all request stubs
#'     }
#'     \item{`is_registered(x)`}{
#'       Find a stubbed request
#'       - x: an object of class [RequestSignature]
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @family stub-registry
#' @examples \dontrun{
#' # Make a stub
#' stub1 <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' stub1$with(headers = list('User-Agent' = 'R'))
#' stub1$to_return(status = 200, body = "foobar", headers = list())
#' stub1
#'
#' # Make another stub
#' stub2 <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' stub2
#'
#' # Put both stubs in the stub registry
#' reg <- StubRegistry$new()
#' reg$register_stub(stub = stub1)
#' reg$register_stub(stub = stub2)
#' reg
#' reg$request_stubs
#' }
StubRegistry <- R6::R6Class(
  "StubRegistry",
  public = list(
    stub = NULL,
    request_stubs = list(),
    global_stubs = list(),

    print = function(x, ...) {
      cat("<webmockr stub registry> ", sep = "\n")
      cat(" Registered Stubs", sep = "\n")
      for (i in seq_along(self$request_stubs)) {
        cat("  ", self$request_stubs[[i]]$to_s(), "\n")
      }
      invisible(self$request_stubs)
    },

    register_stub = function(stub) {
      self$request_stubs <- Filter(length, c(self$request_stubs, stub))
    },

    find_stubbed_request = function(req) {
      stubs <- c(self$global_stubs, self$request_stubs)
      stubs[self$request_stub_for(req)]
    },

    response_for_request = function(request_signature) {
      stub <- self$request_stub_for(request_signature)
      evaluate_response_for_request(stub$response, request_signature) %||% NULL
    },

    request_stub_for = function(request_signature) {
      stubs <- c(self$global_stubs, self$request_stubs)
      vapply(stubs, function(z) {
        tmp <- RequestPattern$new(method = z$method, uri = z$uri,
                                  uri_regex = z$uri_regex, query = z$query,
                                  body = z$body, headers = z$request_headers)
        tmp$matches(request_signature)
      }, logical(1))
    },

    remove_request_stub = function(stub) {
      xx <- vapply(self$request_stubs, function(x) x$to_s(), "")
      if (stub$to_s() %in% xx) {
        self$request_stubs <- self$request_stubs[-which(stub$to_s() %in% xx)]
      } else {
        stop(
          "Request stub \n\n  ",
          stub$to_s(),
          "\n\n is not registered.",
          call. = FALSE
        )
      }
    },

    remove_all_request_stubs = function() {
      self$request_stubs <- list()
    },

    is_registered = function(x) any(self$request_stub_for(x))
  )
)

# initialize empty stub registry on package load
webmockr_stub_registry <- new.env()
webmockr_stub_registry <- StubRegistry$new()

# madke body info for print method
make_body <- function(x) {
  if (is.null(x)) return("")
  paste0(" with body ", jsonlite::toJSON(x, auto_unbox = TRUE))
}

# madke headers info for print method
make_headers <- function(x) {
  if (is.null(x)) return("")
  paste0(" with headers ", jsonlite::toJSON(x, auto_unbox = TRUE))
}

# madke body info for print method
make_status <- function(x) {
  if (is.null(x)) return("")
  paste0(" with status ", as.character(x))
}
