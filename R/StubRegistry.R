#' Stub registry
#'
#' @export
#' @keywords internal
#' @param stub an object of class \code{StubbedRequest}
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{register_stub(stub)}}{
#'       Register a stub
#'     }
#'     \item{\code{find_stubbed_request()}}{
#'       Find a stubbed request
#'     }
#'   }
#' @examples \dontrun{
#' # an actual stub
#' stub1 <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' stub1$with(request_headers = list('User-Agent' = 'R'))
#' stub1$to_return(status = 200, body = "foobar", response_headers = list())
#' stub1
#'
#' stub2 <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
#' stub2
#'
#' reg <- StubRegistry$new()
#' reg$register_stub(stub = stub1)
#' reg$register_stub(stub = stub2)
#' reg
#' reg$request_stubs
#' }
StubRegistry <- R6::R6Class(
  'StubRegistry',
  public = list(
    stub = NULL,
    request_stubs = list(),
    global_stubs = list(),

    print = function(x, ...) {
      cat("<webmockr stub registry> ", sep = "\n")
      cat(" Registered Stubs", sep = "\n")
      for (i in seq_along(self$request_stubs)) {
        cat(
          sprintf(
            "  %s: %s %s %s",
            self$request_stubs[[i]]$method,
            url_build(
              self$request_stubs[[i]]$uri,
              self$request_stubs[[i]]$query
            ),
            make_body(self$request_stubs[[i]]$body),
            make_headers(self$request_stubs[[i]]$request_headers)
          ),
          sep = "\n"
        )
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
        tmp <- RequestPattern$new(method = z$method, uri = z$uri)
        tmp$matches(request_signature)
      }, logical(1))
    },

    remove_request_stub = function(stub) {
      xx <- vapply(self$request_stubs, function(x) to_s, "")
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

#' @rdname StubRegistry
#' @export
stub_registry <- function() webmockr_stub_registry

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
