#' @title StubRegistry
#' @description stub registry to keep track of [StubbedRequest] stubs
#' @export
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
    #' @field request_stubs (list) list of request stubs
    request_stubs = list(),
    #' @field global_stubs (list) list of global stubs
    global_stubs = list(),

    #' @description print method for the `StubRegistry` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat("<webmockr stub registry> ", sep = "\n")
      cat(" Registered Stubs", sep = "\n")
      for (i in seq_along(self$request_stubs)) {
        cat("  ", self$request_stubs[[i]]$to_s(), "\n")
      }
      invisible(self$request_stubs)
    },

    #' @description Register a stub
    #' @param stub an object of type [StubbedRequest]
    #' @return nothing returned; registers the stub
    register_stub = function(stub) {
      self$request_stubs <- Filter(length, c(self$request_stubs, stub))
    },

    #' @description Find a stubbed request
    #' @param req an object of class [RequestSignature]
    #' @return an object of type [StubbedRequest], if matched
    find_stubbed_request = function(req) {
      stubs <- c(self$global_stubs, self$request_stubs)
      stubs[self$request_stub_for(req)]
    },

    # response_for_request = function(request_signature) {
    #   stub <- self$request_stub_for(request_signature)
    #   evaluate_response_for_request(stub$response, request_signature) %||% NULL
    # },

    #' @description Find a stubbed request
    #' @param request_signature an object of class [RequestSignature]
    #' @return logical, 1 or more
    request_stub_for = function(request_signature) {
      stubs <- c(self$global_stubs, self$request_stubs)
      vapply(stubs, function(z) {
        tmp <- RequestPattern$new(method = z$method, uri = z$uri,
                                  uri_regex = z$uri_regex, query = z$query,
                                  body = z$body, headers = z$request_headers)
        tmp$matches(request_signature)
      }, logical(1))
    },

    #' @description Remove a stubbed request by matching request signature
    #' @param stub an object of type [StubbedRequest]
    #' @return nothing returned; removes the stub from the registry
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

    #' @description Remove all request stubs
    #' @return nothing returned; removes all request stubs
    remove_all_request_stubs = function() {
      self$request_stubs <- list()
    },

    #' @description Find a stubbed request
    #' @param x an object of class [RequestSignature]
    #' @return nothing returned; registers the stub
    is_registered = function(x) any(self$request_stub_for(x))
  )
)

# initialize empty stub registry on package load
webmockr_stub_registry <- new.env()
webmockr_stub_registry <- StubRegistry$new()

json_validate <- function(x) {
  res <- tryCatch(jsonlite::validate(x), error = function(e) e)
  if (inherits(res, "error")) return(FALSE)
  res
}

# make body info for print method
make_body <- function(x) {
  if (is.null(x)) return("")
  if (inherits(x, "mock_file")) x <- x$payload
  if (inherits(x, "form_file")) x <- unclass(x)
  clzzes <- vapply(x, function(z) inherits(z, "form_file"), logical(1))
  if (any(clzzes)) for(i in seq_along(x)) x[[i]] <- unclass(x[[i]])
  if (json_validate(x))
    body <- x
  else
    body <- jsonlite::toJSON(x, auto_unbox = TRUE)
  paste0(" with body ", body)
}

# make headers info for print method
make_headers <- function(x) {
  if (is.null(x)) return("")
  paste0(" with headers ", jsonlite::toJSON(x, auto_unbox = TRUE))
}

# make body info for print method
make_status <- function(x) {
  if (is.null(x)) return("")
  paste0(" with status ", as.character(x))
}
