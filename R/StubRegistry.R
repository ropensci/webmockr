#' @title StubRegistry
#' @description stub registry to keep track of [StubbedRequest] stubs
#' @keywords internal
#' @family stub-registry
StubRegistry <- R6::R6Class(
  "StubRegistry",
  public = list(
    #' @field request_stubs (list) list of request stubs
    request_stubs = list(),

    #' @description print method for the `StubRegistry` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat_line("<webmockr stub registry> ")
      cat_line(" Registered Stubs")
      for (i in seq_along(self$request_stubs)) {
        cat_line("  ", self$request_stubs[[i]]$to_s())
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
      self$request_stubs[self$request_stub_for(req)]
    },

    #' @description Find a stubbed request
    #' @param request_signature an object of class [RequestSignature]
    #' @param count (bool) iterate counter or not. default: `TRUE`
    #' @return logical, 1 or more
    request_stub_for = function(request_signature, count = TRUE) {
      stubs <- self$request_stubs
      mtchs <- vapply(
        stubs,
        function(z) {
          tmp <- RequestPattern$new(
            method = z$method,
            uri = z$uri,
            uri_regex = z$uri_regex,
            query = z$query,
            body = z$body,
            headers = z$request_headers,
            basic_auth = z$basic_auth
          )
          tmp$matches(request_signature)
        },
        logical(1)
      )
      if (count) {
        for (i in seq_along(stubs)) {
          if (mtchs[i]) stubs[[i]]$counter$put(request_signature)
        }
      }
      return(mtchs)
    },

    #' @description Remove a stubbed request by matching request signature
    #' @param stub an object of type [StubbedRequest]
    #' @return nothing returned; removes the stub from the registry
    remove_request_stub = function(stub) {
      xx <- vapply(self$request_stubs, function(x) x$to_s(), "")
      if (stub$to_s() %in% xx) {
        self$request_stubs <- self$request_stubs[-which(stub$to_s() %in% xx)]
      } else {
        abort(c(
          "This request stub is not registered:",
          stub$to_s()
        ))
      }
    },

    #' @description Remove all request stubs
    #' @return nothing returned; removes all request stubs
    remove_all_request_stubs = function() {
      for (stub in self$request_stubs) {
        if (inherits(stub, "StubbedRequest")) stub$reset()
      }
      self$request_stubs <- list()
    },

    #' @description Find a stubbed request from a request signature
    #' @param x an object of class [RequestSignature]
    #' @return nothing returned; registers the stub
    is_registered = function(x) any(self$request_stub_for(x, count = FALSE)),

    #' @description Check if a stubbed request is in the stub registry
    #' @param stub an object of class [StubbedRequest]
    #' @return single boolean, `TRUE` or `FALSE`
    is_stubbed = function(stub) {
      if (!length(self$request_stubs)) {
        return(FALSE)
      }
      any(stub$to_s() %in% vapply(self$request_stubs, \(x) x$to_s(), ""))
    }
  )
)

#' @importFrom jsonlite validate
json_validate <- function(x) {
  res <- tryCatch(jsonlite::validate(x), error = function(e) e)
  if (inherits(res, "error")) {
    return(FALSE)
  }
  res
}

# make body info for print method
make_body <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (inherits(x, "mock_file")) x <- x$payload
  if (inherits(x, c("form_file", "partial"))) x <- unclass(x)
  clzzes <- vapply(x, function(z) inherits(z, "form_file"), logical(1))
  if (any(clzzes)) for (i in seq_along(x)) x[[i]] <- unclass(x[[i]])
  if (json_validate(x)) {
    body <- x
  } else {
    body <- jsonlite::toJSON(x, auto_unbox = TRUE)
  }
  paste0(" with body ", body)
}

# make query info for print
make_query <- function(x) {
  if (is.null(x)) {
    return("")
  }
  txt <- paste(
    names(x),
    subs(unname(unlist(x)), 20),
    sep = "=",
    collapse = ", "
  )
  if (attr(x, "partial_match") %||% FALSE) {
    txt <- sprintf(
      "%s(%s)",
      switch(
        attr(x, "partial_type"),
        include = "including",
        exclude = "excluding"
      ),
      txt
    )
  }
  paste0(" with query params ", txt)
}

#' make headers info for print method
#' @importFrom jsonlite toJSON
#' @param x a named list
#' @noRd
make_headers <- function(x) {
  if (is.null(x)) {
    return("")
  }
  paste0(" with headers ", jsonlite::toJSON(x, auto_unbox = TRUE))
}

# make body info for print method
make_status <- function(x) {
  if (is.null(x)) {
    return("")
  }
  paste0(" with status ", as.character(x))
}
