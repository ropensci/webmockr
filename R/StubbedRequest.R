#' @title StubCounter
#' @description hash with counter to store requests and count number
#' of requests made against the stub
#' @keywords internal
StubCounter <- R6::R6Class(
  "StubCounter",
  public = list(
    #' @field hash (list) a list for internal use only, with elements
    #' `key`, `sig`, and `count`
    hash = list(),

    #' @description Register a request by it's key
    #' @param x an object of class `RequestSignature`
    #' @return nothing returned; registers request & iterates internal counter
    put = function(x) {
      assert_is(x, "RequestSignature")
      key <- x$to_s()
      self$hash[[key]] <- list(key = key, sig = x)
      private$total <- private$total + 1
    },

    #' @description Get the count of number of times any matching request has
    #' been made against this stub
    count = function() {
      private$total
    }
  ),
  private = list(
    total = 0
  )
)

#' @title StubbedRequest
#' @description stubbed request class underlying [stub_request()]
#' @keywords internal
#' @seealso [stub_request()]
StubbedRequest <- R6::R6Class(
  "StubbedRequest",
  public = list(
    #' @field method (xx) xx
    method = NULL,
    #' @field uri (xx) xx
    uri = NULL,
    #' @field uri_regex (xx) xx
    uri_regex = NULL,
    #' @field regex a logical
    regex = FALSE,
    #' @field uri_parts (xx) xx
    uri_parts = NULL,
    #' @field host (xx) xx
    host = NULL,
    #' @field query (xx) xx
    query = NULL,
    #' @field body (xx) xx
    body = NULL,
    #' @field basic_auth (xx) xx
    basic_auth = NULL,
    #' @field request_headers (xx) xx
    request_headers = NULL,
    #' @field response_headers (xx) xx
    response_headers = NULL,
    #' @field responses_sequences (xx) xx
    responses_sequences = NULL,
    #' @field status_code (xx) xx
    status_code = NULL,
    #' @field counter a StubCounter object
    counter = NULL,

    #' @description Create a new `StubbedRequest` object
    #' @param method the HTTP method (any, head, get, post, put,
    #' patch, or delete). "any" matches any HTTP method. required.
    #' @param uri (character) request URI. either this or `uri_regex`
    #' required. \pkg{webmockr} can match uri's without the "http" scheme,
    #' but does not match if the scheme is "https". required, unless
    #' `uri_regex` given. See [UriPattern] for more.
    #' @param uri_regex (character) request URI as regex. either this or `uri`
    #' required
    #' @return A new `StubbedRequest` object
    initialize = function(method, uri = NULL, uri_regex = NULL) {
      if (!missing(method)) {
        verb <- match.arg(tolower(method), http_verbs)
        self$method <- verb
      }
      if (is.null(uri) && is.null(uri_regex)) {
        abort("one of uri or uri_regex is required")
      }
      self$uri <- uri
      self$uri_regex <- uri_regex
      if (!is.null(uri_regex)) {
        self$regex <- TRUE
      }
      if (!is.null(uri)) {
        self$uri_parts <- parseurl(self$uri)
      }
      self$counter <- StubCounter$new()
    },

    #' @description print method for the `StubbedRequest` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat_line("<webmockr stub> ")
      cat_line(paste0("  method: ", self$method))
      cat_line(paste0("  uri: ", self$uri %||% self$uri_regex))
      cat_line("  with: ")
      cat_line(paste0("    query: ", hdl_lst(self$query)))
      if (is.null(self$body)) {
        cat_line("    body: ")
      } else {
        cat_line(sprintf(
          "    body (class: %s): %s",
          class(self$body)[1L],
          hdl_lst(self$body)
        ))
      }
      cat_line(
        paste0(
          "    request_headers: ",
          hdl_lst(self$request_headers)
        )
      )
      cat_line(
        paste0(
          "    auth: ",
          prep_cat_auth(self$basic_auth)
        )
      )
      cat_line("  to_return: ")
      rs <- self$responses_sequences
      for (i in seq_along(rs)) {
        cat_line(paste0("  - status: ", hdl_lst(rs[[i]]$status)))
        cat_line(paste0("    body: ", hdl_lst(rs[[i]]$body)))
        cat_line(
          paste0(
            "    response_headers: ",
            hdl_lst(rs[[i]]$headers)
          )
        )
        cat_line(paste0("    should_timeout: ", rs[[i]]$timeout))
        cat_line(paste0(
          "    should_raise: ",
          if (rs[[i]]$raise) {
            paste0(
              vapply(rs[[i]]$exceptions, "[[", "", "classname"),
              collapse = ", "
            )
          } else {
            "FALSE"
          }
        ))
      }
    },

    #' @description Set expectations for what's given in HTTP request
    #' @param query (list) request query params, as a named list. optional
    #' @param body (list) request body, as a named list. optional
    #' @param headers (list) request headers as a named list. optional.
    #' @param basic_auth (character) basic authentication. optional.
    #' @return nothing returned; sets only
    with = function(
      query = NULL,
      body = NULL,
      headers = NULL,
      basic_auth = NULL
    ) {
      if (!is.null(query)) {
        query[] <- lapply(query, as.character)
      }
      self$query <- query
      self$body <- body
      self$basic_auth <- basic_auth
      self$request_headers <- headers
    },

    #' @description Set expectations for what's returned in HTTP response
    #' @param status (numeric) an HTTP status code
    #' @param body (list) response body, one of: `character`, `json`,
    #' `list`, `raw`, `numeric`, `NULL`, `FALSE`, or a file connection
    #' (other connection types not supported)
    #' @param headers (list) named list, response headers. optional.
    #' @return nothing returned; sets whats to be returned
    to_return = function(status, body, headers) {
      body <- if (inherits(body, "connection")) {
        bod_sum <- summary(body)
        close.connection(body)
        if (bod_sum$class != "file") {
          abort("'to_return' only supports connections of type 'file'")
        }
        structure(bod_sum$description, type = "file")
      } else {
        body
      }
      # FIXME: for then change, remove eventually
      self$response_headers <- headers
      body_raw <- {
        if (inherits(body, "mock_file")) {
          body
        } else if (inherits(body, "logical")) {
          if (!body) {
            raw()
          } else {
            webmockr_stub_registry$remove_request_stub(self)
            abort(c(
              "Unknown `body` type",
              "*" = "must be NULL, FALSE, character, raw or list; stub removed"
            ))
          }
        } else if (inherits(body, "raw")) {
          body
        } else if (is.null(body)) {
          raw()
        } else if (is.character(body) || inherits(body, "json")) {
          if (!is.null(attr(body, "type"))) {
            stopifnot(attr(body, "type") == "file")
            body
          } else {
            charToRaw(body)
          }
        } else if (!is.list(body)) {
          webmockr_stub_registry$remove_request_stub(self)
          abort(c(
            "Unknown `body` type",
            "*" = paste(
              "must be: numeric, NULL, FALSE, character,",
              "json, raw, list, or file connection"
            ),
            "*" = "stub removed"
          ))
        } else {
          charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
        }
      }
      private$append_response(
        private$response(
          status = status,
          body = body,
          headers = headers,
          body_raw = body_raw
        )
      )
    },

    #' @description Response should time out
    #' @return nothing returned
    to_timeout = function() {
      private$append_response(private$response(timeout = TRUE))
    },

    #' @description Response should raise an exception `x`
    #' @param x (character) an exception message
    #' @return nothing returned
    to_raise = function(x) {
      private$append_response(
        private$response(
          raise = TRUE,
          exceptions = if (inherits(x, "list")) x else list(x)
        )
      )
    },

    #' @description Response as a character string
    #' @return (character) the response as a string
    to_s = function() {
      ret <- self$responses_sequences
      gsub(
        "^\\s+|\\s+$",
        "",
        sprintf(
          "  %s: %s %s %s %s %s",
          toupper(self$method),
          url_builder(self$uri %||% self$uri_regex, self$regex),
          make_query(self$query),
          make_body(self$body),
          make_headers(self$request_headers),
          if (length(ret) > 0) {
            strgs <- c()
            for (i in seq_along(ret)) {
              bd <- make_body(ret[[i]]$body)
              stt <- make_status(ret[[i]]$status)
              hed <- make_headers(ret[[i]]$headers)
              strgs[i] <- sprintf(
                "%s %s %s",
                if (nzchar(paste0(bd, stt, hed))) {
                  paste("| to_return: ", bd, stt, hed)
                } else {
                  ""
                },
                if (ret[[i]]$timeout) "| should_timeout: TRUE" else "",
                if (ret[[i]]$raise) {
                  paste0(
                    "| to_raise: ",
                    paste0(
                      vapply(ret[[i]]$exceptions, "[[", "", "classname"),
                      collapse = ", "
                    )
                  )
                } else {
                  ""
                }
              )
            }
            paste0(strgs, collapse = " ")
          } else {
            ""
          }
        )
      )
    },

    #' @description Reset the counter for the stub
    #' @return nothing returned; resets stub counter to no requests
    reset = function() {
      self$counter <- StubCounter$new()
    }
  ),
  private = list(
    append_response = function(x) {
      self$responses_sequences <- cc(c(self$responses_sequences, list(x)))
    },
    response = function(
      status = NULL,
      body = NULL,
      headers = NULL,
      body_raw = NULL,
      timeout = FALSE,
      raise = FALSE,
      exceptions = list()
    ) {
      list(
        status = status,
        body = body,
        headers = headers,
        body_raw = body_raw,
        timeout = timeout,
        raise = raise,
        exceptions = exceptions
      )
    }
  )
)

#' @importFrom jsonlite base64_enc
basic_auth_header <- function(x) {
  assert_is(x, "character")
  stopifnot(length(x) == 1)
  encoded <- jsonlite::base64_enc(x)
  paste0("Basic ", encoded)
}

prep_auth <- function(x) {
  if (!is_null(x)) {
    list(Authorization = basic_auth_header(x))
  }
}

prep_cat_auth <- function(x) {
  if (!is_null(x %||% NULL)) {
    basic_auth_header(paste0(x, collapse = ":"))
  }
}
