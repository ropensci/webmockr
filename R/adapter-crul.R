#' crul library adapter
#'
#' @export
#' @family http_lib_adapters
#' @details
#' **Methods**
#'   \describe{
#'     \item{`enable()`}{
#'       Enable the adapter
#'     }
#'     \item{`disable()`}{
#'       Disable the adapter
#'     }
#'     \item{`build_crul_request(x)`}{
#'       Build a crul [RequestSignature]
#'       x: crul request parts (list)
#'     }
#'     \item{`build_crul_response(req, resp)`}{
#'       Build a crul response
#'       req: a crul request (list)
#'       resp: a crul response ()
#'     }
#'     \item{`handle_request()`}{
#'       All logic for handling a request
#'       req: a crul request (list)
#'     }
#'     \item{`remove_crul_stubs()`}{
#'       Remove all crul stubs
#'     }
#'   }
#'
#' This adapter modifies \pkg{crul} to allow mocking HTTP requests
#'
#' @format NULL
#' @usage NULL
CrulAdapter <- R6::R6Class(
  'CrulAdapter',
  public = list(
    name = "crul_adapter",

    enable = function() {
      message("CrulAdapter enabled!")
      webmockr_lightswitch$crul <- TRUE
    },

    disable = function() {
      message("CrulAdapter disabled!")
      webmockr_lightswitch$crul <- FALSE
      self$remove_crul_stubs()
    },

    build_crul_request = function(x) {
      RequestSignature$new(
        method = x$method,
        uri = x$url$url,
        options = list(
          body = x$body %||% NULL,
          headers = x$headers %||% NULL,
          proxies = x$proxies %||% NULL,
          auth = x$auth %||% NULL
        )
      )
    },

    build_crul_response = function(req, resp) {
      crul::HttpResponse$new(
        method = req$method,
        url = req$url$url,
        status_code = resp$status_code,
        request_headers = c(useragent = req$options$useragent, req$headers),
        #response_headers = list(),
        response_headers = {
          if (grepl("^ftp://", resp$url)) {
            list()
          } else {
            hh <- rawToChar(resp$response_headers %||% raw(0))
            if (is.null(hh) || nchar(hh) == 0) {
              list()
            } else {
              crul_headers_parse(curl::parse_headers(hh))
            }
          }
        },
        modified = resp$modified,
        times = resp$times,
        content = resp$content,
        handle = req$url$handle,
        request = req
      )
    },

    handle_request = function(req) {
      # put request in request registry
      request_signature <- self$build_crul_request(req)
      webmockr_request_registry$register_request(
        request = request_signature$to_s()
      )

      if (request_is_in_cache(request_signature)) {
        # if real requests NOT allowed
        # even if net connects allowed, we check if stubbed found first

        # if user wants to return a partial object
        #   get stub with response and return that
        ss <-
          webmockr_stub_registry$find_stubbed_request(request_signature)[[1]]

        resp <- Response$new()
        resp$set_url(ss$uri)
        resp$set_body(ss$body)
        resp$set_request_headers(ss$request_headers)
        resp$set_response_headers(ss$response_headers)
        # generate crul response
        crul_resp <- self$build_crul_response(req, resp)

        # add to_return() elements if given
        if (length(cc(ss$responses_sequences)) != 0) {
          # remove NULLs
          toadd <- cc(ss$responses_sequences)
          # modify responses
          for (i in seq_along(toadd)) {
            if (names(toadd)[i] == "status")
              crul_resp$status_code <- toadd[[i]]
            if (names(toadd)[i] == "body")
              crul_resp$content <- toadd[[i]]
            if (names(toadd)[i] == "headers")
              crul_resp$response_headers <- toadd[[i]]
          }
        }
      } else if (webmockr_net_connect_allowed()) {
        # if real requests ARE allowed && nothing found above
        tmp <- crul::HttpClient$new(url = req$url$url)
        tmp2 <- webmockr_crul_fetch(req)
        crul_resp <- self$build_crul_response(req, tmp2)
      } else {
        # no stubs found and net connect not allowed
        x <- "Real HTTP connections are disabled.\nUnregistered request:"
        y <- "\n\nYou can stub this request with the following snippet:\n\n  "
        z <- "\n\nregistered request stubs:\n\n"
        msgx <- paste(x, request_signature$to_s())
        msgy <- paste(
          y,
          #make_stub_request_code(request_signature)
          private$make_stub_request_code(request_signature)
        )
        if (length(webmockr_stub_registry$request_stubs)) {
          msgz <- paste(
            z,
            paste0(vapply(webmockr_stub_registry$request_stubs, function(z)
              z$to_s(), ""), collapse = "\n ")
          )
        } else {
          msgz <- ""
        }
        stop(paste0(msgx, msgy, msgz), call. = FALSE)
      }

      return(crul_resp)
    },

    remove_crul_stubs = function() {
      webmockr_stub_registry$remove_all_request_stubs()
    }
  ),

  private = list(
    make_stub_request_code = function(x) {
      sprintf(
        "stub_request('%s', url = '%s')",
        x$method,
        x$uri
      )
    }
  )
)
