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
      crul::mock(TRUE)
      invisible(TRUE)
    },

    disable = function() {
      message("CrulAdapter disabled!")
      webmockr_lightswitch$crul <- FALSE
      crul::mock(FALSE)
      self$remove_crul_stubs()
      invisible(FALSE)
    },

    handle_request = function(req) {
      # put request in request registry
      request_signature <- build_crul_request(req)
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
        resp$set_status(ss$status_code %||% 200)

        # if user set to_timeout or to_raise, do that
        if (ss$timeout || ss$raise) {
          if (ss$timeout) {
            x <- fauxpas::HTTPRequestTimeout$new()
            resp$set_status(x$status_code)
            x$do_verbose(resp)
          }
          if (ss$raise) {
            x <- ss$exceptions[[1]]$new()
            resp$set_status(x$status_code)
            x$do_verbose(resp)
          }
        }

        # generate crul response
        crul_resp <- build_crul_response(req, resp)

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
              #crul_resp$set_body(toadd[[i]])
            if (names(toadd)[i] == "headers")
              crul_resp$response_headers <- toadd[[i]]
          }
        }

        # if vcr loaded: record http interaction into vcr namespace
        if ("package:vcr" %in% search()) {
          # get current cassette
          cas <- vcr::cassette_current()
          # record http interaction into vcr http interaction list
          cas$record_http_interaction(crul_resp)
          # build crul_resp from vcr http interaction on disk (i.e., on casette)
          crul_resp <- cas$serialize_to_crul()
        }
        # else: since vcr is not loaded - skip

      } else if (webmockr_net_connect_allowed()) {
        # if real requests ARE allowed && nothing found above
        tmp <- crul::HttpClient$new(url = req$url$url)
        tmp2 <- webmockr_crul_fetch(req)
        crul_resp <- build_crul_response(req, tmp2)

        # if vcr loaded: record http interaction into vcr namespace
        if ("package:vcr" %in% search()) {
          # get current cassette
          cas <- vcr::cassette_current()
          # record http interaction into vcr http interaction list
          cas$record_http_interaction(crul_resp)
        }

      } else {
        # no stubs found and net connect not allowed - STOP basically
        x <- "Real HTTP connections are disabled.\nUnregistered request:"
        y <- "\n\nYou can stub this request with the following snippet:\n\n  "
        z <- "\n\nregistered request stubs:\n\n"
        msgx <- paste(x, request_signature$to_s())
        msgy <- paste(y, private$make_stub_request_code(request_signature))
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
      tmp <- sprintf(
        "stub_request('%s', uri = '%s')",
        x$method,
        x$uri
      )
      if (!is.null(x$headers)) {
        hd <- x$headers
        hd_str <- sprintf(
          "wi_th(headers = list(%s))",
          paste0(
            paste(sprintf("'%s'", names(hd)),
                  sprintf("'%s'", unlist(unname(hd))), sep = " = "),
            collapse = ", ")
        )
        tmp <- paste0(tmp, " %>%\n    ", hd_str)
      }
      cat(tmp, sep = "\n")
      return(tmp)
    }
  )
)

#' Build a crul response
#' @export
#' @param req a request
#' @param resp a response
#' @return a crul response
build_crul_response <- function(req, resp) {
  crul::HttpResponse$new(
    method = req$method,
    url = req$url$url,
    status_code = resp$status_code,
    request_headers = c('User-Agent' = req$options$useragent, req$headers),
    response_headers = {
      if (grepl("^ftp://", resp$url)) {
        list()
      } else {
        hh <- rawToChar(resp$headers %||% raw(0))
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
}

#' Build a crul request
#' @export
#' @param x an unexecuted crul request object
#' @return a crul request
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
}

