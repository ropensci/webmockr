#' curl library adapter
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
#'     \item{`build_curl_request(x)`}{
#'       Build a curl [RequestSignature]
#'       x: curl request parts (list)
#'     }
#'     \item{`build_curl_response(req, resp)`}{
#'       Build a curl response
#'       req: a curl request (list)
#'       resp: a curl response ()
#'     }
#'     \item{`handle_request()`}{
#'       All logic for handling a request
#'       req: a curl request (list)
#'     }
#'     \item{`remove_curl_stubs()`}{
#'       Remove all curl stubs
#'     }
#'   }
#'
#' This adapter modifies \pkg{curl} to allow mocking HTTP requests
#'
#' @format NULL
#' @usage NULL
CurlAdapter <- R6::R6Class(
  'CurlAdapter',
  public = list(
    name = "curl_adapter",

    enable = function() {
      message("CurlAdapter enabled!")
      webmockr_lightswitch$curl <- TRUE
      crul::mock(TRUE)
      invisible(TRUE)
    },

    disable = function() {
      message("CurlAdapter disabled!")
      webmockr_lightswitch$curl <- FALSE
      crul::mock(FALSE)
      self$remove_curl_stubs()
      invisible(FALSE)
    },

    handle_request = function(url, handle) {
      # put request in request registry
      request_signature <- build_curl_request(url)
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

        curl_resp <- build_curl_response(req, resp)

        # add to_return() elements if given
        if (length(cc(ss$responses_sequences)) != 0) {
          # remove NULLs
          toadd <- cc(ss$responses_sequences)
          # modify responses
          for (i in seq_along(toadd)) {
            if (names(toadd)[i] == "status") {
              curl_resp$status_code <- toadd[[i]]
            }
            if (names(toadd)[i] == "body") {
              # curl_resp$content <- toadd[[i]]
              curl_resp$content <- ss$responses_sequences$body_raw
            }
            if (names(toadd)[i] == "headers") {
              curl_resp$response_headers <- toadd[[i]]
            }
          }
        }
      } else if (webmockr_net_connect_allowed(uri = req$url$url)) {
        # if real requests || localhost || certain exceptions ARE
        #   allowed && nothing found above
        tmp <- crul::HttpClient$new(url = req$url$url)
        tmp2 <- webmockr_crul_fetch(req)
        curl_resp <- build_curl_response(req, tmp2)
      } else {
        # no stubs found and net connect not allowed - STOP
        x <- "Real HTTP connections are disabled.\nUnregistered request:\n "
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
        ending <- "\n============================================================"
        stop(paste0(msgx, msgy, msgz, ending), call. = FALSE)
      }

      return(curl_resp)
    },

    remove_curl_stubs = function() {
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
      if (!is.null(x$headers) || !is.null(x$body)) {
        # set defaults to ""
        hd_str <- bd_str <- ""

        # headers has to be a named list, so easier to deal with
        if (!is.null(x$headers)) {
          hd <- x$headers
          hd_str <- paste0(
            paste(sprintf("'%s'", names(hd)),
                  sprintf("'%s'", unlist(unname(hd))), sep = " = "),
            collapse = ", ")
        }

        # body can be lots of things, so need to handle various cases
        if (!is.null(x$body)) {
          bd <- x$body
          bd_str <- hdl_lst2(bd)
        }

        if (nzchar(hd_str) && nzchar(bd_str)) {
          with_str <- sprintf(" wi_th(\n       headers = list(%s),\n       body = list(%s)\n     )",
                              hd_str, bd_str)
        } else if (nzchar(hd_str) && !nzchar(bd_str)) {
          with_str <- sprintf(" wi_th(\n       headers = list(%s)\n     )", hd_str)
        } else if (!nzchar(hd_str) && nzchar(bd_str)) {
          with_str <- sprintf(" wi_th(\n       body = list(%s)\n     )", bd_str)
        }

        tmp <- paste0(tmp, " %>%\n    ", with_str)
      }
      return(tmp)
    }
  )
)

#' Build a curl response
#' @export
#' @param req a request
#' @param resp a response
#' @return a curl response
build_curl_response <- function(req, resp) {
  crul::HttpResponse$new(
    method = req$method,
    url = req$url$url,
    status_code = resp$status_code,
    request_headers = c('User-Agent' = req$options$useragent, req$headers),
    response_headers = {
      if (grepl("^ftp://", resp$url)) {
        list()
      } else {
        hds <- resp$headers

        if (is.null(hds)) {
          hds <- resp$response_headers

          if (is.null(hds)) {
            list()
          } else {
            stopifnot(is.list(hds))
            stopifnot(is.character(hds[[1]]))
            hds
          }
        } else {
          hh <- rawToChar(hds %||% raw(0))
          if (is.null(hh) || nchar(hh) == 0) {
            list()
          } else {
            crul_headers_parse(curl::parse_headers(hh))
          }
        }
      }
    },
    modified = resp$modified %||% NA,
    times = resp$times,
    content = resp$content,
    handle = req$url$handle,
    request = req
  )
}

#' Build a curl request
#' @export
#' @param x an unexecuted curl request object
#' @return a curl request
build_curl_request = function(x) {
  x <- urltools::url_parse(x)
  RequestSignature$new(
    method = "get",
    uri = urltools::url_compose(x),
    options = list(
      body = x$fields %||% NULL,
      headers = x$headers %||% NULL,
      proxies = x$proxies %||% NULL,
      auth = x$auth %||% NULL
    )
  )
}

