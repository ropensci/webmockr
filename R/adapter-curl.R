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
#'
#' @examples \dontrun{
#' library(curl)
#'
#' # before mocking turned on
#' h <- new_handle()
#' curl_fetch_memory("https://httpbin.org/get?foo=bar", h)
#'
#' # after mocking turned on
#' disable()
#' enable("curl")
#' # curl_fetch_memory("https://httpbin.org/get?foo=bar", h)
#'
#' # after stubbing request
#' stub_request("get", "https://httpbin.org/get?foo=bar")
#' stub_registry()
#'
#' # now you get a mocked response
#' curl_fetch_memory("https://httpbin.org/get?foo=bar", h)
#'
#' # another request, this time setting what to return
#' h2 <- new_handle()
#' # g = curl_fetch_memory("https://httpbin.org/get?bear=brown", h2)
#' stub_request('get', uri = 'https://httpbin.org/get?bear=brown') %>%
#'     to_return(status = 418, body = "stuff", headers = list(a = 5))
#' stub_registry()
#'
#' # make request, inspect returned items
#' g = curl_fetch_memory("https://httpbin.org/get?bear=brown", h2)
#' g
#' rawToChar(g$headers)
#' rawToChar(g$content)
#'
#' # allow net connect
#' webmockr_allow_net_connect()
#' webmockr_net_connect_allowed()
#' h3 <- new_handle()
#' # curl_fetch_memory("https://httpbin.org/get?cow=brown", h3)
#' ## disable again
#' webmockr_disable_net_connect()
#' stub_request("get", "https://httpbin.org/get?cow=brown") %>% 
#'   to_return(headers = list(brown = "cow"))
#' x <- curl_fetch_memory("https://httpbin.org/get?cow=brown", h3)
#' x
#' rawToChar(x$headers)
#' }
CurlAdapter <- R6::R6Class(
  'CurlAdapter',
  public = list(
    name = "curl_adapter",

    enable = function() {
      message("CurlAdapter enabled!")
      webmockr_lightswitch$curl <- TRUE
      curl_mock(TRUE)
      invisible(TRUE)
    },

    disable = function() {
      message("CurlAdapter disabled!")
      webmockr_lightswitch$curl <- FALSE
      curl_mock(FALSE)
      self$remove_curl_stubs()
      invisible(FALSE)
    },

    handle_request = function(req) {
      # put request in request registry
      request_signature <- build_curl_request(req)
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
              curl_resp$headers <- charToRaw(make_curl_headers(toadd[[i]]))
            }
          }
        }

      } else if (webmockr_net_connect_allowed(uri = req$url)) {
        # if real requests || localhost || certain exceptions ARE
        #   allowed && nothing found above
        disable("curl")
        resp <- eval(parse(text = paste0("curl::", strsplit(req$called, '\\\"')[[1]][1])))(req$url, req$handle)
        curl_resp <- build_curl_response(req, resp)
        enable("curl")
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
  headers <- NULL
  if (!is.null(resp$response_headers)) {
    headers <- charToRaw(make_curl_headers(resp$response_headers))
  }
  if (!is.null(resp$headers)) {
    if (inherits(resp$headers, "raw")) headers <- resp$headers
  }
  list(
    url = req$url,
    status_code = resp$status_code,
    type = ctype_fetch(resp$response_headers) %||% NA,
    headers = headers %||% raw(0),
    modified = resp$modified %||% NA,
    times = resp$times %||% numeric(0),
    content = resp$content
  )
}

ctype_fetch <- function(x) {
  match_ctype <- which("content-type" == tolower(names(x)))
  if (length(match_ctype) > 0) x[[match_ctype]]
}

#' Build a curl request
#' @export
#' @param x an unexecuted curl request object
#' @return a curl request
build_curl_request = function(x) {
  RequestSignature$new(
    method = x$method,
    uri = x$url,
    options = list(
      body = x$body %||% NULL,
      headers = x$headers %||% NULL,
      proxies = x$proxies %||% NULL,
      auth = x$auth %||% NULL
    )
  )
}

#' @noRd
#' @keywords internal
#' @examples
#' foo <- list(a = 5, `User-Agent` = "ropensci")
#' make_curl_headers(foo)
#' cat(make_curl_headers(foo))
make_curl_headers <- function(x) {
  stopifnot(is.list(x))
  paste0(paste0(
    paste(names(x), unname(unlist(x)), sep = ": "),
    collapse = "\r\n"
  ), "\r\n\r\n")
}
