#' httr library adapter
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
#'     \item{`build_httr_request(x)`}{
#'       Build a httr [RequestSignature]
#'       x: httr request parts (list)
#'     }
#'     \item{`build_httr_response(req, resp)`}{
#'       Build a httr response
#'       req: a httr request (list)
#'       resp: a httr response ()
#'     }
#'     \item{`handle_request()`}{
#'       All logic for handling a request
#'       req: a httr request (list)
#'     }
#'     \item{`remove_httr_stubs()`}{
#'       Remove all httr stubs
#'     }
#'   }
#'
#' This adapter modifies \pkg{httr} to allow mocking HTTP requests
#'
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' library(httr)
#'
#' # normal httr request, works fine
#' real <- GET("https://httpbin.org/get")
#'
#' # with webmockr
#' library(webmockr)
#' ## turn on httr mocking
#' httr_mock()
#' ## now this request isn't allowed
#' GET("https://httpbin.org/get")
#' ## stub the request
#' stub_request('get', uri = 'https://httpbin.org/get') %>%
#'   wi_th(
#'     headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
#'   ) %>%
#'   to_return(status = 418, body = "I'm a teapot!", headers = list(a = 5))
#' ## now the request succeeds and returns a mocked response
#' (res <- GET("https://httpbin.org/get"))
#' res$status_code
#' rawToChar(res$content)
#'
#' # allow real requests while webmockr is loaded
#' webmockr_allow_net_connect()
#' webmockr_net_connect_allowed()
#' GET("https://httpbin.org/get?animal=chicken")
#' webmockr_disable_net_connect()
#' webmockr_net_connect_allowed()
#' # GET("https://httpbin.org/get?animal=chicken")
#' }
HttrAdapter <- R6::R6Class(
  'HttrAdapter',
  public = list(
    name = "httr_adapter",

    enable = function() {
      message("HttrAdapter enabled!")
      webmockr_lightswitch$httr <- TRUE
      httr_mock(TRUE)
      invisible(TRUE)
    },

    disable = function() {
      message("HttrAdapter disabled!")
      webmockr_lightswitch$httr <- FALSE
      httr_mock(FALSE)
      self$remove_httr_stubs()
      invisible(FALSE)
    },

    handle_request = function(req) {
      # put request in request registry
      request_signature <- build_httr_request(req)
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
        resp$set_status(as.integer(ss$status_code %||% 200))

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

        # generate httr response
        httr_resp <- build_httr_response(req, resp)

        # add to_return() elements if given
        if (length(cc(ss$responses_sequences)) != 0) {
          # remove NULLs
          toadd <- cc(ss$responses_sequences)
          # modify responses
          for (i in seq_along(toadd)) {
            if (names(toadd)[i] == "status") {
              httr_resp$status_code <- as.integer(toadd[[i]])
            }
            if (names(toadd)[i] == "body") {
              # httr_resp$content <- toadd[[i]]
              httr_resp$content <- ss$responses_sequences$body_raw
            }
            if (names(toadd)[i] == "headers") {
              httr_resp$headers <- toadd[[i]]
            }
          }
        }

      } else if (webmockr_net_connect_allowed(uri = req$url)) {
        # if real requests || localhost || certain exceptions ARE
        #   allowed && nothing found above
        httr_mock(FALSE)
        httr_resp <- eval(parse(text = paste0("httr::", req$method)))(req$url)
        httr_mock(TRUE)
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

      return(httr_resp)
    },

    remove_httr_stubs = function() {
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

#' Build a httr response
#' @export
#' @param req a request
#' @param resp a response
#' @return a httr response
build_httr_response <- function(req, resp) {
  lst <- list(
    url = req$url,
    status_code = as.integer(resp$status_code),
    headers = {
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
          hds
        }
      }
    },
    all_headers = list(),
    cookies = httr_cookies_df(),
    content = resp$content,
    date = {
      if (!is.null(resp$response_headers$date)) {
        resp$response_headers$date
      }
    },
    times = numeric(0),
    request = req,
    handle = NA
  )
  lst$all_headers <- list(list(
    status = lst$status_code,
    version = "",
    headers = lst$headers
  ))
  structure(lst, class = "response")
}

httr_cookies_df <- function() {
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  x <- c("domain", "flag", "path", "secure", "expiration", "name", "value")
  colnames(df) <- x
  df
}

#' Build a httr request
#' @export
#' @param x an unexecuted httr request object
#' @return a httr request
build_httr_request = function(x) {
  RequestSignature$new(
    method = x$method,
    uri = x$url,
    options = list(
      body = x$fields %||% NULL,
      headers = as.list(x$headers) %||% NULL,
      proxies = x$proxies %||% NULL,
      auth = x$auth %||% NULL
    )
  )
}

#' Turn on httr mocking
#' @export
#' @param on (logical) set to `TRUE` to turn on, and `FALSE`
#' to turn off. default: `TRUE`
#' @return silently sets a callback that routes httr request
#' through webmockr
httr_mock <- function(on = TRUE) {
  check_for_pkg("httr")
  webmockr_handle <- function(req) {
    webmockr::HttrAdapter$new()$handle_request(req)
  }
  if (on) {
    httr::set_callback("request", webmockr_handle)
  } else {
    httr::set_callback("request", NULL)
  }
}
