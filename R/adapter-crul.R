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
        # VCR: recordable/ignored
        if ("package:vcr" %in% search()) {
          cas <- vcr::current_cassette()
          if (length(cas$previously_recorded_interactions()) == 0) {
            # using vcr, but no recorded interactions to the cassette yet
            # do real request
            # tmp <- crul::HttpClient$new(url = req$url$url)
            # tmp2 <- webmockr_crul_fetch(req)
            # crul_resp <- build_crul_response(req, tmp2)

            # use RequestHandler - gets current cassette & record interaction
            crul_resp <- vcr::RequestHandlerCrul$new(req)$handle()
          }
        } else {
          crul_resp <- build_crul_response(req, resp)

          # add to_return() elements if given
          if (length(cc(ss$responses_sequences)) != 0) {
            # remove NULLs
            toadd <- cc(ss$responses_sequences)
            # modify responses
            for (i in seq_along(toadd)) {
              if (names(toadd)[i] == "status") {
                crul_resp$status_code <- toadd[[i]]
              }
              if (names(toadd)[i] == "body") {
                # crul_resp$content <- toadd[[i]]
                crul_resp$content <- ss$responses_sequences$body_raw
              }
              if (names(toadd)[i] == "headers") {
                crul_resp$response_headers <- toadd[[i]]
              }
            }
          }
        }

        # if vcr loaded: record http interaction into vcr namespace
        # VCR: recordable/stubbed_by_vcr ??
        if ("package:vcr" %in% search()) {
          # get current cassette
          cas <- vcr::current_cassette()
          # FIXME: doing this fix for record:once - not sure will work for other record modes
          ## OLD STUFF
          # record http interaction into vcr http interaction list
          # cas$record_http_interaction(crul_resp)
          ## OLD STUFF
          # SO, don't record interaction since we have a match

          # if (length(cas$previously_recorded_interactions()) == 0) {
          #   cas$record_http_interaction(crul_resp)
          # }

          # use RequestHandler instead? - which gets current cassette for us
          #  here, record interaction
          # cat(paste0("class <req>: ", paste0(class(req), collapse = ", ")), sep = "\n")
          crul_resp <- vcr::RequestHandlerCrul$new(req)$handle()

          # build crul_resp from vcr http interaction on disk or previously
          # recorded in memory
          # crul_resp <- cas$serialize_to_crul()

        } # vcr is not loaded, skip

      } else if (webmockr_net_connect_allowed(uri = req$url$url)) {
        # fail early here if certain failure conditions met
        # VCR: unhandled
        # if ("package:vcr" %in% search()) {
        #   # get current cassette
        #   # cas <- vcr::current_cassette()
        #   # since no match for given request, then error here
        #   #   if certain conditions met
        #   # vcr::UnhandledHTTPRequestError$new(req)$run()

        #   # use RequestHandler instead
        #   vcr::RequestHandlerCrul$new(req)$handle()
        # }

        # if real requests || localhost || certain exceptions ARE
        #   allowed && nothing found above
        tmp <- crul::HttpClient$new(url = req$url$url)
        tmp2 <- webmockr_crul_fetch(req)
        crul_resp <- build_crul_response(req, tmp2)

        # if vcr loaded: record http interaction into vcr namespace
        # VCR: recordable
        if ("package:vcr" %in% search()) {
          # stub request so next time we match it
          urip <- crul::url_parse(req$url$url)
          m <- vcr::vcr_configuration()$match_requests_on

          if (all(m %in% c("method", "uri")) && length(m) == 2) {
            stub_request(req$method, req$url$url)
          } else if (all(m %in% c("method", "uri", "query")) && length(m) == 3) {
            stub_request(req$method, req$url$url) %>% wi_th(query = urip$parameter)
          } else if (all(m %in% c("method", "uri", "headers")) && length(m) == 3) {
            stub_request(req$method, req$url$url) %>% wi_th(headers = req$headers)
          } else if (all(m %in% c("method", "uri", "headers", "query")) && length(m) == 4) {
            stub_request(req$method, req$url$url) %>%
              wi_th(
                query = urip$parameter,
                headers = req$headers
              )
          }

          # get current cassette
          # cas <- vcr::current_cassette()
          # record http interaction into vcr http interaction list
          # cas$record_http_interaction(crul_resp)

          # use RequestHandler instead? - which gets current cassette for us
          vcr::RequestHandlerCrul$new(req)$handle()
        }

      } else {
        # no stubs found and net connect not allowed - STOP
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

