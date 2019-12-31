#' @title Adapter
#' @description Default methods are based on crul, which are overridden in HttrAdapter where necessary
#' @export
#' @family http_lib_adapters
#' @details This adapter modifies \pkg{httr} to allow mocking HTTP requests
#' @examples \dontrun{
#' if (requireNamespace("httr", quietly = TRUE)) {
#' # library(httr)
#'
#' # normal httr request, works fine
#' # real <- GET("https://httpbin.org/get")
#' # real
#'
#' # with webmockr
#' # library(webmockr)
#' ## turn on httr mocking
#' # httr_mock()
#' ## now this request isn't allowed
#' # GET("https://httpbin.org/get")
#' ## stub the request
#' # stub_request('get', uri = 'https://httpbin.org/get') %>%
#' #   wi_th(
#' #     headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
#' #   ) %>%
#' #   to_return(status = 418, body = "I'm a teapot!", headers = list(a = 5))
#' ## now the request succeeds and returns a mocked response
#' # (res <- GET("https://httpbin.org/get"))
#' # res$status_code
#' # rawToChar(res$content)
#'
#' # allow real requests while webmockr is loaded
#' # webmockr_allow_net_connect()
#' # webmockr_net_connect_allowed()
#' # GET("https://httpbin.org/get?animal=chicken")
#' # webmockr_disable_net_connect()
#' # webmockr_net_connect_allowed()
#' # GET("https://httpbin.org/get?animal=chicken")
#'
#' # httr_mock(FALSE)
#' }
#' }
Adapter <- R6::R6Class("Adapter",
  public = list(
    #' @field client HTTP client package name
    client = NULL,
    #' @field name adapter name
    name = NULL,

    #' @description Enable the adapter
    #' @return `TRUE`, invisibly
    enable = function() {
      message(sprintf("%s enabled!", self$name))
      webmockr_lightswitch[[self$client]] <- TRUE
      
      switch(self$client,
        crul = crul::mock(on = TRUE),
        httr = httr_mock(on = TRUE)
      )
    },

    #' @description Disable the adapter
    #' @return `FALSE`, invisibly
    disable = function() {
      message(sprintf("%s disabled!", self$name))
      webmockr_lightswitch[[self$client]] <- FALSE
      self$remove_stubs()

      switch(self$client,
        crul = crul::mock(on = FALSE),
        httr = httr_mock(on = FALSE)
      )
    },

    #' @description All logic for handling a request
    #' @param req a request
    #' @return various outcomes
    handle_request = function(req) {
      browser()
      # put request in request registry
      request_signature <- private$build_request(req)
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

        # generate response
        # VCR: recordable/ignored
        if ("package:vcr" %in% search()) {
          cas <- vcr::current_cassette()
          if (length(cas$previously_recorded_interactions()) == 0) {
            # using vcr, but no recorded interactions to the cassette yet
            # use RequestHandler - gets current cassette & record interaction
            resp <- private$request_handler(req)$handle()
          }
        } else {
          resp <- private$build_response(req, resp)

          # add to_return() elements if given
          if (length(cc(ss$responses_sequences)) != 0) {
            # remove NULLs
            toadd <- cc(ss$responses_sequences)
            # modify responses
            for (i in seq_along(toadd)) {
              if (names(toadd)[i] == "status") {
                resp$status_code <- as.integer(toadd[[i]])
              }
              if (names(toadd)[i] == "body") {
                
                if (inherits(ss$responses_sequences$body_raw, "mock_file")) {
                  cat(ss$responses_sequences$body_raw$payload,
                    file = ss$responses_sequences$body_raw$path,
                    sep = "\n")
                  ss$responses_sequences$body_raw <- ss$responses_sequences$body_raw$path
                  if (self$client == "httr") {
                    class(ss$responses_sequences$body_raw) <- "path"
                  }
                }

                if (self$client == "httr") {
                  body_type <- attr(ss$responses_sequences$body_raw, "type") %||% ""
                  if (body_type == "file") {
                    attr(ss$responses_sequences$body_raw, "type") <- NULL
                    class(ss$responses_sequences$body_raw) <- "path"
                  }
                }

                resp$content <- ss$responses_sequences$body_raw
              }
              if (names(toadd)[i] == "headers") {
                headers <- names_to_lower(as_character(toadd[[i]]))
                if (self$client == "crul") {
                  resp$response_headers <- headers
                  resp$response_headers_all <- list(headers)
                } else {
                  resp$headers <- httr::insensitive(headers)
                }
              }
            }
          }
        }

        # if vcr loaded: record http interaction into vcr namespace
        # VCR: recordable/stubbed_by_vcr ??
        if ("package:vcr" %in% search()) {
          # get current cassette
          cas <- vcr::current_cassette()
          resp <- private$request_handler(req)$handle()
          
          # if written to disk, see if we should modify file path
          if (self$client == "crul" && is.character(resp$content)) {
            resp <- private$update_vcr_disk_path(resp)
          }
        } # vcr is not loaded, skip

      # request is not in cache but connections are allowed
      } else if (webmockr_net_connect_allowed(uri = private$pluck_url(req))) {
        # if real requests || localhost || certain exceptions ARE
        #   allowed && nothing found above
        private$mock(on = FALSE)
        resp <- private$fetch_request(req)
        private$mock(on = TRUE)

        # if vcr loaded: record http interaction into vcr namespace
        # VCR: recordable
        if ("package:vcr" %in% search()) {
          
          # if written to disk, see if we should modify file path
          if (self$client == "crul" && is.character(resp$content)) {
            if (file.exists(resp$content)) {
              resp <- private$update_vcr_disk_path(resp)
            }
          }
          
          # stub request so next time we match it
          req_url <- private$pluck_url(req)
          urip <- crul::url_parse(req_url)
          m <- vcr::vcr_configuration()$match_requests_on

          if (all(m %in% c("method", "uri")) && length(m) == 2) {
            stub_request(req$method, req_url)
          } else if (all(m %in% c("method", "uri", "query")) && length(m) == 3) {
            tmp <- stub_request(req$method, req_url)
            wi_th(tmp, .list = list(query = urip$parameter))
          } else if (all(m %in% c("method", "uri", "headers")) && length(m) == 3) {
            tmp <- stub_request(req$method, req_url)
            wi_th(tmp, .list = list(headers = req$headers))
          } else if (all(m %in% c("method", "uri", "headers", "query")) && length(m) == 4) {
            tmp <- stub_request(req$method, req_url)
            wi_th(tmp, .list = list(query = urip$parameter, headers = req$headers))
          }

          # use RequestHandler instead? - which gets current cassette for us
          private$request_handler(req)$handle()
        }
      
      # request is not in cache and connections are not allowed
      } else {
        # throw vcr error: should happen when user not using
        #  use_cassette or insert_cassette
        if ("package:vcr" %in% search()) {
          private$request_handler(req)$handle()
        }

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

      return(resp)
    },

    #' @description Remove all stubs
    #' @return nothing returned; removes all request stubs
    remove_stubs = function() {
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

        if (all(nzchar(hd_str) && nzchar(bd_str))) {
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
