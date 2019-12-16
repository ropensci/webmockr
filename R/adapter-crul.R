#' @title CrulAdapter
#' @description \pkg{crul} library adapter
#' @export
#' @family http_lib_adapters
#' @details This adapter modifies \pkg{crul} to allow mocking HTTP requests
CrulAdapter <- R6::R6Class(
  'CrulAdapter',
  public = list(
    #' @field name adapter name
    name = "crul_adapter",

    #' @description Enable the adapter
    #' @return `TRUE`, invisibly
    enable = function() {
      message("CrulAdapter enabled!")
      webmockr_lightswitch$crul <- TRUE
      crul::mock(TRUE)
      invisible(TRUE)
    },

    #' @description Disable the adapter
    #' @return `FALSE`, invisibly
    disable = function() {
      message("CrulAdapter disabled!")
      webmockr_lightswitch$crul <- FALSE
      crul::mock(FALSE)
      self$remove_crul_stubs()
      invisible(FALSE)
    },

    #' @description All logic for handling a request
    #' @param req a crul request
    #' @return various outcomes
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
                if (inherits(ss$responses_sequences$body_raw, "mock_file")) {
                  cat(ss$responses_sequences$body_raw$payload,
                    file = ss$responses_sequences$body_raw$path,
                    sep = "\n")
                  ss$responses_sequences$body_raw <-
                    ss$responses_sequences$body_raw$path
                }
                crul_resp$content <- ss$responses_sequences$body_raw
              }
              if (names(toadd)[i] == "headers") {
                crul_resp$response_headers <- 
                  names_to_lower(as_character(toadd[[i]]))
                crul_resp$response_headers_all <- 
                  list(crul_resp$response_headers)
              }
            }
          }
        }

        # if vcr loaded: record http interaction into vcr namespace
        # VCR: recordable/stubbed_by_vcr ??
        if ("package:vcr" %in% search()) {
          # get current cassette
          cas <- vcr::current_cassette()
          crul_resp <- vcr::RequestHandlerCrul$new(req)$handle()
        } # vcr is not loaded, skip

        # if written to disk, see if we should modify file path
        if ("package:vcr" %in% search()) {
          if (is.character(crul_resp$content)) {
            write_disk_path <- vcr::vcr_configuration()$write_disk_path
            write_disk_path <- normalizePath(write_disk_path, mustWork=TRUE)
            crul_resp$content <- file.path(write_disk_path, basename(crul_resp$content))
          }
        }

      } else if (webmockr_net_connect_allowed(uri = req$url$url)) {
        # if real requests || localhost || certain exceptions ARE
        #   allowed && nothing found above
        tmp <- crul::HttpClient$new(url = req$url$url)
        tmp2 <- webmockr_crul_fetch(req)
        crul_resp <- build_crul_response(req, tmp2)

        # if vcr loaded: record http interaction into vcr namespace
        # VCR: recordable
        if ("package:vcr" %in% search()) {
          # if written to disk, see if we should modify file path
          if (is.character(crul_resp$content)) {
            if (file.exists(crul_resp$content)) {
              write_disk_path <- vcr::vcr_configuration()$write_disk_path
              write_disk_path <- normalizePath(write_disk_path, mustWork=TRUE)
              crul_resp$content <- file.path(write_disk_path, basename(crul_resp$content))
            }
          }

          # stub request so next time we match it
          urip <- crul::url_parse(req$url$url)
          m <- vcr::vcr_configuration()$match_requests_on
        
          if (all(m %in% c("method", "uri")) && length(m) == 2) {
            stub_request(req$method, req$url$url)
          } else if (all(m %in% c("method", "uri", "query")) && length(m) == 3) {
            tmp <- stub_request(req$method, req$url$url)
            wi_th(tmp, .list = list(query = urip$parameter))
          } else if (all(m %in% c("method", "uri", "headers")) && length(m) == 3) {
            tmp <- stub_request(req$method, req$url$url)
            wi_th(tmp, .list = list(headers = req$headers))
          } else if (all(m %in% c("method", "uri", "headers", "query")) && length(m) == 4) {
            tmp <- stub_request(req$method, req$url$url)
            wi_th(tmp, .list = list(query = urip$parameter, headers = req$headers))
          }
        
          # use RequestHandler instead? - which gets current cassette for us
          vcr::RequestHandlerCrul$new(req)$handle()
        }

      } else {
        # throw vcr error: should happen when user not using
        #  use_cassette or insert_cassette
        if ("package:vcr" %in% search()) {
          vcr::RequestHandlerCrul$new(req)$handle()
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

      return(crul_resp)
    },

    #' @description Remove all crul stubs
    #' @return nothing returned; removes all crul request stubs
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

#' Build a crul response
#' @export
#' @param req a request
#' @param resp a response
#' @return a crul response
build_crul_response <- function(req, resp) {
  # prep headers
  if (grepl("^ftp://", resp$url %||% "")) { # in case uri_regex only
    headers <- list()
  } else {
    hds <- resp$headers
    if (is.null(hds)) {
      hds <- resp$response_headers
      headers <- if (is.null(hds)) {
        list()
      } else {
        stopifnot(is.list(hds))
        stopifnot(is.character(hds[[1]]))
        hds
      }
    } else {
      hh <- rawToChar(hds %||% raw(0))
      if (is.null(hh) || nchar(hh) == 0) {
        headers <- list()
      } else {
        headers <- lapply(curl::parse_headers(hh, multiple = TRUE), 
          crul_headers_parse)
      }
    }
  }

  crul::HttpResponse$new(
    method = req$method,
    url = req$url$url,
    status_code = resp$status_code,
    request_headers = c('User-Agent' = req$options$useragent, req$headers),
    response_headers = {
      if (all(hz_namez(headers))) headers else last(headers)
    },
    response_headers_all = headers,
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
      body = pluck_body(x),
      headers = x$headers %||% NULL,
      proxies = x$proxies %||% NULL,
      auth = x$auth %||% NULL,
      disk = x$disk %||% NULL
    )
  )
}
