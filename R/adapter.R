#' @title Adapters for Modifying HTTP Requests
#' @description `Adapter` is the base parent class used to implement
#'   \pkg{webmockr} support for different HTTP clients. It should not be used
#'   directly. Instead, use one of the client-specific adapters that webmockr
#'   currently provides:
#' * `CrulAdapter` for \pkg{crul}
#' * `HttrAdapter` for \pkg{httr}
#' * `Httr2Adapter` for \pkg{httr2}
#' @details Note that the documented fields and methods are the same across all
#'   client-specific adapters.
#' @keywords internal
Adapter <- R6::R6Class(
  "Adapter",
  public = list(
    #' @field client HTTP client package name
    client = NULL,
    #' @field name adapter name
    name = NULL,

    #' @description Create a new Adapter object
    initialize = function() {
      if (is.null(self$client)) {
        abort(c(
          "Adapter parent class should not be called directly",
          "*" = "Use one of the following package-specific adapters instead:",
          "*" = "  CrulAdapter$new()",
          "*" = "  HttrAdapter$new()",
          "*" = "  Httr2Adapter$new()"
        ))
      }
    },

    #' @description Enable the adapter
    #' @param quiet (logical) suppress messages? default: `FALSE`
    #' @return `TRUE`, invisibly
    enable = function(quiet = FALSE) {
      assert_is(quiet, "logical")
      if (!quiet) {
        message(sprintf("%s enabled!", self$name))
      }
      webmockr_lightswitch[[self$client]] <- TRUE

      switch(
        self$client,
        crul = crul_mock(on = TRUE),
        httr = httr_mock(on = TRUE),
        httr2 = httr2_mock(on = TRUE)
      )
    },

    #' @description Disable the adapter
    #' @param quiet (logical) suppress messages? default: `FALSE`
    #' @return `FALSE`, invisibly
    disable = function(quiet = FALSE) {
      assert_is(quiet, "logical")
      if (!quiet) {
        message(sprintf("%s disabled!", self$name))
      }
      webmockr_lightswitch[[self$client]] <- FALSE
      self$remove_stubs()

      switch(
        self$client,
        crul = crul_mock(on = FALSE),
        httr = httr_mock(on = FALSE),
        httr2 = httr2_mock(on = FALSE)
      )
    },

    #' @description All logic for handling a request
    #' @param req a request
    #' @return various outcomes
    handle_request = function(req) {
      # put request in request registry
      request_signature <- private$build_request(req)
      webmockr_request_registry$register_request(
        request = request_signature
      )

      if (request_is_in_cache(request_signature)) {
        # if real requests NOT allowed
        # even if net connects allowed, we check if stubbed found first
        ss <- webmockr_stub_registry$find_stubbed_request(
          request_signature
        )[[1]]

        # if user wants to return a partial object
        #   get stub with response and return that
        resp <- private$build_stub_response(ss)

        resp <- private$build_response(req, resp)
        # add to_return() elements if given
        resp <- private$add_response_sequences(ss, resp)

        # request is not in cache but connections are allowed
      } else if (webmockr_net_connect_allowed(uri = private$pluck_url(req))) {
        # if real requests || localhost || certain exceptions ARE
        #   allowed && nothing found above
        private$mock(on = FALSE)
        resp <- private$fetch_request(req)
        private$mock(on = TRUE)

        # request is not in cache and connections are not allowed
      } else {
        # no stubs found and net connect not allowed - STOP
        x <- c(
          "Real HTTP connections are disabled.",
          "!" = "Unregistered request:"
        )
        y <- "\nYou can stub this request with the following snippet:\n"
        z <- "\nregistered request stubs:\n"
        msgx <- c(x, "i" = request_signature$to_s())
        msgy <- ""
        if (webmockr_conf_env$show_stubbing_instructions) {
          msgy <- paste(y, private$make_stub_request_code(request_signature))
        }
        msgz <- ""
        if (length(webmockr_stub_registry$request_stubs)) {
          msgz <- paste(
            z,
            paste0(
              vapply(
                webmockr_stub_registry$request_stubs,
                function(z) {
                  z$to_s()
                },
                ""
              ),
              collapse = "\n "
            )
          )
        }
        msg_diff <- ""
        if (webmockr_conf_env$show_body_diff) {
          msg_diff <- private$make_body_diff(request_signature)
        }
        ending <- paste0("\n", paste(rep.int("=", 60), collapse = ""))
        abort(c(msgx, msgy, msgz, msg_diff, ending))
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
            paste(
              sprintf("'%s'", names(hd)),
              sprintf("'%s'", unlist(unname(hd))),
              sep = " = "
            ),
            collapse = ", "
          )
        }

        # body can be lots of things, so need to handle various cases
        if (!is.null(x$body)) {
          bd <- x$body
          bd_str <- hdl_lst2(bd, client = self$client)
        }

        with_str <- ""
        if (all(nzchar(hd_str) && nzchar(bd_str))) {
          with_str <- sprintf(
            paste0(
              " wi_th(\n       headers = list(%s),",
              "\n       body = list(%s)\n     )"
            ),
            hd_str,
            bd_str
          )
        } else if (nzchar(hd_str) && !nzchar(bd_str)) {
          with_str <- sprintf(
            " wi_th(\n       headers = list(%s)\n     )",
            hd_str
          )
        } else if (!nzchar(hd_str) && nzchar(bd_str)) {
          with_str <- sprintf(" wi_th(\n       body = list(%s)\n     )", bd_str)
        }

        tmp <- paste0(tmp, " %>%\n    ", with_str)
      }
      return(tmp)
    },
    build_stub_response = function(stub) {
      stopifnot(inherits(stub, "StubbedRequest"))
      resp <- Response$new()
      resp$set_url(stub$uri)
      resp$set_body(stub$body)
      resp$set_request_headers(stub$request_headers)
      resp$set_response_headers(stub$response_headers)
      resp$set_status(as.integer(stub$status_code %||% 200))

      stub_num_get <- stub$counter$count()
      if (stub_num_get > length(stub$responses_sequences)) {
        stub_num_get <- length(stub$responses_sequences)
      }
      respx <- stub$responses_sequences[[stub_num_get]]

      # if user set to_timeout or to_raise, do that
      if (!is.null(respx)) {
        if (respx$timeout || respx$raise) {
          if (respx$timeout) {
            x <- fauxpas::HTTPRequestTimeout$new()
            resp$set_status(x$status_code)
            x$do_verbose(resp)
          }
          if (respx$raise) {
            x <- respx$exceptions[[1]]$new()
            resp$set_status(x$status_code)
            x$do_verbose(resp)
          }
        }
      }
      return(resp)
    },
    add_response_sequences = function(stub, response) {
      # TODO: assert HttpResponse (is it ever a crul response?)
      stopifnot(inherits(stub, "StubbedRequest"))

      # FIXME: temporary fix, change to using request registry counter
      # to decide which responses_sequence entry to use

      # choose which response to return
      stub_num_get <- stub$counter$count()
      if (stub_num_get > length(stub$responses_sequences)) {
        stub_num_get <- length(stub$responses_sequences)
      }
      respx <- stub$responses_sequences[[stub_num_get]]
      # remove NULLs
      toadd <- cc(respx)
      if (is.null(toadd)) {
        return(response)
      }

      # remove timeout, raise, exceptions fields
      toadd <- toadd[!names(toadd) %in% c("timeout", "raise", "exceptions")]

      for (i in seq_along(toadd)) {
        if (names(toadd)[i] == "status") {
          response$status_code <- as.integer(toadd[[i]])
        }

        if (names(toadd)[i] == "body") {
          if (inherits(respx$body_raw, "mock_file")) {
            cat_line(
              respx$body_raw$payload,
              file = respx$body_raw$path
            )
            respx$body_raw <-
              respx$body_raw$path
            if (self$client == "httr") {
              class(respx$body_raw) <- "path"
            }
            if (self$client == "httr2") {
              class(respx$body_raw) <- "httr2_path"
            }
          }

          body_type <- attr(respx$body_raw, "type") %||% ""

          if (self$client == "httr" && body_type == "file") {
            attr(respx$body_raw, "type") <- NULL
            class(respx$body_raw) <- "path"
          }

          if (self$client == "httr2" && body_type == "file") {
            attr(respx$body_raw, "type") <- NULL
            class(respx$body_raw) <- "httr2_path"
          }

          if (self$client == "httr2") {
            response$body <- respx$body_raw
          } else {
            response$content <- respx$body_raw
          }
        }

        if (names(toadd)[i] == "headers") {
          headers <- names_to_lower(as_character(toadd[[i]]))
          if (self$client == "crul") {
            response$response_headers <- headers
            response$response_headers_all <- list(headers)
          } else if (self$client == "httr") {
            response$headers <- httr::insensitive(headers)
          } else {
            response$headers <- httr2_headers(headers)
          }
        }
      }

      return(response)
    },
    make_body_diff = function(request_signature) {
      check_installed("diffobj")
      prefix <- "\n\nBody diff:"
      stubs <- webmockr_stub_registry$request_stubs
      comps <- lapply(stubs, \(stub) {
        diffobj::diffObj(stub$body, request_signature$body)
      })
      num_diffs <- vapply(comps, \(w) attr(w@diffs, "meta")$diffs[2], 1)
      if (length(stubs) > 1) {
        diffs_msg <- "diffs: >1 stub found, showing diff with least differences"
        diff_to_show <- comps[which.min(num_diffs)][[1]]
        c(prefix, "i" = diffs_msg, as.character(diff_to_show))
      } else {
        c(prefix, as.character(comps[[1]]))
      }
    }
  )
)
