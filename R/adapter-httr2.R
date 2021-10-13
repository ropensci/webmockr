#' Build a httr response
#' @export
#' @param req a request
#' @param resp a response
#' @return a httr response

build_httr2_response <- function(req, resp) {
  try_url <- tryCatch(resp$url, error = function(e) e)

  lst <- list(
    url = try_url %|s|% req$url,
    status_code = as.integer(resp$status_code),
    headers = {
      if (grepl("^ftp://", resp$url %||% "")) { # in case uri_regex only
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
            httr::insensitive(hds)
          }
        } else {
          httr::insensitive(hds)
        }
      }
    },
    all_headers = list(),
    cookies = httr2_cookies_df(),
    content = resp$content,
    date = {
      if (!is.null(resp$response_headers$date)) {
        httr::parse_http_date(resp$response_headers$date)
      } else {
        Sys.time()
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

httr2_cookies_df <- function() {
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  x <- c("domain", "flag", "path", "secure", "expiration", "name", "value")
  colnames(df) <- x
  df
}

#' Build a httr2 request
#' @export
#' @param x an unexecuted httr request object
#' @return a httr request
build_httr2_request = function(x) {
  headers <- as.list(x$headers) %||% NULL
  auth <- check_user_pwd(x$options$userpwd) %||% NULL
  if (!is.null(auth)) {
    auth_header <- prep_auth(auth)
    headers <- c(headers, auth_header)
  }
  RequestSignature$new(
    method = x$method,
    uri = x$url,
    options = list(
      body = pluck_body(x),
      headers = headers,
      proxies = x$proxies %||% NULL,
      auth = auth,
      disk = x$disk %||% NULL,
      fields = x$fields %||% NULL,
      output = x$output %||% NULL
    )
  )
}

# NOTE: dont think this method is needed anymore for httr2
#' Turn on httr2 mocking
#' Sets a callback that routes httr request through webmockr
#' 
#' @export
#' @param on (logical) set to `TRUE` to turn on, and `FALSE`
#' to turn off. default: `TRUE`
#' @return Silently returns `TRUE` when enabled and `FALSE` when disabled.
# httr_mock <- function(on = TRUE) {
#   check_for_pkg("httr")
#   webmockr_handle <- function(req) {
#     webmockr::HttrAdapter$new()$handle_request(req)
#   }
#   if (on) {
#     httr2::set_callback("request", webmockr_handle)
#   } else {
#     httr2::set_callback("request", NULL)
#   }
#   invisible(on)
# }

#' @rdname Adapter
#' @export
Httr2Adapter <- R6::R6Class("Httr2Adapter",
  inherit = Adapter,
  public = list(
    #' @field client HTTP client package name
    client = "httr",
    #' @field name adapter name
    name = "Httr2Adapter"
  ),

  private = list(
    pluck_url = function(request) request$url,

    mock = function(on) httr2_mock(on),

    build_request   = build_httr2_request,
    build_response  = build_httr2_response,

    request_handler = function(request) vcr::RequestHandlerHttr$new(request),

    fetch_request = function(request) {
      METHOD <- eval(parse(text = paste0("httr2::", request$method)))
      METHOD(
        private$pluck_url(request),
        body = pluck_body(request),
        do.call(httr::config, request$options),
        httr::add_headers(request$headers),
        if (!is.null(request$output$path)) {
          httr::write_disk(request$output$path, TRUE)
        } 
      )
    }
  )
)
