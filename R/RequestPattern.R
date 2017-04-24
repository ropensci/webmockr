#' RequestPattern class
#'
#' @export
#' @param method the HTTP method (any, head, options, get, post, put,
#' patch, trace, or delete). "any" matches any HTTP method. required.
#' @param uri (character) request URI. required.
#' @param options (list) options. optional
#' @details
#' **Methods**
#'   \describe{
#'     \item{`matches(request_signature)`}{
#'       Test if request_signature matches a pattern
#'       - request_signature: a request signature
#'     }
#'     \item{`to_s()`}{
#'       Print pattern for easy human consumption
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' (x <- RequestPattern$new(method = "get", uri = "https://httpbin.org/get"))
#' x$body_pattern
#' x$headers_pattern
#' x$method_pattern
#' x$uri_pattern
#' x$to_s()
#'
#' # make a request signature
#' rs <- RequestSignature$new(metho="get", uri="https://httpbin.org/get")
#'
#' # check if it matches
#' x$matches(rs)
#' }
RequestPattern <- R6::R6Class(
  'RequestPattern',
  public = list(
    method_pattern = NULL,
    uri_pattern = NULL,
    body_pattern = NULL,
    headers_pattern = NULL,

    initialize = function(method, uri, options = list()) {
      self$method_pattern <- MethodPattern$new(method)
      self$uri_pattern <- UriPattern$new(uri)
      if (length(options)) private$assign_options(options)
    },

    matches = function(request_signature) {
      c_type <- if (!is.null(request_signature$headers)) request_signature$headers$`Content-Type` else NULL
      c_type <- if (!is.null(c_type)) strsplit(c_type, ';')[[1]][1]
      self$method_pattern$matches(request_signature$method) &&
        self$uri_pattern$matches(request_signature$uri) &&
        (is.null(self$body_pattern) || self$body_pattern$matches(request_signature$body, c_type %||% "")) &&
        (is.null(self$headers_pattern) || self$headers_pattern$matches(request_signature$headers))
    },

    to_s = function() {
      gsub("^\\s+|\\s+$", "", paste(
        toupper(self$method_pattern$to_s()),
        self$uri_pattern$to_s(),
        if (!is.null(self$body_pattern)) paste0(" with body ", self$body_pattern$to_s()),
        if (!is.null(self$headers_pattern)) paste0(" with headers ", self$headers_pattern$to_s())
      ))
    }
  ),

  private = list(
    assign_options = function(options) {
      self$validate_keys(options, 'body', 'headers', 'query', 'basic_auth')
      set_basic_auth_as_headers(options)
      self$body_pattern <- if ('body' %in% names(options)) BodyPattern$new(options['body'])
      self$headers_pattern <- if ('headers' %in% names(options)) HeadersPattern$new(options['headers'])
      if ('query' %in% names(options)) self$uri_pattern$add_query_params(options['query'])
    },

    validate_keys = function(x, ...) {
      valid_keys <- unlist(list(...), recursive = FALSE)
      for (i in seq_along(x)) {
        if (!names(x)[i] %in% valid_keys) {
          stop(
            sprintf("Unknown key: %s. Valid keys are: %s",
                    names(x)[i],
                    paste0(valid_keys, collapse = ", "),
                    call. = FALSE
            )
          )
        }
      }
    },

    set_basic_auth_as_headers = function(options) {
      if ('basic_auth' %in% names(options)) {
        private$validate_basic_auth(options$basic_auth)
        options$headers <- list()
        options$headers$Authorization <-
          private$make_basic_auth(options$basic_auth[1], options$basic_auth[2])
      }
    },

    validate_basic_auth = function(x) {
      if (!inherits(x, "list") || length(unique(unname(unlist(x)))) == 1) {
        stop(
          "'basic_auth' option should be a list of length 2: username and password",
          call. = FALSE
        )
      }
    },

    make_basic_auth = function(x, y) {
      jsonlite::base64_enc(paste0(x, ":", y))
    }
  )
)

# MethodPattern
MethodPattern <- R6::R6Class(
  'MethodPattern',
  public = list(
    pattern = NULL,

    initialize = function(pattern) {
      self$pattern <- pattern
    },

    matches = function(method) {
      self$pattern == method || self$pattern == "any"
    },

    to_s = function() self$pattern
  )
)

# HeadersPattern
HeadersPattern <- R6::R6Class(
  'HeadersPattern',
  public = list(
    pattern = NULL,

    initialize = function(pattern) {
      self$pattern <- pattern
    },

    matches = function(headers) {
      if (empty_headers(self$pattern)) {
        empty_headers(headers)
      } else {
        if (empty_headers(headers)) return(FALSE)
        out <- c()
        for (i in seq_along(self$pattern)) {
          out[i] <- names(self$pattern)[i] %in% names(headers) &&
            self$pattern[[i]] == headers[names(self$pattern)[i]]
        }
        all(out)
      }
    },

    empty_headers = function(headers) {
      is.null(headers) || length(headers) == 0
    },

    to_s = function() self$pattern
  )
)

# BodyPattern
BodyPattern <- R6::R6Class(
  'BodyPattern',
  public = list(
    pattern = NULL,
    body = NULL,
    content_type = NULL,
    headers = NULL,
    string = NULL,

    initialize = function(pattern) {
      self$pattern <- pattern
    },

    matches = function(body, content_type = "") {
      if (inherits(self$pattern, "list")) {
        if (length(self$pattern) == 0) return(TRUE)
        private$matching_hashes(self$body_as_hash(body, content_type), self$pattern)
      } else {
        private$empty_string(self$pattern) && private$empty_string(body) ||
          self$pattern == body
      }
    },

    to_s = function() self$pattern
  ),

  private = list(

    empty_headers = function(headers) {
      is.null(headers) || length(headers) == 0
    },

    empty_string = function(string) {
      is.null(string) || nchar(string) == 0
    },

    matching_hashes = function(query_parameters, pattern) {
      if (inherits(query_parameters, "list")) return(FALSE)
      if (sort(names(query_parameters)) == sort(names(self$pattern))) return(FALSE)
      for (i in seq_along(query_parameters)) {
        expected <- self$pattern[names(query_parameters)[i]]
        if (inherits(actual, "list") && inherits(expected, "list")) {
          if (private$matching_hashes(actual, expected)) return(FALSE)
        } else {
          if (identical(actual, expected)) return(FALSE)
        }
      }
    },

    body_as_hash = function(body, content_type) {
      bctype <- BODY_FORMATS[[content_type]]
      if (bctype == 'json') {
        jsonlite::fromJSON(body, FALSE)
      } else if (bctype == 'xml') {
        xml2::read_xml(body)
      } else {
        stop('fix me')
      }
    }
  )
)

BODY_FORMATS <- list(
  'text/xml'               = 'xml',
  'application/xml'        = 'xml',
  'application/json'       = 'json',
  'text/json'              = 'json',
  'application/javascript' = 'json',
  'text/javascript'        = 'json',
  'text/html'              = 'html',
  'application/x-yaml'     = 'yaml',
  'text/yaml'              = 'yaml',
  'text/plain'             = 'plain'
)

# UriPattern
UriPattern <- R6::R6Class(
  'UriPattern',
  public = list(
    pattern = NULL,
    query_params = NULL,

    initialize = function(pattern) {
      self$pattern <- normalize_uri(pattern)
    },

    matches = function(uri) {
      # FIXME, may need to match optionally to URI alone or URI + query params, etc.
      uri == self$pattern
    },

    add_query_params = function(query_params) {
      if (inherits(query_params, "list") || inherits(query_params, "character")) {
        self$pattern$query <- "xx"
        self$query_params <- NULL
      }
    },

    to_s = function() self$pattern
  )
)

normalize_uri <- function(x) {
  if (is.na(urltools::url_parse(x)$scheme)) {
    paste0('http://', x)
  } else {
    x
  }
}


# matcher helpers --------------------------
get_method <- function(x) {
  x <- as.character(x)
  tmp <- grep(
    "(get)$|(post)$|(put)$|(delete)$|(options)$|(patch)$|(head)$",
    tolower(x), value = TRUE)
  tmp <- sub("httr::", "", tmp)
  if (length(tmp) == 0) NULL else tmp
}

is_url <- function(x) {
  grepl(
    "https?://", x, ignore.case = TRUE) ||
    grepl("localhost:[0-9]{4}", x, ignore.case = TRUE
    )
}

get_uri <- function(x) {
  x <- as.character(x)
  #tmp <- grep("(https?|ftp|file)?:?(//)?[-A-Za-z0-9]+\\.[A-Za-z0-9]+", x, value = TRUE)
  tmp <- x[vapply(x, is_url, logical(1))]
  if (length(tmp) == 0) NULL else tmp
}

get_host <- function(x) {
  eval(parse(text = vcr_c$uri_parser))(x)$hostname
}

get_path <- function(x) {
  eval(parse(text = vcr_c$uri_parser))(x)$path
}

get_query <- function(x) {
  if ("query" %in% names(x)) {
    x[["query"]]
  } else {
    NULL
  }
}

get_body <- function(x) {
  if ("body" %in% names(x)) {
    x[["body"]]
  } else {
    NULL
  }
}

