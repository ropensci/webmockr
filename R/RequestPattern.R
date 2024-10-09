#' @title RequestPattern class
#' @description class handling all request matchers
#' @export
#' @seealso pattern classes for HTTP method [MethodPattern], headers
#' [HeadersPattern], body [BodyPattern], and URI/URL [UriPattern]
#' @examples \dontrun{
#' (x <- RequestPattern$new(method = "get", uri = "httpbin.org/get"))
#' x$body_pattern
#' x$headers_pattern
#' x$method_pattern
#' x$uri_pattern
#' x$to_s()
#'
#' # make a request signature
#' rs <- RequestSignature$new(method = "get", uri = "http://httpbin.org/get")
#'
#' # check if it matches
#' x$matches(rs)
#'
#' # regex uri
#' (x <- RequestPattern$new(method = "get", uri_regex = ".+ossref.org"))
#' x$uri_pattern
#' x$uri_pattern$to_s()
#' x$to_s()
#'
#' # uri with query parameters
#' (x <- RequestPattern$new(
#'   method = "get", uri = "https://httpbin.org/get",
#'   query = list(foo = "bar")
#' ))
#' x$to_s()
#' ## query params included in url, not separately
#' (x <- RequestPattern$new(
#'   method = "get", uri = "https://httpbin.org/get?stuff=things"
#' ))
#' x$to_s()
#' x$query_params
#'
#' # just headers (via setting method=any & uri_regex=.+)
#' headers <- list(
#'   "User-Agent" = "Apple",
#'   "Accept-Encoding" = "gzip, deflate",
#'   "Accept" = "application/json, text/xml, application/xml, */*"
#' )
#' x <- RequestPattern$new(
#'   method = "any",
#'   uri_regex = ".+",
#'   headers = headers
#' )
#' x$to_s()
#' rs <- RequestSignature$new(
#'   method = "any", uri = "http://foo.bar",
#'   options = list(headers = headers)
#' )
#' rs
#' x$matches(rs)
#'
#' # body
#' x <- RequestPattern$new(
#'   method = "post", uri = "httpbin.org/post",
#'   body = list(y = crul::upload(system.file("CITATION")))
#' )
#' x$to_s()
#' rs <- RequestSignature$new(
#'   method = "post", uri = "http://httpbin.org/post",
#'   options = list(
#'     body = list(y = crul::upload(system.file("CITATION")))
#'   )
#' )
#' rs
#' x$matches(rs)
#' }
RequestPattern <- R6::R6Class(
  "RequestPattern",
  public = list(
    #' @field method_pattern xxx
    method_pattern = NULL,
    #' @field uri_pattern xxx
    uri_pattern = NULL,
    #' @field body_pattern xxx
    body_pattern = NULL,
    #' @field headers_pattern xxx
    headers_pattern = NULL,

    #' @description Create a new `RequestPattern` object
    #' @param method the HTTP method (any, head, options, get, post, put,
    #' patch, trace, or delete). "any" matches any HTTP method. required.
    #' @param uri (character) request URI. required or uri_regex
    #' @param uri_regex (character) request URI as regex. required or uri
    #' @param query (list) query parameters, optional
    #' @param body (list) body request, optional
    #' @param headers (list) headers, optional
    #' @return A new `RequestPattern` object
    initialize = function(method, uri = NULL, uri_regex = NULL,
                          query = NULL, body = NULL, headers = NULL) {
      if (is.null(uri) && is.null(uri_regex)) {
        stop("one of uri or uri_regex is required", call. = FALSE)
      }

      self$method_pattern <- MethodPattern$new(pattern = method)
      self$uri_pattern <- if (is.null(uri_regex)) {
        UriPattern$new(pattern = uri)
      } else {
        UriPattern$new(regex_pattern = uri_regex)
      }
      self$uri_pattern$add_query_params(query)
      self$body_pattern <- if (!is.null(body)) BodyPattern$new(pattern = body)
      self$headers_pattern <- if (!is.null(headers)) {
        HeadersPattern$new(pattern = headers)
      }
      # FIXME: all private methods used in the below line, see if needed or remove
      # if (length(options)) private$assign_options(options)
    },

    #' @description does a request signature match the selected matchers?
    #' @param request_signature a [RequestSignature] object
    #' @return a boolean
    matches = function(request_signature) {
      assert(request_signature, "RequestSignature")
      c_type <- if (!is.null(request_signature$headers)) request_signature$headers$`Content-Type` else NULL
      if (!is.null(c_type)) c_type <- strsplit(c_type, ";")[[1]][1]
      self$method_pattern$matches(request_signature$method) &&
        self$uri_pattern$matches(request_signature$uri) &&
        (is.null(self$body_pattern) || self$body_pattern$matches(request_signature$body, c_type %||% "")) &&
        (is.null(self$headers_pattern) || self$headers_pattern$matches(request_signature$headers))
    },

    #' @description Print pattern for easy human consumption
    #' @return a string
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
    # assign_options = function(options) {
    #   #self$validate_keys(options, 'body', 'headers', 'query', 'basic_auth')
    #   set_basic_auth_as_headers(options)
    #   self$body_pattern <- if ('body' %in% names(options)) BodyPattern$new(options['body'])
    #   self$headers_pattern <- if ('headers' %in% names(options)) HeadersPattern$new(options['headers'])
    #   if ('query' %in% names(options)) self$uri_pattern$add_query_params(options['query'])
    # },

    # validate_keys = function(x, ...) {
    #   valid_keys <- unlist(list(...), recursive = FALSE)
    #   for (i in seq_along(x)) {
    #     if (!names(x)[i] %in% valid_keys) {
    #       stop(
    #         sprintf("Unknown key: %s. Valid keys are: %s",
    #                 names(x)[i],
    #                 paste0(valid_keys, collapse = ", "),
    #                 call. = FALSE
    #         )
    #       )
    #     }
    #   }
    # },
    set_basic_auth_as_headers = function(options) {
      if ("basic_auth" %in% names(options)) {
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

#' @title MethodPattern
#' @description method matcher
#' @export
#' @keywords internal
#' @details Matches regardless of case. e.g., POST will match to post
#' @examples
#' (x <- MethodPattern$new(pattern = "post"))
#' x$pattern
#' x$matches(method = "post")
#' x$matches(method = "POST")
#'
#' # all matches() calls should be TRUE
#' (x <- MethodPattern$new(pattern = "any"))
#' x$pattern
#' x$matches(method = "post")
#' x$matches(method = "GET")
#' x$matches(method = "HEAD")
MethodPattern <- R6::R6Class(
  "MethodPattern",
  public = list(
    #' @field pattern (character) an http method
    pattern = NULL,

    #' @description Create a new `MethodPattern` object
    #' @param pattern (character) a HTTP method, lowercase
    #' @return A new `MethodPattern` object
    initialize = function(pattern) {
      self$pattern <- tolower(pattern)
    },

    #' @description test if the pattern matches a given http method
    #' @param method (character) a HTTP method, lowercase
    #' @return a boolean
    matches = function(method) {
      self$pattern == tolower(method) || self$pattern == "any"
    },

    #' @description Print pattern for easy human consumption
    #' @return a string
    to_s = function() self$pattern
  )
)

#' @title HeadersPattern
#' @description headers matcher
#' @export
#' @keywords internal
#' @details
#' `webmockr` normalises headers and treats all forms of same headers as equal:
#' i.e the following two sets of headers are equal:
#' `list(Header1 = "value1", content_length = 123, X_CuStOm_hEAder = "foo")`
#' and
#' `list(header1 = "value1", "Content-Length" = 123, "x-cuSTOM-HeAder" = "foo")`
#' @examples
#' (x <- HeadersPattern$new(pattern = list(a = 5)))
#' x$pattern
#' x$matches(list(a = 5))
#'
#' # different cases
#' (x <- HeadersPattern$new(pattern = list(Header1 = "value1")))
#' x$pattern
#' x$matches(list(header1 = "value1"))
#' x$matches(list(header1 = "value2"))
#'
#' # different symbols
#' (x <- HeadersPattern$new(pattern = list(`Hello_World` = "yep")))
#' x$pattern
#' x$matches(list(`hello-world` = "yep"))
#' x$matches(list(`hello-worlds` = "yep"))
#'
#' headers <- list(
#'   "User-Agent" = "Apple",
#'   "Accept-Encoding" = "gzip, deflate",
#'   "Accept" = "application/json, text/xml, application/xml, */*"
#' )
#' (x <- HeadersPattern$new(pattern = headers))
#' x$to_s()
#' x$pattern
#' x$matches(headers)
HeadersPattern <- R6::R6Class(
  "HeadersPattern",
  public = list(
    #' @field pattern a list
    pattern = NULL,

    #' @description Create a new `HeadersPattern` object
    #' @param pattern (list) a pattern, as a named list, must be named,
    #' e.g,. `list(a = 5, b = 6)`
    #' @return A new `HeadersPattern` object
    initialize = function(pattern) {
      stopifnot(is.list(pattern))
      pattern <- private$normalize_headers(pattern)
      self$pattern <- pattern
    },

    #' @description Match a list of headers against that stored
    #' @param headers (list) named list of headers, e.g,. `list(a = 5, b = 6)`
    #' @return a boolean
    matches = function(headers) {
      if (self$empty_headers(self$pattern)) {
        self$empty_headers(headers)
      } else {
        if (self$empty_headers(headers)) {
          return(FALSE)
        }
        headers <- private$normalize_headers(headers)
        out <- c()
        for (i in seq_along(self$pattern)) {
          out[i] <- names(self$pattern)[i] %in% names(headers) &&
            self$pattern[[i]] == headers[[names(self$pattern)[i]]]
        }
        all(out)
      }
    },

    #' @description Are headers empty? tests if null or length==0
    #' @param headers named list of headers
    #' @return a boolean
    empty_headers = function(headers) {
      is.null(headers) || length(headers) == 0
    },

    #' @description Print pattern for easy human consumption
    #' @return a string
    to_s = function() hdl_lst2(self$pattern)
  ),
  private = list(
    normalize_headers = function(x) {
      # normalize names
      names(x) <- tolower(names(x))
      # normalize symbols
      ## underscores to single dash
      names(x) <- gsub("_", "-", names(x))
      return(x)
    }
  )
)

#' @title BodyPattern
#' @description body matcher
#' @export
#' @keywords internal
#' @examples
#' # make a request signature
#' bb <- RequestSignature$new(
#'   method = "get",
#'   uri = "https:/httpbin.org/get",
#'   options = list(
#'     body = list(foo = "bar", a = 5)
#'   )
#' )
#'
#' # make body pattern object
#' ## FALSE
#' z <- BodyPattern$new(pattern = list(foo = "bar"))
#' z$pattern
#' z$matches(bb$body)
#' ## TRUE
#' z <- BodyPattern$new(pattern = list(foo = "bar", a = 5))
#' z$pattern
#' z$matches(bb$body)
#'
#' # uploads in bodies
#' ## upload NOT in a list
#' bb <- RequestSignature$new(
#'   method = "post", uri = "https:/httpbin.org/post",
#'   options = list(body = crul::upload(system.file("CITATION")))
#' )
#' bb$body
#' z <- BodyPattern$new(
#'   pattern =
#'     crul::upload(system.file("CITATION"))
#' )
#' z$pattern
#' z$matches(bb$body)
#'
#' ## upload in a list
#' bb <- RequestSignature$new(
#'   method = "post", uri = "https:/httpbin.org/post",
#'   options = list(body = list(y = crul::upload(system.file("CITATION"))))
#' )
#' bb$body
#' z <- BodyPattern$new(
#'   pattern =
#'     list(y = crul::upload(system.file("CITATION")))
#' )
#' z$pattern
#' z$matches(bb$body)
#'
#' # partial matching
#' ## including
#' partial_incl <- including(list(foo = "bar"))
#' z <- BodyPattern$new(pattern = partial_incl)
#' z$pattern
#' z$matches(list(foo = "bar", a = 5)) # TRUE
#'
#' ## excluding
#' partial_excl <- excluding(list(hello = "world"))
#' z <- BodyPattern$new(pattern = partial_excl)
#' z$pattern
#' z$matches(list(a = 5)) # TRUE
#' z$matches(list(hello = "mars", a = 5)) # TRUE
#' z$matches(list(hello = "world")) # FALSE
BodyPattern <- R6::R6Class(
  "BodyPattern",
  public = list(
    #' @field pattern a list
    pattern = NULL,
     #' @field partial bool, default: `FALSE`
    partial = FALSE,
    #' @field partial_type a string, default: NULL
    partial_type = NULL,

    #' @description Create a new `BodyPattern` object
    #' @param pattern (list) a body object
    #' @return A new `BodyPattern` object
    initialize = function(pattern) {
      if (inherits(pattern, "partial")) {
        self$partial <- attr(pattern, "partial_match") %||% FALSE
        self$partial_type <- attr(pattern, "partial_type")
        pattern <- drop_partial_attrs(pattern)
        self$pattern <- unclass(pattern)
      } else if (inherits(pattern, "form_file")) {
        self$pattern <- unclass(pattern)
      } else {
        self$pattern <- pattern
      }
    },

    #' @description Match a request body pattern against a pattern
    #' @param body (list) the body
    #' @param content_type (character) content type
    #' @return a boolean
    matches = function(body, content_type = "") {
      if (inherits(self$pattern, "list")) {
        if (length(self$pattern) == 0) {
          return(TRUE)
        }
        private$matching_hashes(self$pattern, private$body_as_hash(body, content_type))
      } else {
        # FIXME: add partial approach later
        (private$empty_string(self$pattern) && private$empty_string(body)) || all(self$pattern == body)
      }
    },

    #' @description Print pattern for easy human consumption
    #' @return a string
    to_s = function() self$pattern
  ),
  private = list(
    empty_string = function(string) {
      is.null(string) || !nzchar(string)
    },
    matching_hashes = function(pattern, body) {
      if (is.null(pattern)) {
        return(FALSE)
      }
      if (!inherits(pattern, "list")) {
        return(FALSE)
      }

      pattern_char <- rapply(pattern, as.character, how = "replace")
      body_char <- rapply(body, as.character, how = "replace")
      if (self$partial) {
        names_values_check <- switch(self$partial_type,
          include = identical(intersect(pattern_char, body_char), pattern_char),
          exclude = length(intersect(pattern_char, body_char)) == 0
        )
        if (!names_values_check) {
          return(FALSE)
        }
      } else {
        if (!identical(pattern_char, body_char)) {
          return(FALSE)
        }
      }

      # return TRUE (a match) if no FALSE's returned above
      return(TRUE)
    },
    body_as_hash = function(body, content_type) {
      if (inherits(body, "form_file")) body <- unclass(body)
      bctype <- BODY_FORMATS[[content_type]] %||% ""
      if (bctype == "json") {
        jsonlite::fromJSON(body, FALSE)
      } else if (bctype == "xml") {
        check_for_pkg("xml2")
        xml2::read_xml(body)
      } else {
        query_mapper(body)
      }
    }
  )
)

BODY_FORMATS <- list(
  "text/xml"               = "xml",
  "application/xml"        = "xml",
  "application/json"       = "json",
  "text/json"              = "json",
  "application/javascript" = "json",
  "text/javascript"        = "json",
  "text/html"              = "html",
  "application/x-yaml"     = "yaml",
  "text/yaml"              = "yaml",
  "text/plain"             = "plain"
)

#' @title UriPattern
#' @description uri matcher
#' @export
#' @keywords internal
#' @examples
#' # trailing slash
#' (z <- UriPattern$new(pattern = "http://foobar.com"))
#' z$matches("http://foobar.com") # TRUE
#' z$matches("http://foobar.com/") # TRUE
#'
#' # without scheme
#' ## matches http by default: does not match https by default
#' (z <- UriPattern$new(pattern = "foobar.com"))
#' z$matches("http://foobar.com") # TRUE
#' z$matches("http://foobar.com/") # TRUE
#' z$matches("https://foobar.com") # FALSE
#' z$matches("https://foobar.com/") # FALSE
#' ## to match https, you'll have to give the complete url
#' (z <- UriPattern$new(pattern = "https://foobar.com"))
#' z$matches("https://foobar.com/") # TRUE
#' z$matches("http://foobar.com/") # FALSE
#'
#' # default ports
#' (z <- UriPattern$new(pattern = "http://foobar.com"))
#' z$matches("http://foobar.com:80") # TRUE
#' z$matches("http://foobar.com:80/") # TRUE
#' z$matches("http://foobar.com:443") # TRUE
#' z$matches("http://foobar.com:443/") # TRUE
#'
#' # user info - FIXME, not sure we support this yet
#' (z <- UriPattern$new(pattern = "http://foobar.com"))
#' z$matches("http://user:pass@foobar.com")
#'
#' # regex
#' (z <- UriPattern$new(regex_pattern = ".+ample\\.."))
#' z$matches("http://sample.org") # TRUE
#' z$matches("http://example.com") # TRUE
#' z$matches("http://tramples.net") # FALSE
#'
#' # add query parameters
#' (z <- UriPattern$new(pattern = "http://foobar.com"))
#' z$add_query_params(list(pizza = "cheese", cheese = "cheddar"))
#' z
#' z$pattern
#' z$matches("http://foobar.com?pizza=cheese&cheese=cheddar") # TRUE
#' z$matches("http://foobar.com?pizza=cheese&cheese=swiss") # FALSE
#'
#' # query parameters in the uri
#' (z <- UriPattern$new(pattern = "https://httpbin.org/get?stuff=things"))
#' z$add_query_params() # have to run this method to gather query params
#' z$matches("https://httpbin.org/get?stuff=things") # TRUE
#' z$matches("https://httpbin.org/get?stuff2=things") # FALSE
#'
#' # regex add query parameters
#' (z <- UriPattern$new(regex_pattern = "https://foobar.com/.+/order"))
#' z$add_query_params(list(pizza = "cheese"))
#' z
#' z$pattern
#' z$matches("https://foobar.com/pizzas/order?pizza=cheese") # TRUE
#' z$matches("https://foobar.com/pizzas?pizza=cheese") # FALSE
#'
#' # query parameters in the regex uri
#' (z <- UriPattern$new(regex_pattern = "https://x.com/.+/order\\?fruit=apple"))
#' z$add_query_params() # have to run this method to gather query params
#' z$matches("https://x.com/a/order?fruit=apple") # TRUE
#' z$matches("https://x.com/a?fruit=apple") # FALSE
#'
#' # any pattern
#' (z <- UriPattern$new(regex_pattern = "stuff\\.com.+"))
#' z$regex
#' z$pattern
#' z$matches("http://stuff.com") # FALSE
#' z$matches("https://stuff.com/stff") # TRUE
#' z$matches("https://stuff.com/apple?bears=brown&bats=grey") # TRUE
#'
#' # partial matching
#' ## including
#' z <- UriPattern$new(pattern = "http://foobar.com")
#' z$add_query_params(including(list(hello = "world")))
#' z$matches(uri = "http://foobar.com?hello=world&bye=mars") # TRUE
#' z$matches("http://foobar.com?bye=mars") # FALSE
#'
#' ## excluding
#' z <- UriPattern$new(pattern = "http://foobar.com")
#' z$add_query_params(excluding(list(hello = "world")))
#' z$matches(uri = "http://foobar.com?hello=world&bye=mars") # FALSE
#' z$matches("http://foobar.com?bye=mars") # TRUE
#'
#' ## match on list keys (aka: names) only, ignore values 0
#' z <- UriPattern$new(pattern = "http://foobar.com")
#' z$add_query_params(including(list(hello = NULL)))
#' z$matches(uri = "http://foobar.com?hello=world&bye=mars") # TRUE
#' z$matches("http://foobar.com?hello=stuff") # TRUE
#' z$matches("http://foobar.com?bye=stuff") # FALSE
UriPattern <- R6::R6Class(
  "UriPattern",
  public = list(
    #' @field pattern (character) pattern holder
    pattern = NULL,
    #' @field regex a logical
    regex = FALSE,
    #' @field query_params a list, or `NULL` if empty
    query_params = NULL,
    #' @field partial bool, default: `FALSE`
    partial = FALSE,
    #' @field partial_type a string, default: NULL
    partial_type = NULL,

    #' @description Create a new `UriPattern` object
    #' @param pattern (character) a uri, as a character string. if scheme
    #' is missing, it is added (we assume http)
    #' @param regex_pattern (character) a uri as a regex character string,
    #' see [base::regex]. if scheme is missing, it is added (we assume
    #' http)
    #' @return A new `UriPattern` object
    initialize = function(pattern = NULL, regex_pattern = NULL) {
      stopifnot(xor(is.null(pattern), is.null(regex_pattern)))
      if (!is.null(regex_pattern)) self$regex <- TRUE
      pattern <- if (!is.null(pattern)) pattern else regex_pattern
      if (self$regex) pattern <- add_scheme(pattern)
      self$pattern <- normalize_uri(pattern, self$regex)
    },

    #' @description Match a uri against a pattern
    #' @param uri (character) a uri
    #' @return a boolean
    matches = function(uri) {
      uri <- normalize_uri(uri, self$regex)
      if (self$regex) {
        grepl(self$pattern, uri)
      } else {
        self$pattern_matches(uri) && self$query_params_matches(uri)
      }
    },

    #' @description Match a URI
    #' @param uri (character) a uri
    #' @return a boolean
    pattern_matches = function(uri) {
      if (!self$regex) {
        return(just_uri(uri) == just_uri(self$pattern))
      } # not regex
      grepl(drop_query_params(self$pattern), just_uri(uri)) # regex
    },

    #' @importFrom rlang is_empty
    #' @description Match query parameters of a URI
    #' @param uri (character) a uri
    #' @return a boolean
    query_params_matches = function(uri) {
      if (self$partial) {
        uri_qp <- self$extract_query(uri)
        qp <- drop_partial_attrs(self$query_params)

        bools <- vector(mode = "logical")
        for (i in seq_along(qp)) {
          if (rlang::is_empty(qp[[i]])) {
            bools[i] <- names(qp) %in% names(uri_qp)
          } else {
            bools[i] <- qp %in% uri_qp
          }
        }
        out <- switch(self$partial_type,
          include = any(bools),
          exclude = !any(bools)
        )
        return(out)
      }
      identical(self$query_params, self$extract_query(uri))
    },

    #' @description Extract query parameters as a named list
    #' @param uri (character) a uri
    #' @return named list, or `NULL` if no query parameters
    extract_query = function(uri) {
      params <- parse_a_url(uri)$parameter
      if (all(is.na(params))) {
        return(NULL)
      }
      params
    },

    #' @description Add query parameters to the URI
    #' @param query_params (list|character) list or character
    #' @return nothing returned, updates uri pattern
    add_query_params = function(query_params) {
      if (self$regex) {
        return(NULL)
      }
      if (missing(query_params) || is.null(query_params)) {
        self$query_params <- self$extract_query(self$pattern)
      } else {
        self$query_params <- query_params
        self$partial <- attr(query_params, "partial_match") %||% FALSE
        self$partial_type <- attr(query_params, "partial_type")
        if (
          inherits(query_params, "list") ||
            inherits(query_params, "character")
        ) {
          pars <- paste0(unname(Map(
            function(x, y) paste(x, esc(y), sep = "="),
            names(query_params), query_params
          )), collapse = "&")
          self$pattern <- paste0(self$pattern, "?", pars)
        }
      }
    },

    #' @description Print pattern for easy human consumption
    #' @return a string
    to_s = function() self$pattern
  )
)

drop_partial_attrs <- function(x) {
  attr(x, "partial_match") <- NULL
  attr(x, "partial_type") <- NULL
  return(x)
}

add_scheme <- function(x) {
  if (is.na(urltools::url_parse(x)$scheme)) {
    paste0("https?://", x)
  } else {
    x
  }
}
esc <- function(x) curl::curl_escape(x)
normalize_uri <- function(x, regex = FALSE) {
  x <- prune_trailing_slash(x)
  x <- prune_port(x)
  if (!regex) {
    if (is.na(urltools::url_parse(x)$scheme)) {
      x <- paste0("http://", x)
    }
  }
  tmp <- urltools::url_parse(x)
  if (is.na(tmp$path)) {
    return(x)
  }
  if (!regex) tmp$path <- esc(tmp$path)
  urltools::url_compose(tmp)
}

prune_trailing_slash <- function(x) sub("/$", "", x)

prune_port <- function(x) gsub("(:80)|(:443)", "", x)

# matcher helpers --------------------------
## URI stuff
is_url <- function(x) {
  grepl("https?://", x, ignore.case = TRUE) ||
    grepl("localhost:[0-9]{4}", x, ignore.case = TRUE)
}

is_localhost <- function(x) {
  grepl("localhost|127.0.0.1|0.0.0.0", x, ignore.case = TRUE)
}

parse_a_url <- function(url) {
  tmp <- urltools::url_parse(url)
  tmp <- as.list(tmp)
  if (!is.na(tmp$parameter)) {
    tmp$parameter <- unlist(
      lapply(
        strsplit(tmp$parameter, "&")[[1]], function(x) {
          z <- strsplit(x, split = "=")[[1]]
          as.list(stats::setNames(z[2], z[1]))
        }
      ),
      recursive = FALSE
    )
  }
  tmp$default_port <- 443
  return(tmp)
}

just_uri <- function(x) {
  z <- urltools::url_parse(x)
  z$parameter <- NA_character_
  urltools::url_compose(z)
}

uri_fetch <- function(x) {
  x <- as.character(x)
  tmp <- x[vapply(x, FUN = is_url, FUN.VALUE = logical(1))]
  if (length(tmp) == 0) NULL else tmp
}
uri_host <- function(x) parse_a_url(x)$domain
uri_path <- function(x) parse_a_url(x)$path
uri_port <- function(x) parse_a_url(x)$port

drop_query_params <- function(x) {
  x <- urltools::url_parse(x)
  x$parameter <- NA_character_
  x <- urltools::url_compose(x)
  # prune trailing slash
  sub("\\/$", "", x)
}
