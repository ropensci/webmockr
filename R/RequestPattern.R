#' @title RequestPattern class
#' @description Class handling all request matchers
#' @keywords internal
#' @seealso pattern classes for HTTP method [MethodPattern], headers
#' [HeadersPattern], body [BodyPattern], and URI/URL [UriPattern]
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
    #' @param basic_auth (list) vector of length 2 (username, password),
    #' optional
    #' @return A new `RequestPattern` object
    initialize = function(
      method,
      uri = NULL,
      uri_regex = NULL,
      query = NULL,
      body = NULL,
      headers = NULL,
      basic_auth = NULL
    ) {
      if (is.null(uri) && is.null(uri_regex)) {
        abort("one of uri or uri_regex is required")
      }

      self$method_pattern <- MethodPattern$new(pattern = method)
      self$uri_pattern <- if (is.null(uri_regex)) {
        UriPattern$new(pattern = uri)
      } else {
        UriPattern$new(regex_pattern = uri_regex)
      }
      self$uri_pattern$add_query_params(query)
      self$body_pattern <- if (!is.null(body)) BodyPattern$new(pattern = body)
      auth_headers <- private$set_basic_auth_as_headers(basic_auth)
      headers <- c(headers, auth_headers)
      self$headers_pattern <- if (!is.null(headers)) {
        HeadersPattern$new(pattern = headers)
      }
    },

    #' @description does a request signature match the selected matchers?
    #' @param request_signature a [RequestSignature] object
    #' @return a boolean
    matches = function(request_signature) {
      assert_is(request_signature, "RequestSignature")
      c_type <- NULL
      c_type <- if (!is.null(request_signature$headers)) {
        request_signature$headers$`Content-Type`
      }
      if (!is.null(c_type)) {
        c_type <- strsplit(c_type, ";")[[1]][1]
      }
      self$method_pattern$matches(request_signature$method) &&
        self$uri_pattern$matches(request_signature$uri) &&
        (is.null(self$body_pattern) ||
          self$body_pattern$matches(request_signature$body, c_type %||% "")) &&
        (is.null(self$headers_pattern) ||
          self$headers_pattern$matches(request_signature$headers))
    },

    #' @description Print pattern for easy human consumption
    #' @return a string
    to_s = function() {
      gsub(
        "^\\s+|\\s+$",
        "",
        paste(
          toupper(self$method_pattern$to_s()),
          self$uri_pattern$to_s(),
          if (!is.null(self$body_pattern)) {
            if (!is.null(self$body_pattern$pattern)) {
              paste0(" with body ", self$body_pattern$to_s())
            }
          },
          if (!is.null(self$headers_pattern)) {
            paste0(" with headers ", self$headers_pattern$to_s())
          }
        )
      )
    }
  ),
  private = list(
    set_basic_auth_as_headers = function(x) {
      if (!is_null(x)) {
        private$validate_basic_auth(x)
        list(
          Authorization = private$make_basic_auth(x[1], x[2])
        )
      }
    },
    validate_basic_auth = function(x) {
      if (!inherits(x, "character") || length(unique(unname(unlist(x)))) == 1) {
        abort(c(
          "error in basic auth",
          "'basic_auth' option should be a length 2 vector"
        ))
      }
    },
    make_basic_auth = function(x, y) {
      paste0("Basic ", jsonlite::base64_enc(paste0(x, ":", y)))
    }
  )
)

#' @title MethodPattern
#' @description method matcher
#' @keywords internal
#' @details Matches regardless of case. e.g., POST will match to post
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
#' @keywords internal
#' @details
#' `webmockr` normalises headers and treats all forms of same headers as equal:
#' i.e the following two sets of headers are equal:
#' `list(Header1 = "value1", content_length = 123, X_CuStOm_hEAder = "foo")`
#' and
#' `list(header1 = "value1", "Content-Length" = 123, "x-cuSTOM-HeAder" = "foo")`
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
        self_pattern_for_matching <- headers_flatten(self$pattern)
        headers <- headers_flatten(headers)

        out <- c()
        for (i in seq_along(self_pattern_for_matching)) {
          out[i] <- names(self_pattern_for_matching)[i] %in%
            names(headers) &&
            self_pattern_for_matching[[i]] ==
              headers[[names(self_pattern_for_matching)[i]]]
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
      # underscores to single dash
      names(x) <- gsub("_", "-", names(x))
      return(x)
    }
  )
)

#' @importFrom jsonlite fromJSON
seems_like_json <- function(x) {
  res <- tryCatch(jsonlite::fromJSON(x), error = function(msg) msg)
  !inherits(res, "error")
}

#' @title BodyPattern
#' @description body matcher
#' @keywords internal
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
    #' @param pattern (list) a body object - from a request stub (i.e.,
    #' the mock)
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

      # convert self$pattern to a list if it's json
      if (seems_like_json(self$pattern)) {
        self$pattern <- jsonlite::fromJSON(self$pattern, FALSE)
      }
    },

    #' @importFrom rlang is_null is_na
    #' @description Match a request body pattern against a pattern
    #' @param body (list) the body, i.e., from the HTTP request
    #' @param content_type (character) content type
    #' @return a boolean
    matches = function(body, content_type = "") {
      if (inherits(self$pattern, "list")) {
        if (length(self$pattern) == 0) {
          return(TRUE)
        }
        private$matching_hashes(
          self$pattern,
          private$body_as_hash(body, content_type)
        )
      } else {
        # FIXME: add partial approach later
        (private$empty_string(self$pattern) && private$empty_string(body)) ||
          {
            if (xor(is_na(self$pattern), is_na(body))) {
              return(FALSE)
            }
            if (xor(is_null(self$pattern), is_null(body))) {
              return(FALSE)
            }
            all(self$pattern == body)
          }
      }
    },

    #' @description Print pattern for easy human consumption
    #' @return a string
    to_s = function() self$pattern
  ),
  private = list(
    empty_string = function(string) {
      is_null(string) || !nzchar(string)
    },
    matching_hashes = function(pattern, body) {
      if (is_null(pattern)) {
        return(FALSE)
      }
      if (!inherits(pattern, "list")) {
        return(FALSE)
      }
      if (!rlang::is_list(body)) {
        return(FALSE)
      }

      pattern_char <- rapply(pattern, as.character, how = "replace")
      body_char <- rapply(body, as.character, how = "replace")

      if (self$partial) {
        names_values_check <- switch(
          self$partial_type,
          # unname() here not needed for R < 4.5, but is needed for R 4.5
          # because intersect changes to output unnamed lists
          include = identical(
            unname(intersect(pattern_char, body_char)),
            unname(pattern_char)
          ),
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
      if (inherits(body, "form_file")) {
        body <- unclass(body)
      }
      if (is_empty(content_type)) {
        content_type <- ""
      }
      bctype <- BODY_FORMATS[[content_type]] %||% ""
      if (grepl("json", content_type)) {
        bctype <- "json"
      }
      if (bctype == "json") {
        jsonlite::fromJSON(body, FALSE)
      } else if (bctype == "xml") {
        check_installed("xml2")
        try_xml2list <- rlang::try_fetch(
          {
            body_xml <- xml2::read_xml(body)
            xml_as_list <- xml2::as_list(body_xml)
            lapply(xml_as_list, promote_attr)
          },
          error = function(e) e
        )
        if (rlang::is_error(try_xml2list)) {
          rlang::warn(
            "xml to list conversion failed; using xml string for comparison",
            use_cli_format = TRUE,
            .frequency = "always"
          )
          body
        } else {
          try_xml2list
        }
      } else {
        if (seems_like_json(body)) {
          return(jsonlite::fromJSON(body, FALSE))
        }
        query_mapper(body)
      }
    }
  )
)

BODY_FORMATS <- list(
  "text/xml" = "xml",
  "application/xml" = "xml",
  "application/json" = "json",
  "text/json" = "json",
  "application/javascript" = "json",
  "text/javascript" = "json",
  "application/x-amz-json-1.1" = "json", # AWS
  "text/html" = "html",
  "application/x-yaml" = "yaml",
  "text/yaml" = "yaml",
  "text/plain" = "plain"
)

# remove_reserved & promote_attr from
# https://www.garrickadenbuie.com/blog/recursive-xml-workout/
remove_reserved <- function(this_attr) {
  reserved_attr <- c(
    "class",
    "comment",
    "dim",
    "dimnames",
    "names",
    "row.names",
    "tsp"
  )
  if (!any(reserved_attr %in% names(this_attr))) {
    return(this_attr)
  }
  for (reserved in reserved_attr) {
    if (!is.null(this_attr[[reserved]])) this_attr[[reserved]] <- NULL
  }
  this_attr
}
promote_attr <- function(ll) {
  this_attr <- attributes(ll)
  this_attr <- remove_reserved(this_attr)
  if (length(ll)) {
    # recursive case
    c(this_attr, lapply(ll, promote_attr))
  } else {
    # base case (no sub-items)
    this_attr
  }
}

#' @title UriPattern
#' @description uri matcher
#' @keywords internal
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
      if (!is.null(regex_pattern)) {
        self$regex <- TRUE
      }
      pattern <- if (!is.null(pattern)) pattern else regex_pattern
      if (self$regex) {
        pattern <- add_scheme(pattern)
      }
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
        out <- switch(
          self$partial_type,
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
          pars <- paste0(
            unname(Map(
              function(x, y) paste(x, esc(y), sep = "="),
              names(query_params),
              query_params
            )),
            collapse = "&"
          )
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
  if (!regex) {
    tmp$path <- esc(tmp$path)
  }
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
        strsplit(tmp$parameter, "&")[[1]],
        function(x) {
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

# adapted from httr2:::headers_flatten
# headers_flatten(req$headers)
headers_flatten <- function(x) {
  is_redacted <- wm_is_redacted(x)
  out <- vector("list", length(x))
  names(out) <- names(x)
  out[!is_redacted] <- lapply(x[!is_redacted], paste, collapse = ",")
  out[is_redacted] <- lapply(x[is_redacted], rlang::wref_value)
  out[is_redacted] <- lapply(out[is_redacted], function(x) {
    if (!is.null(x)) {
      paste(x, collapse = ",")
    }
  })
  Filter(length, out)
}

wm_is_redacted <- function(x) {
  if (rlang::is_weakref(x)) {
    return(TRUE)
  }
  if (is.list(x)) {
    vapply(x, rlang::is_weakref, logical(1))
  } else {
    rlang::is_weakref(x)
  }
}
