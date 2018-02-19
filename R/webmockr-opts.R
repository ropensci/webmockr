#' webmockr configuration
#'
#' @export
#' @param allow_net_connect (logical) Default: `TRUE`
#' @param allow_localhost  (logical) Default: `TRUE`
#' @param allow (character) one or more URI/URL to allow (and by extension
#' all others are not allowed)
#' @param net_http_connect_on_start (logical) Default: `TRUE`
#' @param show_stubbing_instructions (logical) Default: `TRUE`
#' @param query_values_notation (logical) Default: `TRUE`
#' @param show_body_diff (logical) Default: `TRUE`
#' @param uri (character) a URI/URL as a character string - to determine
#' whether or not it is allowed
#'
#' @examples \dontrun{
#' webmockr_configure()
#' webmockr_configure(
#'  allow_localhost = TRUE
#' )
#' webmockr_configuration()
#' webmockr_configure_reset()
#'
#' webmockr_allow_net_connect()
#' webmockr_net_connect_allowed()
#'
#' # disable net connect for any URIs
#' webmockr_disable_net_connect()
#' ### gives NULL with no URI passed
#' webmockr_net_connect_allowed()
#' # disable net connect EXCEPT FOR given URIs
#' webmockr_disable_net_connect(allow = "google.com")
#' ### is a specific URI allowed?
#' webmockr_net_connect_allowed("google.com")
#' }
webmockr_configure <- function(
  allow_net_connect = FALSE,
  allow_localhost = FALSE,
  allow = NULL,
  net_http_connect_on_start = FALSE,
  show_stubbing_instructions = FALSE,
  query_values_notation = FALSE,
  show_body_diff = FALSE) {

  opts <- list(
    allow_net_connect = allow_net_connect,
    allow_localhost = allow_localhost,
    allow = allow,
    net_http_connect_on_start = net_http_connect_on_start,
    show_stubbing_instructions = show_stubbing_instructions,
    query_values_notation = query_values_notation,
    show_body_diff = show_body_diff
  )
  for (i in seq_along(opts)) {
    assign(names(opts)[i], opts[[i]], envir = webmockr_conf_env)
  }
  webmockr_configuration()
}

#' @export
#' @rdname webmockr_configure
webmockr_configure_reset <- function() webmockr_configure()

#' @export
#' @rdname webmockr_configure
webmockr_configuration <- function() {
  structure(as.list(webmockr_conf_env), class = "webmockr_config")
}

#' @export
#' @rdname webmockr_configure
webmockr_allow_net_connect <- function() {
  message("net connect allowed")
  assign('allow_net_connect', TRUE, envir = webmockr_conf_env)
}

#' @export
#' @rdname webmockr_configure
webmockr_disable_net_connect <- function(allow = NULL) {
  assert(allow, "character")
  message("net connect disabled")
  assign('allow_net_connect', FALSE, envir = webmockr_conf_env)
  assign('allow', allow, envir = webmockr_conf_env)
}

#' @export
#' @rdname webmockr_configure
webmockr_net_connect_allowed <- function(uri = NULL) {
  assert(uri, c("character", "list"))
  if (is.null(uri)) return(webmockr_conf_env$allow_net_connect)
  uri <- normalize_uri(uri)
  webmockr_conf_env$allow_net_connect ||
    (webmockr_conf_env$allow_localhost && is_localhost(uri) ||
       `!!`(webmockr_conf_env$allow) &&
       net_connect_explicit_allowed(webmockr_conf_env$allow, uri))
}

net_connect_explicit_allowed <- function(allowed, uri = NULL) {
  if (is.null(allowed)) return(FALSE)
  if (is.null(uri)) return(FALSE)
  z <- parse_a_url(uri)
  if (is.na(z$domain)) return(FALSE)
  if (inherits(allowed, "list")) {
    any(vapply(allowed, net_connect_explicit_allowed, logical(1), uri = uri))
  } else if (inherits(allowed, "character")) {
    if (length(allowed) == 1) {
      allowed == uri ||
        allowed == z$domain ||
        allowed == sprintf("%s:%s", z$domain, z$port) ||
        allowed == sprintf("%s://%s:%s", z$scheme, z$domain, z$port) ||
        allowed == sprintf("%s://%s", z$scheme, z$domain) &&
        z$port == z$default_port
    } else {
      any(vapply(allowed, net_connect_explicit_allowed, logical(1), uri = uri))
    }
  }
}

#' @export
print.webmockr_config <- function(x, ...) {
  cat("<webmockr configuration>", sep = "\n")
  cat(paste0("  enabled?: ", webmockr_lightswitch$crul), sep = "\n")
  cat(paste0("  allow_net_collect?: ", x$allow_net_collect), sep = "\n")
  cat(paste0("  allow_localhost?: ", x$allow_localhost), sep = "\n")
  cat(paste0("  allow: ", x$allow %||% ""), sep = "\n")
  cat(paste0("  net_http_connect_on_start: ", x$net_http_connect_on_start),
      sep = "\n")
  cat(paste0("  show_stubbing_instructions: ", x$show_stubbing_instructions),
      sep = "\n")
  cat(paste0("  query_values_notation: ", x$query_values_notation), sep = "\n")
  cat(paste0("  show_body_diff: ", x$show_body_diff), sep = "\n")
}

webmockr_conf_env <- new.env()
