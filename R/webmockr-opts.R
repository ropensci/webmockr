#' webmockr configuration
#'
#' @export
#' @param turn_on (logical) Default: `FALSE`
#' @param allow_net_connect (logical) Default: `TRUE`
#' @param allow_localhost  (logical) Default: `TRUE`
#' @param allow (logical) Default: `TRUE`
#' @param net_http_connect_on_start (logical) Default: `TRUE`
#' @param show_stubbing_instructions (logical) Default: `TRUE`
#' @param query_values_notation (logical) Default: `TRUE`
#' @param show_body_diff (logical) Default: `TRUE`
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
#' webmockr_disable_net_connect()
#' webmockr_net_connect_allowed()
#' }
webmockr_configure <- function(
  turn_on = FALSE,
  allow_net_connect = FALSE,
  allow_localhost = FALSE,
  allow = FALSE,
  net_http_connect_on_start = FALSE,
  show_stubbing_instructions = FALSE,
  query_values_notation = FALSE,
  show_body_diff = FALSE) {

  opts <- list(
    turn_on = turn_on,
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
webmockr_enable <- function() {
  message("webmockr enabled")
  assign('turn_on', TRUE, envir = webmockr_conf_env)
}

#' @export
#' @rdname webmockr_configure
webmockr_disable <- function() {
  message("webmockr disabled")
  assign('turn_on', FALSE, envir = webmockr_conf_env)
}

#' @export
#' @rdname webmockr_configure
webmockr_allow_net_connect <- function() {
  message("net connect allowed")
  assign('allow_net_connect', TRUE, envir = webmockr_conf_env)
}

#' @export
#' @rdname webmockr_configure
webmockr_disable_net_connect <- function() {
  message("net connect disabled")
  assign('allow_net_connect', FALSE, envir = webmockr_conf_env)
}

#' @export
#' @rdname webmockr_configure
webmockr_net_connect_allowed <- function() {
  webmockr_conf_env$allow_net_connect
}



print.webmockr_config <- function(x, ...) {
  cat("<webmockr configuration>", sep = "\n")
  cat(paste0("  enabled?: ", x$turn_on), sep = "\n")
  cat(paste0("  allow_net_collect?: ", x$allow_net_collect), sep = "\n")
  cat(paste0("  allow_localhost?: ", x$allow_localhost), sep = "\n")
  cat(paste0("  allow: ", x$allow), sep = "\n")
  cat(paste0("  net_http_connect_on_start: ", x$net_http_connect_on_start),
      sep = "\n")
  cat(paste0("  show_stubbing_instructions: ", x$show_stubbing_instructions),
      sep = "\n")
  cat(paste0("  query_values_notation: ", x$query_values_notation), sep = "\n")
  cat(paste0("  show_body_diff: ", x$show_body_diff), sep = "\n")
}

webmockr_conf_env <- new.env()
