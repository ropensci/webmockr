#' @section Features:
#'
#' - Stubbing HTTP requests at low http client lib level
#' - Setting and verifying expectations on HTTP requests
#' - Matching requests based on method, URI, headers and body
#' - Supports multiple HTTP libraries, including \pkg{crul},
#' \pkg{httr}, and \pkg{httr2}
#' - Integration with HTTP test caching library \pkg{vcr}
#' - Supports async http request mocking with \pkg{crul} only
#'
#' @examples
#' library(webmockr)
#' stub_request("get", "https://httpbin.org/get")
#' stub_request("post", "https://httpbin.org/post")
#' stub_registry()
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom R6 R6Class
#' @importFrom fauxpas HTTPRequestTimeout
#' @importFrom crul mock
#' @importFrom rlang abort warn check_installed is_list is_function is_error
#' caller_arg try_fetch caller_env
#' @importFrom cli cli_abort ansi_collapse format_error
## usethis namespace: end
NULL
