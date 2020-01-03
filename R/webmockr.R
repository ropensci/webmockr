#' Stubbing and setting expectations on HTTP requests
#'
#' @importFrom R6 R6Class
#' @importFrom fauxpas HTTPRequestTimeout
#' @importFrom crul mock
#' @name webmockr-package
#' @aliases webmockr
#' @docType package
#' @keywords package
#' @author Scott Chamberlain \email{myrmecocystus+r@@gmail.com}
#'
#' @section Features:
#'
#' - Stubbing HTTP requests at low http client lib level
#' - Setting and verifying expectations on HTTP requests
#' - Matching requests based on method, URI, headers and body
#' - Supports multiple HTTP libraries, including \pkg{crul} and
#' \pkg{httr}
#' - Integration with HTTP test caching library \pkg{vcr}
#'
#' @examples
#' library(webmockr)
#' stub_request("get", "https://httpbin.org/get")
#' stub_request("post", "https://httpbin.org/post")
#' stub_registry()
NULL
