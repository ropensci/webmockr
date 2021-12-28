#' @title webmockr
#' @description Stubbing and setting expectations on HTTP requests
#'
#' @importFrom R6 R6Class
#' @importFrom fauxpas HTTPRequestTimeout
#' @importFrom crul mock
#' @importFrom base64enc base64encode
#' @name webmockr-package
#' @aliases webmockr
#' @docType package
#' @keywords package
#' @author Scott Chamberlain \email{myrmecocystus+r@@gmail.com}
#' @author Aaron Wolen
#'
#' @section Features:
#'
#' - Stubbing HTTP requests at low http client lib level
#' - Setting and verifying expectations on HTTP requests
#' - Matching requests based on method, URI, headers and body
#' - Supports multiple HTTP libraries, including \pkg{crul},
#' \pkg{httr}, and \pkg{httr2}
#' - Integration with HTTP test caching library \pkg{vcr}
#'
#' @examples
#' library(webmockr)
#' stub_request("get", "https://httpbin.org/get")
#' stub_request("post", "https://httpbin.org/post")
#' stub_registry()
NULL
