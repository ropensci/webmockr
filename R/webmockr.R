#' Stubbing and setting expectations on HTTP requests
#'
#' @importFrom lazyeval lazy_dots all_dots
#' @importFrom R6 R6Class
#' @importFrom fauxpas HTTPRequestTimeout
#' @name webmockr-package
#' @aliases webmockr
#' @docType package
#' @keywords package
#' @author Scott Chamberlain \email{myrmecocystus+r@@gmail.com}
#'
#' @section Features:
#' \itemize{
#'  \item Stubbing HTTP requests at low http client lib level
#'  \item Setting and verifying expectations on HTTP requests
#'  \item Matching requests based on method, URI, headers and body
#'  \item Can support many HTTP libraries, though only \pkg{crul} for now
#'  \item Integration with testing libraries (coming soon) via `vcr`
#' }
#'
#' @examples
#' library(webmockr)
#' stub_request("get", "https://httpbin.org/get")
#' stub_request("post", "https://httpbin.org/post")
#' stub_registry()
NULL
