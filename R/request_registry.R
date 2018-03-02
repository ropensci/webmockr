#' List requests in the request registry
#'
#' @export
#' @return an object of class `RequestRegistry`, print method gives the
#' requests in the registry and the number of times each one has been
#' performed
#' @family request-registry
#' @examples
#' webmockr::enable()
#' stub_request("get", "https://httpbin.org/get") %>%
#'   to_return(body = "success!", status = 200)
#' 
#' # nothing in the request registry
#' request_registry()
#'
#' # make the request
#' z <- crul::HttpClient$new(url = "https://httpbin.org")$get("get")
#'
#' # check the request registry - the request was made 1 time
#' request_registry()
#'
#' # do the request again
#' z <- crul::HttpClient$new(url = "https://httpbin.org")$get("get")
#'
#' # check the request registry - now it's been made 2 times, yay!
#' request_registry()

request_registry <- function() webmockr_request_registry
