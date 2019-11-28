#' List requests in the request registry
#'
#' @export
#' @return an object of class `RequestRegistry`, print method gives the
#' requests in the registry and the number of times each one has been
#' performed
#' @family request-registry
#' @details `request_registry()` lists the requests that have been made
#' that webmockr knows about; `request_registry_clear()` resets the 
#' request registry (removes all recorded requests)
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
#' 
#' # clear the request registry
#' request_registry_clear()
request_registry <- function() webmockr_request_registry

#' @export
#' @rdname request_registry
request_registry_clear <- function() webmockr_request_registry$reset()
