#' List stubs in the stub registry
#'
#' @export
#' @return an object of class `StubRegistry`, print method gives the
#' stubs in the registry
#' @family stub-registry
#' @examples
#' # make a stub
#' stub_request("get", "https://httpbin.org/get") %>%
#'   to_return(body = "success!", status = 200)
#'
#' # check the stub registry, there should be one in there
#' stub_registry()
#'
#' # make another stub
#' stub_request("get", "https://httpbin.org/get") %>%
#'   to_return(body = "woopsy", status = 404)
#'
#' # check the stub registry, now there are two there
#' stub_registry()
#'
#' # to clear the stub registry
#' stub_registry_clear()
stub_registry <- function() webmockr_stub_registry
