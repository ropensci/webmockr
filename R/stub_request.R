#' Stub an http request
#'
#' @export
#' @param verb curl verb, one of get, post, put, patch, head, delete.
#' @param url Base url for the stub reqeust
#' @param where Where to store stubbed request. Only tests/cache/ for now.
#' @examples \donttest{
#' stub_request("get", url="http://google.com/")
#' GET("http://google.com/")
#' }

stub_request <- function(verb="get", url){
  verb <- match.arg(verb, c("get","post","put","patch","head","delete"))
#   hash <- cache_request()
  out <- list(hash = "999", verb = verb, body = NULL, header = NULL,
              uri = url, location = 'tests/cache/')
  structure(out, class = "http_stub")
}

#' @export
print.http_stub <- function(x, ...){
  cat(paste0("<http stub> ", x$hash), sep = "\n")
  cat(paste0("  verb: ", x$verb), sep = "\n")
  cat(paste0("  uri: ", x$uri), sep = "\n")
  cat(paste0("  body: ", x$body), sep = "\n")
  cat(paste0("  header: ", x$header), sep = "\n")
  cat(paste0("  location: ", x$location), sep = "\n")
}

# pry
# require 'webmock'
# include WebMock::API
# stub_request(:any, "api.plos.org/search").with(:query => { :q => "*:*", :wt => "json", :fl => "id" })

# require 'httparty'
# stub_request(:get, "http://api.plos.org/search?fl=id,journal&q=*:*&wt=json").
#   to_return(:status => 200, :body => "", :headers => {})
# HTTParty.get("http://api.plos.org/search?q=*:*&wt=json&fl=id,journal")

# http://api.plos.org/search?q=*:*&wt=json&fl=id,journal
