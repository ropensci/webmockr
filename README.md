webmockr
========



[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/ropensci/webmockr.svg?branch=master)](https://travis-ci.org/ropensci/webmockr)
[![codecov](https://codecov.io/gh/ropensci/webmockr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/webmockr)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/webmockr)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/webmockr)](https://cran.r-project.org/package=webmockr)


R library for stubbing and setting expectations on HTTP requests.

Port of the Ruby gem [webmock](https://github.com/bblimke/webmock)

<details> <summary><strong>How it works it way too much detail</strong></summary> <p>

The very very short version is: `webmockr` helps you stub HTTP requests so you 
don't have to repeat yourself.

**More details**

You tell `webmockr` what HTTP request you want to match against and if it sees a 
request matching your criteria it doesn't actually do the HTTP request. Instead,
it gives back the same object you would have gotten back with a real request, but 
only with the bits it knows about. For example, we can't give back the actual 
data you'd get from a real HTTP request as the request wasn't performed.

In addition, if you set an expectation of what `webmockr` should return, we 
return that. For example, if you expect a request to return a 418 error 
(I'm a Teapot), then that's what you'll get.

**Scenarios**

There's a few scenarios to think about when using `webmockr`:

* Real HTTP requests allowed:
* Real HTTP requests dis-allowed:
* Certain kinds of real HTTP requests allowed:
* xxxx:
* xxxx:

**Storing actual HTTP responses**

`webmockr` doesn't do that. You can use `vcr` for that.

</p></details>

## Features

* Stubbing HTTP requests at low http client lib level
* Setting and verifying expectations on HTTP requests
* Matching requests based on method, URI, headers and body
* Support for `testthat` coming soon via [vcr](https://github.com/ropenscilabs/vcr)

## Supported HTTP libraries

* [crul](https://github.com/ropensci/crul) - an updated `crul` version is coming 
to CRAN soon, but not up as of today (2017-05-21)

> more to come

## Install

from cran


```r
install.packages("webmockr")
```

Dev version


```r
devtools::install_github("ropensci/webmockr")
```


```r
library(webmockr)
```

## Turn on webmockr


```r
webmockr::enable()
#> CrulAdapter enabled!
#> [1] TRUE
crul::mock()
```

## Outside a test framework


```r
library(crul)
```

### Stubbed request based on uri only and with the default response


```r
stub_request("get", "https://httpbin.org/get")
#> <webmockr stub> 
#>   method: get
#>   uri: https://httpbin.org/get
#>   with: 
#>     query: 
#>     body: 
#>     request_headers: 
#>   to_return: 
#>     status: 
#>     body: 
#>     response_headers:
```


```r
x <- HttpClient$new(url = "https://httpbin.org")
x$get('get')
#> <crul response> 
#>   url: https://httpbin.org/get
#>   request_headers: 
#>     User-Agent: libcurl/7.51.0 r-curl/2.6 crul/0.3.5.9318
#>     Accept-Encoding: gzip, deflate
#>   response_headers: 
#>   status: 200
```

set return objects


```r
stub_request("get", "https://httpbin.org/get") %>%
  wi_th(
    query = list(hello = "world")) %>%
    to_return(status = 418)
#> <webmockr stub> 
#>   method: get
#>   uri: https://httpbin.org/get
#>   with: 
#>     query: hello=world
#>     body: 
#>     request_headers: 
#>   to_return: 
#>     status: 418
#>     body: 
#>     response_headers:
```


```r
x$get('get', query = list(hello = "world"))
#> <crul response> 
#>   url: https://httpbin.org/get?hello=world
#>   request_headers: 
#>     User-Agent: libcurl/7.51.0 r-curl/2.6 crul/0.3.5.9318
#>     Accept-Encoding: gzip, deflate
#>   response_headers: 
#>   params: 
#>     hello: world
#>   status: 418
```

### Stubbing requests based on method, uri and query params


```r
stub_request("get", "https://httpbin.org/get") %>%
  wi_th(query = list(hello = "world"), 
        headers = list('User-Agent' = 'libcurl/7.51.0 r-curl/2.6 crul/0.3.6', 
                       'Accept-Encoding' = "gzip, deflate"))
#> <webmockr stub> 
#>   method: get
#>   uri: https://httpbin.org/get
#>   with: 
#>     query: hello=world
#>     body: 
#>     request_headers: User-Agent=libcurl/7.51.0 r-curl/2.6 crul/0.3.6, Accept-Encoding=gzip, deflate
#>   to_return: 
#>     status: 
#>     body: 
#>     response_headers:
```


```r
stub_registry()
#> <webmockr stub registry> 
#>  Registered Stubs
#>    get: https://httpbin.org/get 
#>    get: https://httpbin.org/get?hello=world   | to_return:   with status 418 
#>    get: https://httpbin.org/get?hello=world   with headers {"User-Agent":"libcurl/7.51.0 r-curl/2.6 crul/0.3.6","Accept-Encoding":"gzip, deflate"} 
#>    get: https://httpbin.org/get 
#>    get: https://httpbin.org/get?hello=world   | to_return:   with status 418 
#>    get: https://httpbin.org/get?hello=world   with headers {"User-Agent":"libcurl/7.51.0 r-curl/2.6 crul/0.3.6","Accept-Encoding":"gzip, deflate"} 
#>    get: https://httpbin.org/get 
#>    get: https://httpbin.org/get?hello=world   | to_return:   with status 418 
#>    get: https://httpbin.org/get?hello=world   with headers {"User-Agent":"libcurl/7.51.0 r-curl/2.6 crul/0.3.6","Accept-Encoding":"gzip, deflate"}
```


```r
x <- HttpClient$new(url = "https://httpbin.org")
x$get('get', query = list(hello = "world"))
#> <crul response> 
#>   url: https://httpbin.org/get?hello=world
#>   request_headers: 
#>     User-Agent: libcurl/7.51.0 r-curl/2.6 crul/0.3.5.9318
#>     Accept-Encoding: gzip, deflate
#>   response_headers: 
#>   params: 
#>     hello: world
#>   status: 418
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/webmockr/issues).
* License: MIT
* Get citation information for `webmockr` in R doing `citation(package = 'webmockr')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
