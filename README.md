webmockr
========



[![Build Status](https://travis-ci.org/ropensci/webmockr.svg?branch=master)](https://travis-ci.org/ropensci/webmockr)
[![codecov](https://codecov.io/gh/ropensci/webmockr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/webmockr)

R library for stubbing and setting expectations on HTTP requests.

Port of the Ruby gem [webmock](https://github.com/bblimke/webmock)


## Features

* Stubbing HTTP requests at low http client lib level
* Setting and verifying expectations on HTTP requests
* Matching requests based on method, URI, headers and body
* Support for `testthat` coming soon via [vcr](https://github.com/ropenscilabs/vcr)

## Supported HTTP libraries

* [crul](https://github.com/ropensci/crul)

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
stub_request("any", "https://httpbin.org/get")
#> <webmockr stub> 
#>   method: any
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
#> $url
#> $url$url
#> [1] "https://httpbin.org/get"
#> 
#> $url$handle
#> <curl handle> (empty)
#> 
#> 
#> $method
#> [1] "get"
#> 
#> $options
#> $options$httpget
#> [1] TRUE
#> 
#> 
#> $headers
#> $headers$`User-Agent`
#> [1] "libcurl/7.51.0 r-curl/2.6 crul/0.3.5.9313"
#> 
#> $headers$`Accept-Encoding`
#> [1] "gzip, deflate"
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
#> $url
#> $url$url
#> [1] "https://httpbin.org/get?hello=world"
#> 
#> $url$handle
#> <curl handle> (empty)
#> 
#> 
#> $method
#> [1] "get"
#> 
#> $options
#> $options$httpget
#> [1] TRUE
#> 
#> 
#> $headers
#> $headers$`User-Agent`
#> [1] "libcurl/7.51.0 r-curl/2.6 crul/0.3.5.9313"
#> 
#> $headers$`Accept-Encoding`
#> [1] "gzip, deflate"
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
#>    any: https://httpbin.org/get 
#>    get: https://httpbin.org/get?hello=world   | to_return:   with status 418 
#>    get: https://httpbin.org/get?hello=world   with headers {"User-Agent":"libcurl/7.51.0 r-curl/2.6 crul/0.3.6","Accept-Encoding":"gzip, deflate"}
```


```r
x <- HttpClient$new(url = "https://httpbin.org")
x$get('get', query = list(hello = "world"))
#> $url
#> $url$url
#> [1] "https://httpbin.org/get?hello=world"
#> 
#> $url$handle
#> <curl handle> (empty)
#> 
#> 
#> $method
#> [1] "get"
#> 
#> $options
#> $options$httpget
#> [1] TRUE
#> 
#> 
#> $headers
#> $headers$`User-Agent`
#> [1] "libcurl/7.51.0 r-curl/2.6 crul/0.3.5.9313"
#> 
#> $headers$`Accept-Encoding`
#> [1] "gzip, deflate"
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/webmockr/issues).
* License: MIT
* Get citation information for `webmockr` in R doing `citation(package = 'webmockr')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
