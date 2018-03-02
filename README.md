webmockr
========



[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/ropensci/webmockr.svg?branch=master)](https://travis-ci.org/ropensci/webmockr)
[![codecov](https://codecov.io/gh/ropensci/webmockr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/webmockr)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/webmockr)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/webmockr)](https://cran.r-project.org/package=webmockr)


R library for stubbing and setting expectations on HTTP requests.

Port of the Ruby gem [webmock](https://github.com/bblimke/webmock)

<details> <summary><strong>How it works in way too much detail</strong></summary> <p>

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

**What you can match against**

* HTTP method (required)

Plus any single or combination of the following:

* URI
    * Right now, we can match directly against URI's, and with regex URI patterns. 
  Eventually, we will support RFC 6570 URI templates. 
    * We normalize URI paths so that URL encoded things match 
  URL un-encoded things (e.g. `hello world` to `hello%20world`)
* Query parameters
    * We normalize query parameter values so that URL encoded things match 
  URL un-encoded things (e.g. `message = hello world` to 
  `message = hello%20world`)
* Request headers
    * We normalize headers and treat all forms of same headers as equal. For 
  example, the following two sets of headers are equal:
        * `list(H1 = "value1", content_length = 123, X_CuStOm_hEAder = "foo")`
        * `list(h1 = "value1", "Content-Length" = 123, "x-cuSTOM-HeAder" = "foo")`
* Request body

**Real HTTP requests**

There's a few scenarios to think about when using `webmockr`:

After doing

```r
libraray(webmockr)
```

`webmockr` is loaded but not turned on. At this point `webmockr` doesn't 
change anythning.

Once you turn on `webmockr` like 

```r
webmockr::enable()
```

`webmockr` will now by default not allow real HTTP requests from the http 
libraries that adapters are loaded for (right now only `crul`).

You can optionally allow real requests via `webmockr_allow_net_connect()`, and
disallow real requests via `webmockr_disable_net_connect()`. You can check 
whether you are allowing real requests with `webmockr_net_connect_allowed()`.

Certain kinds of real HTTP requests allowed: We don't suppoprt this yet, 
but you can allow localhost HTTP requests with the `allow_localhost` parameter
in the `webmockr_configure()` function. 

**Storing actual HTTP responses**

`webmockr` doesn't do that. Check out [vcr](https://github.com/ropensci/vcr) for that.

</p></details>

## Features

* Stubbing HTTP requests at low http client lib level
* Setting and verifying expectations on HTTP requests
* Matching requests based on method, URI, headers and body
* Support for `testthat` coming soon via [vcr](https://github.com/ropenscilabs/vcr)
* Can be used for testing or outside of a testing context

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

## Enable webmockr


```r
webmockr::enable()
#> CrulAdapter enabled!
```

## Inside a test framework


```r
library(crul)
library(testthat)

# make a stub
stub_request("get", "https://httpbin.org/get") %>%
   to_return(body = "success!", status = 200)
#> <webmockr stub> 
#>   method: get
#>   uri: https://httpbin.org/get
#>   with: 
#>     query: 
#>     body: 
#>     request_headers: 
#>   to_return: 
#>     status: 200
#>     body: success!
#>     response_headers: 
#>   should_timeout: FALSE
#>   should_raise: FALSE

# check that it's in the stub registry
stub_registry()
#> <webmockr stub registry> 
#>  Registered Stubs
#>    get: https://httpbin.org/get   | to_return:  with body "success!"  with status 200

# make the request
z <- crul::HttpClient$new(url = "https://httpbin.org")$get("get")

# run tests (nothing returned means it passed)
expect_is(z, "HttpResponse")
expect_equal(z$status_code, 200)
expect_equal(z$parse("UTF-8"), "success!")
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
#>   should_timeout: FALSE
#>   should_raise: FALSE
```


```r
x <- HttpClient$new(url = "https://httpbin.org")
x$get('get')
#> <crul response> 
#>   url: https://httpbin.org/get
#>   request_headers: 
#>     User-Agent: libcurl/7.54.0 r-curl/3.1 crul/0.5.2
#>     Accept-Encoding: gzip, deflate
#>     Accept: application/json, text/xml, application/xml, */*
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
#>   should_timeout: FALSE
#>   should_raise: FALSE
```


```r
x$get('get', query = list(hello = "world"))
#> <crul response> 
#>   url: https://httpbin.org/get?hello=world
#>   request_headers: 
#>     User-Agent: libcurl/7.54.0 r-curl/3.1 crul/0.5.2
#>     Accept-Encoding: gzip, deflate
#>     Accept: application/json, text/xml, application/xml, */*
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
#>   should_timeout: FALSE
#>   should_raise: FALSE
```


```r
stub_registry()
#> <webmockr stub registry> 
#>  Registered Stubs
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
#>     User-Agent: libcurl/7.54.0 r-curl/3.1 crul/0.5.2
#>     Accept-Encoding: gzip, deflate
#>     Accept: application/json, text/xml, application/xml, */*
#>   response_headers: 
#>   params: 
#>     hello: world
#>   status: 418
```

### Stubbing requests and set expectation of a timeout


```r
stub_request("post", "https://httpbin.org/post") %>% to_timeout()
#> <webmockr stub> 
#>   method: post
#>   uri: https://httpbin.org/post
#>   with: 
#>     query: 
#>     body: 
#>     request_headers: 
#>   to_return: 
#>     status: 
#>     body: 
#>     response_headers: 
#>   should_timeout: TRUE
#>   should_raise: FALSE
x <- HttpClient$new(url = "https://httpbin.org")
x$post('post')
#> Error: Request Timeout (HTTP 408).
#>  - The client did not produce a request within the time that the server was prepared to wait. The client MAY repeat the request without modifications at any later time.
```

### Stubbing requests and set HTTP error expectation


```r
library(fauxpas)
stub_request("get", "https://httpbin.org/get?a=b") %>% to_raise(HTTPBadRequest)
#> <webmockr stub> 
#>   method: get
#>   uri: https://httpbin.org/get?a=b
#>   with: 
#>     query: 
#>     body: 
#>     request_headers: 
#>   to_return: 
#>     status: 
#>     body: 
#>     response_headers: 
#>   should_timeout: FALSE
#>   should_raise: HTTPBadRequest
x <- HttpClient$new(url = "https://httpbin.org")
x$get('get', query = list(a = "b"))
#> Error: Bad Request (HTTP 400).
#>  - The request could not be understood by the server due to malformed syntax. The client SHOULD NOT repeat the request without modifications.
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/webmockr/issues).
* License: MIT
* Get citation information for `webmockr` in R doing `citation(package = 'webmockr')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
