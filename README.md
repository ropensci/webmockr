webmockr
========



<!-- NOTE: run `make readme` to generate the README.md -->

[![cran checks](https://cranchecks.info/badges/worst/webmockr)](https://cranchecks.info/pkgs/webmockr)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/ropensci/webmockr.svg?branch=master)](https://travis-ci.org/ropensci/webmockr)
[![Build status](https://ci.appveyor.com/api/projects/status/47scc0vur41sbfyx?svg=true)](https://ci.appveyor.com/project/sckott/webmockr)
[![codecov](https://codecov.io/gh/ropensci/webmockr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/webmockr)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/webmockr)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/webmockr)](https://cran.r-project.org/package=webmockr)


R library for stubbing and setting expectations on HTTP requests.

Port of the Ruby gem [webmock](https://github.com/bblimke/webmock)

<details> <summary><strong>How it works in detail</strong></summary> <p>





<p>The very very short version is: <code>webmockr</code> helps you stub HTTP requests so you don’t have to repeat yourself.</p>
<p><strong>More details</strong></p>
<p>You tell <code>webmockr</code> what HTTP request you want to match against and if it sees a request matching your criteria it doesn’t actually do the HTTP request. Instead, it gives back the same object you would have gotten back with a real request, but only with the bits it knows about. For example, we can’t give back the actual data you’d get from a real HTTP request as the request wasn’t performed.</p>
<p>In addition, if you set an expectation of what <code>webmockr</code> should return, we return that. For example, if you expect a request to return a 418 error (I’m a Teapot), then that’s what you’ll get.</p>
<p><strong>What you can match against</strong></p>
<ul>
<li>HTTP method (required)</li>
</ul>
<p>Plus any single or combination of the following:</p>
<ul>
<li>URI
<ul>
<li>Right now, we can match directly against URI’s, and with regex URI patterns. Eventually, we will support RFC 6570 URI templates.</li>
<li>We normalize URI paths so that URL encoded things match URL un-encoded things (e.g. <code>hello world</code> to <code>hello%20world</code>)</li>
</ul></li>
<li>Query parameters
<ul>
<li>We normalize query parameter values so that URL encoded things match URL un-encoded things (e.g. <code>message = hello world</code> to <code>message = hello%20world</code>)</li>
</ul></li>
<li>Request headers
<ul>
<li>We normalize headers and treat all forms of same headers as equal. For example, the following two sets of headers are equal:
<ul>
<li><code>list(H1 = &quot;value1&quot;, content_length = 123, X_CuStOm_hEAder = &quot;foo&quot;)</code></li>
<li><code>list(h1 = &quot;value1&quot;, &quot;Content-Length&quot; = 123, &quot;x-cuSTOM-HeAder&quot; = &quot;foo&quot;)</code></li>
</ul></li>
</ul></li>
<li>Request body</li>
</ul>
<p><strong>Real HTTP requests</strong></p>
<p>There’s a few scenarios to think about when using <code>webmockr</code>:</p>
<p>After doing</p>
<pre class="r"><code>library(webmockr)</code></pre>
<p><code>webmockr</code> is loaded but not turned on. At this point <code>webmockr</code> doesn’t change anythning.</p>
<p>Once you turn on <code>webmockr</code> like</p>
<pre class="r"><code>webmockr::enable()</code></pre>
<p><code>webmockr</code> will now by default not allow real HTTP requests from the http libraries that adapters are loaded for (right now only <code>crul</code>).</p>
<p>You can optionally allow real requests via <code>webmockr_allow_net_connect()</code>, and disallow real requests via <code>webmockr_disable_net_connect()</code>. You can check whether you are allowing real requests with <code>webmockr_net_connect_allowed()</code>.</p>
<p>Certain kinds of real HTTP requests allowed: We don’t suppoprt this yet, but you can allow localhost HTTP requests with the <code>allow_localhost</code> parameter in the <code>webmockr_configure()</code> function.</p>
<p><strong>Storing actual HTTP responses</strong></p>
<p><code>webmockr</code> doesn’t do that. Check out <a href="https://github.com/ropensci/vcr">vcr</a></p>

</p></details>

## Features

* Stubbing HTTP requests at low http client lib level
* Setting and verifying expectations on HTTP requests
* Matching requests based on method, URI, headers and body
* Support for `testthat` via [vcr][]
* Can be used for testing or outside of a testing context

## Supported HTTP libraries

* [crul](https://github.com/ropensci/crul)
* [httr](https://github.com/r-lib/httr)

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
#> HttrAdapter enabled!
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
#>    GET: https://httpbin.org/get   | to_return:  with body "success!"  with status 200

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
#>     User-Agent: libcurl/7.54.0 r-curl/4.3 crul/0.9.0
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
#>     User-Agent: libcurl/7.54.0 r-curl/4.3 crul/0.9.0
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
#>     request_headers: User-Agent=libcurl/7.51.0 r-cur..., Accept-Encoding=gzip, deflate
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
#>    GET: https://httpbin.org/get 
#>    GET: https://httpbin.org/get?hello=world   | to_return:   with status 418 
#>    GET: https://httpbin.org/get?hello=world   with headers {"User-Agent":"libcurl/7.51.0 r-curl/2.6 crul/0.3.6","Accept-Encoding":"gzip, deflate"}
```


```r
x <- HttpClient$new(url = "https://httpbin.org")
x$get('get', query = list(hello = "world"))
#> <crul response> 
#>   url: https://httpbin.org/get?hello=world
#>   request_headers: 
#>     User-Agent: libcurl/7.54.0 r-curl/4.3 crul/0.9.0
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

## httr integration


```r
library(webmockr)
library(httr)
#> 
#> Attaching package: 'httr'
#> The following object is masked from 'package:crul':
#> 
#>     handle

# turn on httr mocking
httr_mock()
```


```r
# no stub found
GET("https://httpbin.org/get")
#> Error: Real HTTP connections are disabled.
#> Unregistered request:
#>   GET https://httpbin.org/get   with headers {Accept: application/json, text/xml, application/xml, */*}
#> 
#> You can stub this request with the following snippet:
#> 
#>    stub_request('get', uri = 'https://httpbin.org/get') %>%
#>      wi_th(
#>        headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
#>      )
#> ============================================================
```

make a stub


```r
stub_request('get', uri = 'https://httpbin.org/get') %>%
  wi_th(
    headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
  ) %>%
  to_return(status = 418, body = "I'm a teapot!!!", headers = list(im_a = "teapot"))
#> <webmockr stub> 
#>   method: get
#>   uri: https://httpbin.org/get
#>   with: 
#>     query: 
#>     body: 
#>     request_headers: Accept=application/json, te...
#>   to_return: 
#>     status: 418
#>     body: I'm a teapot!!!
#>     response_headers: im_a=teapot
#>   should_timeout: FALSE
#>   should_raise: FALSE
```

now returns mocked response



```r
(res <- GET("https://httpbin.org/get"))
res$status_code
#> [1] 418
res$headers
#> $im_a
#> [1] "teapot"
```

## Writing to disk

Write to a file before mocked request




```r
## make a temp file
f <- tempfile(fileext = ".json")
## write something to the file
cat("{\"hello\":\"world\"}\n", file = f)
readLines(f)
#> [1] "{\"hello\":\"world\"}"
## make the stub
invisible(stub_request("get", "https://httpbin.org/get") %>% 
  to_return(body = file(f)))
## make a request
out <- HttpClient$new("https://httpbin.org/get")$get(disk = f)
readLines(file(f))
#> [1] "{\"hello\":\"world\"}"
```

OR - you can use `mock_file()` to have `webmockr` handle file and contents


```r
g <- tempfile(fileext = ".json")
## make the stub
invisible(stub_request("get", "https://httpbin.org/get") %>% 
  to_return(body = mock_file(g, "{\"hello\":\"mars\"}\n")))
## make a request
out <- crul::HttpClient$new("https://httpbin.org/get")$get(disk = g)
readLines(out$content)
#> [1] "{\"hello\":\"world\"}"
```

Writing to disk is supported in both `crul` and `httr`

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/webmockr/issues).
* License: MIT
* Get citation information for `webmockr` in R doing `citation(package = 'webmockr')`
* Please note that this project is released with a [Contributor Code of Conduct][coc].
By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)


[vcr]: https://github.com/ropensci/vcr
[coc]: https://github.com/ropensci/webmockr/blob/master/CODE_OF_CONDUCT.md
