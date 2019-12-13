---
title: "none"
output: html_fragment
---

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
library(webmockr)
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

`webmockr` doesn't do that. Check out [vcr][]
