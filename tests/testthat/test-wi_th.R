context("wi_th")

test_that("wi_th: with just headers", {
  aa <- stub_request("get", "https://httpbin.org/get") %>%
    wi_th(headers = list("User-Agent" = "R"))

  expect_is(aa, "StubbedRequest")
  expect_null(aa$body)
  expect_null(aa$host)
  expect_null(aa$query)
  expect_is(aa$request_headers, "list")
  expect_null(aa$response)
  expect_null(aa$response_headers)
  expect_null(aa$responses_sequences)

  expect_is(aa$method, "character")
  expect_equal(aa$method, "get")
  expect_is(aa$uri, "character")
  expect_equal(aa$uri, "https://httpbin.org/get")
  expect_equal(aa$request_headers, list("User-Agent" = "R"))
})

test_that("wi_th: with headers and query", {
  aa <- stub_request("get", "https://httpbin.org/get") %>%
    wi_th(
      query = list(hello = "world"),
      headers = list("User-Agent" = "R"))

  expect_is(aa$query, "list")
  expect_is(aa$request_headers, "list")

  expect_output(print(aa), "hello=world")
  expect_output(print(aa), "User-Agent=R")
})

test_that("wi_th: bodies", {
  aa <- stub_request("post", "https://httpbin.org/post") %>%
    wi_th(body = list(foo = "bar"))
  expect_is(aa$body, "list")
  expect_output(print(aa), "body \\(class: list\\): foo=bar")

  bb <- stub_request("post", "https://httpbin.org/post") %>%
    wi_th(body = '{"foo": "bar"}')
  expect_is(bb$body, "character")
  expect_output(print(bb),
    "body \\(class: character\\): \\{\"foo\": \"bar\"\\}")

  cc <- stub_request("post", "https://httpbin.org/post") %>%
    wi_th(body = charToRaw('{"foo": "bar"}'))
  expect_is(cc$body, "raw")
  expect_output(print(cc),
    "body \\(class: raw\\): raw bytes, length: 14")

  dd <- stub_request("post", "https://httpbin.org/post") %>%
    wi_th(body = 5)
  expect_is(dd$body, "numeric")
  expect_output(print(dd), "body \\(class: numeric\\): 5")

  ee <- stub_request("post", "https://httpbin.org/post") %>%
    wi_th(body = crul::upload(system.file("CITATION")))
  expect_is(ee$body, "form_file")
  expect_output(print(ee), "body \\(class: form_file\\): crul::upload")

  # FIXME: ideally (maybe?) we have a upload within a list look like 
  # the above when not in a list?
  ff <- stub_request("post", "https://httpbin.org/post") %>%
    wi_th(body = list(y = crul::upload(system.file("CITATION"))))
  expect_is(ff$body, "list")
  expect_is(ff$body$y, "form_file")
  expect_output(print(ff), "body \\(class: list\\): y = list\\(path")
})

test_that("wi_th fails well", {
  expect_error(wi_th(), "argument \".data\" is missing")
  expect_error(wi_th(5), ".data must be of class StubbedRequest")

  zzz <- stub_request("get", "https://httpbin.org/get")

  # query
  expect_error(wi_th(zzz, query = list(5, 6)),
               "'query' must be a named list")
  expect_error(wi_th(zzz, query = list(a = 5, 6)),
               "'query' must be a named list")

  # headers
  expect_error(wi_th(zzz, headers = list(5, 6)),
               "'headers' must be a named list")
  expect_error(wi_th(zzz, headers = list(a = 5, 6)),
               "'headers' must be a named list")

  # only accepts certain set of named things
  expect_error(wi_th(zzz, a = 5),
    "'wi_th' only accepts query, body, headers")
})

test_that("wi_th .list works", {
  req <- stub_request("post", "https://httpbin.org/post")
  expect_equal(
    wi_th(req, .list = list(body = list(foo = "bar"))),
    wi_th(req, body = list(foo = "bar"))
  )
  expect_equal(
    wi_th(req, .list = list(query = list(a = 3445))),
    wi_th(req, query = list(a = 3445))
  )
  expect_equal(wi_th(req, .list = ), wi_th(req))

  expect_error(wi_th(req, .list = 4), ".list must be of class list")
  expect_error(wi_th(req, .list = list(a = 5)),
    "'wi_th' only accepts query, body, headers")
})

# addresses issue: https://github.com/ropensci/webmockr/issues/107
test_that("wi_th handles QUERIES with varied input classes", {
  stub_registry_clear()
  library(httr)
  enable("httr")

  # works w/ numeric
  stub_request("get", "https://google.com") %>%
    wi_th(query = list(per_page = 30))
  expect_is(GET("https://google.com?per_page=30"), "response")

  # works w/ integer
  stub_registry_clear()
  stub_request("get", "https://google.com") %>%
    wi_th(query = list(per_page = 30L))
  expect_is(GET("https://google.com?per_page=30"), "response")

  # works w/ character
  stub_registry_clear()
  stub_request("get", "https://google.com") %>%
    wi_th(query = list(per_page = "30"))
  expect_is(GET("https://google.com?per_page=30"), "response")

  # works w/ number as factor
  stub_registry_clear()
  stub_request("get", "https://google.com") %>%
    wi_th(query = list(per_page = as.factor(30)))
  expect_is(GET("https://google.com?per_page=30"), "response")

  # works w/ character as factor
  stub_registry_clear()
  stub_request("get", "https://google.com") %>%
    wi_th(query = list(cursor = as.factor("ads97as9dfas8dfasfd")))
  expect_is(GET("https://google.com?cursor=ads97as9dfas8dfasfd"), "response")

  # works w/ AsIs
  stub_registry_clear()
  stub_request("get", "https://google.com") %>%
    wi_th(query = list(per_page = I(30)))
  expect_is(GET("https://google.com?per_page=30"), "response")
})

test_that("wi_th handles HEADERS with varied input classes", {
  stub_registry_clear()
  library(httr)
  enable("httr")

  # works w/ numeric
  stub_request("get", "https://x.com") %>%
    wi_th(headers = list(foo = 30))
  expect_is(GET("https://x.com", add_headers(foo=30)), "response")

  # works w/ integer
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(headers = list(foo = 30L))
  expect_is(GET("https://x.com", add_headers(foo=30)), "response")

  # works w/ character
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(headers = list(foo = "30"))
  expect_is(GET("https://x.com", add_headers(foo=30)), "response")

  # works w/ number as factor
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(headers = list(foo = as.factor(30)))
  expect_is(GET("https://x.com", add_headers(foo=30)), "response")

  # works w/ character as factor
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(headers = list(foo = as.factor("bar")))
  expect_is(GET("https://x.com", add_headers(foo="bar")), "response")

  # works w/ AsIs
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(headers = list(foo = 30))
  expect_is(GET("https://x.com", add_headers(foo=30)), "response")
})

disable("httr")

test_that("wi_th basic_auth", {
  # crul
  library(crul)
  enable("crul")
  con <- HttpClient$new("https://x.com", auth = auth("user", "passwd"))
  # pass
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(basic_auth=c("user", "passwd"))
  expect_is(con$get(), "HttpResponse")
  # ignores auth type
  con$auth <- crul::auth("user", "passwd", "digest")
  expect_is(con$get(), "HttpResponse")
  # fail
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(basic_auth=c("user", "passwd"))
  con$auth <- crul::auth("user", "password")
  expect_error(con$get(), "Unregistered")
  disable("crul")

  # httr
  library(httr)
  enable("httr")
  # pass
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(basic_auth=c("user", "passwd"))
  expect_is(GET("https://x.com", authenticate("user", "passwd")), "response")
  # ignores auth type
  expect_is(
    GET("https://x.com", authenticate("user", "passwd", type = "digest")),
    "response")
  expect_is(
    GET("https://x.com", authenticate("user", "passwd", type = "ntlm")),
    "response")
  # fail
  stub_registry_clear()
  stub_request("get", "https://x.com") %>%
    wi_th(basic_auth=c("user", "passwd"))
  expect_error(GET("https://x.com", authenticate("user", "password")),
    "Unregistered")
  disable("httr")
})

# cleanup
stub_registry_clear()

context("wi_th_: defunct")
test_that("wi_th_: defunct", {
  expect_error(wi_th_(), "wi_th", class = "error")
})
