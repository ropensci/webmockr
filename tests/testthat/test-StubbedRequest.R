context("StubbedRequest")

test_that("StubbedRequest: works", {
  expect_is(StubbedRequest, "R6ClassGenerator")

  aa <- StubbedRequest$new(method = "get", uri = "https://httpbin.org/get")

  expect_is(aa, "StubbedRequest")

  expect_null(aa$host)
  expect_null(aa$query)
  expect_null(aa$body)
  expect_null(aa$request_headers)
  expect_null(aa$response_headers)
  expect_null(aa$response)
  expect_null(aa$response_sequences)

  expect_is(aa$method, "character")
  expect_equal(aa$method, "get")

  expect_is(aa$uri, "character")
  expect_equal(aa$uri, "https://httpbin.org/get")

  expect_is(aa$uri_parts, "list")
  expect_equal(aa$uri_parts$domain, "httpbin.org")
  expect_equal(aa$uri_parts$path, "get")

  expect_is(aa$to_s, "function")
  expect_equal(aa$to_s(), "GET: https://httpbin.org/get")

  # with
  expect_is(aa$with, "function")
  expect_null(aa$query)
  aa$with(query = list(foo = "bar"))
  expect_is(aa$query, "list")
  expect_named(aa$query, "foo")
  expect_equal(aa$to_s(), "GET: https://httpbin.org/get  with query params foo=bar")

  ## >1 query param gets combined with "&" and not ","
  aa$with(query = list(foo = "bar", stuff = 567))
  expect_equal(sort(names(aa$query)), c("foo", "stuff"))
  expect_equal(aa$to_s(), "GET: https://httpbin.org/get  with query params foo=bar, stuff=567")

  # to_return
  expect_is(aa$to_return, "function")
  expect_null(aa$body)
  aa$to_return(
    status = 404,
    body = list(hello = "world"),
    headers = list(a = 5)
  )
  expect_is(aa$responses_sequences, "list")
  expect_is(aa$responses_sequences[[1]]$body, "list")
  expect_named(aa$responses_sequences[[1]]$body, "hello")
})


test_that("StubbedRequest: to_timeout", {
  x <- StubbedRequest$new(method = "get", uri = "https://httpbin.org/get")
  expect_false(grepl("should_timeout: TRUE", x$to_s()))
  x$to_timeout()
  expect_true(grepl("should_timeout: TRUE", x$to_s()))
})

library("fauxpas")
test_that("StubbedRequest: to_raise", {
  x <- StubbedRequest$new(method = "get", uri = "https://httpbin.org/get")
  expect_false(grepl("to_raise: HTTPBadGateway", x$to_s()))
  x$to_raise(HTTPBadGateway)
  expect_true(grepl("to_raise: HTTPBadGateway", x$to_s()))

  ## many exceptions
  x$to_raise(list(HTTPBadGateway, HTTPForbidden, HTTPInsufficientStorage))
  expect_true(
    grepl("to_raise: HTTPBadGateway, HTTPForbidden, HTTPInsufficientStorage",
      x$to_s()))
})

test_that("StubbedRequest: different methods work", {
  expect_equal(
    StubbedRequest$new(method = "any",
      uri = "https:/httpbin.org/get")$method,
    "any"
  )
  expect_equal(
    StubbedRequest$new(method = "get",
      uri = "https:/httpbin.org/get")$method,
    "get"
  )
  expect_equal(
    StubbedRequest$new(method = "head",
      uri = "https:/httpbin.org/get")$method,
    "head"
  )
  expect_equal(
    StubbedRequest$new(method = "post",
      uri = "https:/httpbin.org/get")$method,
    "post"
  )
  expect_equal(
    StubbedRequest$new(method = "put",
      uri = "https:/httpbin.org/get")$method,
    "put"
  )
  expect_equal(
    StubbedRequest$new(method = "patch",
      uri = "https:/httpbin.org/get")$method,
    "patch"
  )
  expect_equal(
    StubbedRequest$new(method = "delete",
      uri = "https:/httpbin.org/get")$method,
    "delete"
  )
})

test_that("StubbedRequest fails well", {
  # requires uri or uri_regex
  expect_error(StubbedRequest$new(), "one of uri or uri_regex is required")

  # method not in acceptable set
  expect_error(StubbedRequest$new(method = "adf"),
               "'arg' should be one of")
})

test_that("StubbedRequest long string handling", {
  x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")

  # with
  x$with(
      query = list(foo = "Bar", a = 5, b = 8,
        user = paste0("asdfa asldfj asdfljas dflajsd fasldjf",
          " asldfja sdfljas dflajs fdlasjf aslfa fdfdsf")),
      body = list(a = 5, b = 8, user = "asdfa asldfj asdfljas dflajsdfdfdsf",
        foo = "Bar"),
      headers = list(farm = "animal",
        `User-Agent` =
        "stuff things whasdlfj adsfla jsdflja sdflasj dflasj dfasljf asdf")
  )
  # with: long query
  expect_output(x$print(), "foo=Bar, a=5, b=8, user=asdfa asldfj asdflja...")
  # with: long body
  expect_output(x$print(), "a=5, b=8, user=asdfa asldfj asdflja..., foo=Bar")
  # with: long request headers
  expect_output(x$print(), "farm=animal, User-Agent=stuff things whasdlf...")

  # to_return
  x$to_return(
      status = 200,
      body = list(name = "julia", title = "advanced user",
        location = "somewhere in the middle of the earth",
        foo = "Bar"),
      headers = list(farm = "animal",
        `User-Agent` =
        "stuff things whasdlfj adsfla jsdflja sdflasj dflasj dfasljf asdf")
  )
  # to_return: status code
  expect_output(x$print(), "200")
  # to_return: long body
  expect_output(x$print(),
  "name=julia, title=advanced user, location=somewhere in the mid..., foo=Bar")
  # to_return: long response headers
  expect_output(x$print(), "farm=animal, User-Agent=stuff things whasdlf...")
})


test_that("StubbedRequest nested lists in body", {
  x <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
  x$with(
      query = list(foo = "Bar"),
      headers = list(farm = "animal"),
      body = list(a = list(b = list(c = "foo", d = "bar")))
  )
  expect_output(x$print(),
    "a = list\\(b = list\\(c = \"foo\", d = \"bar\"\\)\\)")

  # longer
  x$with(
      query = list(foo = "Bar"),
      headers = list(farm = "animal"),
      body = list(
        apple = list(
          bears = list(
            cheesecake =
            list(foo_do_the_thing = "bar asdjlfas dfaljsdf asljdf slf"))))
  )
  expect_output(x$print(),
    "apple = list\\(bears = list\\(cheesecake = list\\(foo_do_the_thing = \"bar asdjlfas dfa...")
})

test_that("StubbedRequest w/ >1 to_return()", {
  stub_registry_clear()

  x <- StubbedRequest$new(method = "get", uri = "httpbin.org")
  x$to_return(status = 200, body = "foobar", headers = list(a = 5))
  x$to_return(status = 200, body = "bears", headers = list(b = 6))
  x$to_s()

  expect_equal(length(x$responses_sequences), 2)
  expect_match(x$to_s(), "foobar")
  expect_match(x$to_s(), "bears")
})
