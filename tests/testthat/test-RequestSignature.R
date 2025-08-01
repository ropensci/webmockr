test_that("RequestSignature: works", {
  expect_s3_class(RequestSignature, "R6ClassGenerator")

  aa <- RequestSignature$new(method = "get", uri = hb("/get"))

  expect_s3_class(aa, "RequestSignature")

  expect_null(aa$auth)
  expect_null(aa$body)
  expect_null(aa$headers)
  expect_null(aa$proxies)
  expect_null(aa$fields)
  expect_null(aa$output)

  expect_type(aa$method, "character")
  expect_equal(aa$method, "get")

  expect_type(aa$uri, "character")
  expect_equal(aa$uri, hb("/get"))

  expect_type(aa$to_s, "closure")
  expect_equal(aa$to_s(), sprintf("GET:  %s", hb("/get")))
})

test_that("RequestSignature: with bodies work", {
  aa <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(body = "abc")
  )
  expect_output(print(aa), "<unnamed>")

  bb <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(body = list(some_thing = "abc"))
  )
  expect_no_match(capture.output(print(bb)), "<unnamed>")
})

test_that("RequestSignature: different methods work", {
  aa <- RequestSignature$new(
    method = "post",
    uri = hb("/post"),
    options = list(fields = list(foo = "bar"))
  )
  aa$headers <- list(Accept = "application/json")
  aa$body <- list(foo = "bar")

  expect_type(aa$method, "character")
  expect_type(aa$uri, "character")
  expect_type(aa$headers, "list")
  expect_type(aa$body, "list")
  expect_type(aa$fields, "list")
  expect_named(aa$fields, "foo")
})

test_that("RequestSignature fails well", {
  expect_error(RequestSignature$new(), "argument \"method\" is missing")
  expect_error(
    RequestSignature$new(method = "adf"),
    "'arg' should be one of"
  )
  expect_error(
    RequestSignature$new(method = "get"),
    "argument \"uri\" is missing"
  )
})
