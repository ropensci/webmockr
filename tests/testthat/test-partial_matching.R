test_that("include/exclude", {
  # keys and values works
  aa <- including(list(foo = "bar"))
  expect_output(print(aa), "<partial match>")
  expect_s3_class(aa, "partial")
  expect_type(unclass(aa), "list")
  expect_equal(length(aa), 1)
  expect_named(aa, "foo")
  expect_true(attr(aa, "partial_match"))
  expect_type(attr(aa, "partial_type"), "character")
  expect_equal(attr(aa, "partial_type"), "include")

  bb <- excluding(list(foo = "bar"))
  expect_output(print(bb), "<partial match>")
  expect_s3_class(bb, "partial")
  expect_type(unclass(bb), "list")
  expect_equal(length(bb), 1)
  expect_named(bb, "foo")
  expect_true(attr(bb, "partial_match"))
  expect_type(attr(bb, "partial_type"), "character")
  expect_equal(attr(bb, "partial_type"), "exclude")

  # just keys works
  cc <- including(list(foo = NULL, bar = NULL))
  expect_output(print(cc), "<partial match>")
  expect_s3_class(cc, "partial")
  expect_type(unclass(cc), "list")
  expect_equal(length(cc), 2)
})

skip_if_not_installed("httr")
library(httr)

test_that("include query parameters", {
  enable(adapter = "httr", quiet = TRUE)
  on.exit({
    disable(adapter = "httr", quiet = TRUE)
  })

  ## matches
  stub_request("get", "https://hb.cran.dev/get") %>%
    wi_th(query = including(list(fruit = "pear"))) %>%
    to_return(body = "matched on including partial query!")

  resp_matched <- GET(
    "https://hb.cran.dev/get",
    query = list(fruit = "pear")
  )

  expect_equal(resp_matched$status_code, 200)
  expect_equal(
    rawToChar(content(resp_matched)),
    "matched on including partial query!"
  )

  stub_registry_clear()

  ## doesn't match when query params dont include what the stub has
  expect_error(
    GET("https://hb.cran.dev/get", query = list(meat = "chicken")),
    "Real HTTP connections are disabled"
  )

  # cleanup
  stub_registry_clear()
})

test_that("exclude query parameters", {
  enable(adapter = "httr", quiet = TRUE)
  on.exit({
    disable(adapter = "httr", quiet = TRUE)
  })

  ## matches
  stub_request("get", "https://hb.cran.dev/get") %>%
    wi_th(query = excluding(list(fruit = "pear"))) %>%
    to_return(body = "matched on excluding partial query!")

  resp_matched <- GET(
    "https://hb.cran.dev/get",
    query = list(fruit = "apple")
  )

  expect_equal(resp_matched$status_code, 200)
  expect_equal(
    rawToChar(content(resp_matched)),
    "matched on excluding partial query!"
  )

  ## doesn't match when query params include what's excluded
  expect_error(
    GET("https://hb.cran.dev/get", query = list(fruit = "pear")),
    "Real HTTP connections are disabled"
  )

  # cleanup
  stub_registry_clear()
})


test_that("include query parameters, just keys", {
  enable(adapter = "httr", quiet = TRUE)
  on.exit({
    disable(adapter = "httr", quiet = TRUE)
  })

  ## matches
  stub_request("get", "https://hb.cran.dev/get") %>%
    wi_th(query = including(list(fruit = NULL))) %>%
    to_return(body = "matched on including key!")

  resp_matched <- GET(
    "https://hb.cran.dev/get",
    query = list(fruit = "pear")
  )

  expect_equal(resp_matched$status_code, 200)
  expect_equal(rawToChar(content(resp_matched)), "matched on including key!")

  stub_registry_clear()

  ## doesn't match when no query param keys match the include
  expect_error(
    GET("https://hb.cran.dev/get", query = list(meat = "chicken")),
    "Real HTTP connections are disabled"
  )

  # cleanup
  stub_registry_clear()
})

test_that("exclude query parameters, just keys", {
  enable(adapter = "httr", quiet = TRUE)
  on.exit({
    disable(adapter = "httr", quiet = TRUE)
  })

  ## matches
  stub_request("get", "https://hb.cran.dev/get") %>%
    wi_th(query = excluding(list(fruit = NULL))) %>%
    to_return(body = "matched on excluding key!")

  resp_matched <- GET(
    "https://hb.cran.dev/get",
    query = list(stuff = "things")
  )

  expect_equal(resp_matched$status_code, 200)
  expect_equal(rawToChar(content(resp_matched)), "matched on excluding key!")

  stub_registry_clear()

  ## doesn't match when there's a query param key that matches the exclude
  expect_error(
    GET("https://hb.cran.dev/get", query = list(fruit = "pineapple")),
    "Real HTTP connections are disabled"
  )

  # cleanup
  stub_registry_clear()
})


test_that("include request body", {
  enable(adapter = "httr", quiet = TRUE)
  on.exit({
    disable(adapter = "httr", quiet = TRUE)
  })

  ## matches
  stub_request("post", "https://hb.cran.dev/post") %>%
    wi_th(body = including(list(fruit = "pear"))) %>%
    to_return(body = "matched on including partial body!")

  resp_matched <- POST(
    "https://hb.cran.dev/post",
    body = list(fruit = "pear", meat = "chicken")
  )

  expect_equal(resp_matched$status_code, 200)
  expect_equal(
    rawToChar(content(resp_matched)),
    "matched on including partial body!"
  )

  stub_registry_clear()

  ## doesn't match when request body does not include what the stub has
  expect_error(
    POST("https://hb.cran.dev/post", query = list(meat = "chicken")),
    "Real HTTP connections are disabled"
  )

  # cleanup
  stub_registry_clear()
})

test_that("exclude request body", {
  enable(adapter = "httr", quiet = TRUE)
  on.exit({
    disable(adapter = "httr", quiet = TRUE)
  })

  ## matches
  stub_request("post", "https://hb.cran.dev/post") %>%
    wi_th(body = excluding(list(fruit = "pear"))) %>%
    to_return(body = "matched on excluding partial body!")

  resp_matched <- POST(
    "https://hb.cran.dev/post",
    body = list(color = "blue")
  )

  expect_equal(resp_matched$status_code, 200)
  expect_equal(
    rawToChar(content(resp_matched)),
    "matched on excluding partial body!"
  )

  stub_registry_clear()

  ## doesn't match when request body does not include what the stub has
  expect_error(
    POST(
      "https://hb.cran.dev/post",
      body = list(fruit = "pear", meat = "chicken")
    ),
    "Real HTTP connections are disabled"
  )

  # cleanup
  stub_registry_clear()
})
