test_that("Adapter class can't be instantiated", {
  expect_s3_class(Adapter, "R6ClassGenerator")
  expect_error(
    Adapter$new(),
    "Adapter parent class should not be called directly"
  )
})

test_that("Adapter initialize method errors as expected", {
  adap <- R6::R6Class(
    "CrulAdapter",
    inherit = Adapter,
    public = list(
      client = NULL
    )
  )
  expect_error(adap$new(), "should not be called directly")
})

test_that("show_body_diff configuration setting", {
  webmockr_configure(show_body_diff = TRUE)
  withr::defer(webmockr_configure(show_body_diff = FALSE))

  library(httr, warn.conflicts = FALSE)
  enable(adapter = "httr", quiet = TRUE)

  stub_request("get", "https://hb.cran.dev/post") %>%
    wi_th(body = list(apple = "green"))

  expect_snapshot(
    POST("https://hb.cran.dev/post", body = list(apple = "red")),
    error = TRUE
  )
})

test_that("show_body_diff configuration setting - > 1 stub", {
  webmockr_configure(show_body_diff = TRUE)
  withr::defer(webmockr_configure(show_body_diff = FALSE))

  library(httr, warn.conflicts = FALSE)
  enable(adapter = "httr", quiet = TRUE)

  stub_request("get", "https://hb.cran.dev/post") %>%
    wi_th(body = list(apple = "green"))
  stub_request("get", "https://hb.cran.dev/post") %>%
    wi_th(body = list(pear = "purple"))

  expect_snapshot(
    POST("https://hb.cran.dev/post", body = list(apple = "red")),
    error = TRUE
  )
})
