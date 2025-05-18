test_that("stub_body_diff throws error when no stubs OR requests found", {
  request_registry_clear()
  stub_registry_clear()

  expect_error(stub_body_diff())
})

test_that("stub_body_diff throws error when a stub is found but a request is not found", {
  request_registry_clear()
  stub_registry_clear()

  stub_request("get", "https://hb.opencpu.org/get")

  expect_error(stub_body_diff())
})

test_that("stub_body_diff throws error when no stub is found but a request is found", {
  request_registry_clear()
  stub_registry_clear()

  crul::ok("https://nytimes.com")

  expect_error(stub_body_diff())
})

test_that("stub_body_diff works when both stub AND request are found, no diff found", {
  request_registry_clear()
  stub_registry_clear()

  enable(quiet = TRUE)
  stub_request("head", "https://nytimes.com")
  crul::ok("https://nytimes.com")

  body_diff <- stub_body_diff()
  expect_s4_class(body_diff, "Diff")
  expect_equal(attr(body_diff@diffs, "meta")$diffs[2], 0)
})

### WRITE THE TEST FOR A DIFFERENCE FOND
test_that("stub_body_diff works when both stub AND request are found, & there's a diff", {
  request_registry_clear()
  stub_registry_clear()

  enable(quiet = TRUE)
  stub_request("post", "https://hb.opencpu.org/post") %>%
    wi_th(body = list(apple = "green"))

  library(crul, warn.conflicts = FALSE)
  expect_error(
    HttpClient$new("https://hb.opencpu.org")$post(
      path = "post",
      body = list(apple = "red")
    ),
    "disabled"
  )

  body_diff <- stub_body_diff()
  expect_s4_class(body_diff, "Diff")
  expect_gt(attr(body_diff@diffs, "meta")$diffs[2], 0)
})

request_registry_clear()
stub_registry_clear()
