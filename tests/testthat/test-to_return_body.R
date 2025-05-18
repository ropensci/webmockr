test_that("to_return: setting body behaves correctly", {
  enable(quiet = TRUE)
  stub_registry_clear()

  # character
  aa <- stub_request("get", "https://google.com") %>%
    to_return(body = '{"foo":"bar"}')
  z <- crul::HttpClient$new(url = "https://google.com")$get()
  expect_type(z$content, "raw")
  expect_type(z$parse("UTF-8"), "character")
  expect_equal(z$parse("UTF-8"), '{"foo":"bar"}')
  stub_registry_clear() # cleanup

  # list
  bb <- stub_request("get", "https://google.com") %>%
    to_return(body = list(foo = "bar"))
  z <- crul::HttpClient$new(url = "https://google.com")$get()
  expect_type(z$content, "raw")
  expect_type(z$parse("UTF-8"), "character")
  expect_equal(z$parse("UTF-8"), '{"foo":"bar"}')
  stub_registry_clear() # cleanup

  # NULL
  cc <- stub_request("get", "https://google.com") %>%
    to_return(body = NULL)
  z <- crul::HttpClient$new(url = "https://google.com")$get()
  expect_type(z$content, "raw")
  expect_type(z$parse("UTF-8"), "character")
  expect_equal(z$parse("UTF-8"), "")
  stub_registry_clear() # cleanup

  # FALSE
  dd <- stub_request("get", "https://google.com") %>%
    to_return(body = FALSE)
  z <- crul::HttpClient$new(url = "https://google.com")$get()
  expect_type(z$content, "raw")
  expect_type(z$parse("UTF-8"), "character")
  expect_equal(z$parse("UTF-8"), "")
  stub_registry_clear() # cleanup

  # raw
  ee <- stub_request("get", "https://google.com") %>%
    to_return(body = charToRaw('{"foo":"bar"}'))
  z <- crul::HttpClient$new(url = "https://google.com")$get()
  expect_type(z$content, "raw")
  expect_type(z$parse("UTF-8"), "character")
  expect_equal(z$parse("UTF-8"), '{"foo":"bar"}')
  stub_registry_clear() # cleanup
})

test_that("to_return: setting body with wrong type errors well", {
  stub_registry_clear()

  ## ERRORS when not of right type
  expect_error(
    stub_request("get", "https://google.com") %>%
      to_return(body = TRUE),
    "Unknown `body` type"
  )
})
