context("uri_regex")

test_that("uri_regex with crul", {
  stub_request("get", uri_regex = "httpbin.org/.+") %>%
    to_return(body = list(foo = "bar"))

  library(crul)
  enable(adapter = "crul")
  
  invisible(
    lapply(c('elephants', 'bears', 'leaves', 'foo', 'bar'), function(z) {
      expect_true(HttpClient$new("https://httpbin.org")$get(z)$success())
    })
  )

  # more complicated regex
  stub_request("get", uri_regex = "[Aa].+\\.io/apple/")
  invisible(
    lapply(c('Anounce', 'apple', 'Afar', 'after'), function(z) {
      expect_true(HttpClient$new(sprintf("https://%s.io", z))$get("apple")$success())
      expect_error(HttpClient$new(sprintf("https://%s.io", z))$get("fruit"),
        "Real HTTP connections are disabled")
    })
  )

  # regex to match any URL
  ## https://github.com/ropensci/webmockr/issues/113 
  ## when matching any url with `.+`, it would lead to an empty url in response
  ##  object, at least with crul
  stub_request("get", uri_regex = ".+")
  invisible(
    lapply(c('Anounce', 'apple', 'Afar', 'after'), function(z) {
      url <- sprintf("https://%s.io", z)
      res <- HttpClient$new(url)$get(z)
      expect_is(res, "HttpResponse")
      expect_true(grepl(res$url, file.path(url, z), ignore.case = TRUE))
    })
  )
})

stub_registry_clear()

test_that("uri_regex with httr", {
  stub_request("get", uri_regex = "httpbin.org/.+") %>%
    to_return(body = list(foo = "bar"))

  library(httr)
  enable(adapter = "httr")
  invisible(
    lapply(c('elephants', 'bears', 'leaves', 'foo', 'bar'), function(z) {
      expect_false(http_error(GET(file.path("https://httpbin.org", z))))
    })
  )

  # more complicated regex
  stub_request("get", uri_regex = "[Aa].+\\.io/apple/")
  invisible(
    lapply(c('Anounce', 'apple', 'Afar', 'after'), function(z) {
      expect_false(http_error(GET(sprintf("https://%s.io/apple", z))))
      expect_error(GET(sprintf("https://%s.io/fruit", z)),
        "Real HTTP connections are disabled")
    })
  )

  # regex to match any URL
  ## https://github.com/ropensci/webmockr/issues/113 
  ## when matching any url with `.+`, it would lead to an empty url in response
  ##  object, at least with crul
  stub_request("get", uri_regex = ".+")
  invisible(
    lapply(c('Anounce', 'apple', 'Afar', 'after'), function(z) {
      url <- sprintf("https://%s.io", z)
      res <- GET(url, path = z)
      expect_is(res, "response")
      expect_true(grepl(res$url, file.path(url, z), ignore.case = TRUE))
    })
  )
})

stub_registry_clear()
