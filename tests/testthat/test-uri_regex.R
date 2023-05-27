context("uri_regex")

test_that("uri_regex with crul", {
  stub_request("get", uri_regex = "hb.opencpu.org/.+") %>%
    to_return(body = list(foo = "bar"))

  library(crul)
  enable(adapter = "crul")
  
  invisible(
    lapply(c('elephants', 'bears', 'leaves', 'foo', 'bar'), function(z) {
      expect_true(HttpClient$new(hb())$get(z)$success())
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
  stub_request("get", uri_regex = "hb.opencpu.org/.+") %>%
    to_return(body = list(foo = "bar"))

  library(httr)
  enable(adapter = "httr")
  invisible(
    lapply(c('elephants', 'bears', 'leaves', 'foo', 'bar'), function(z) {
      expect_false(http_error(GET(file.path(hb(), z))))
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


test_that("uri_regex with httr2", {
  stub_request("get", uri_regex = "hb.opencpu.org/.+") %>%
    to_return(body = list(foo = "bar"))

  library(httr2)
  enable(adapter = "httr2")
  invisible(
    lapply(c('elephants', 'bears', 'leaves', 'foo', 'bar'), function(z) {
      req <- request(file.path(hb(), z))
      expect_false(resp_is_error(req_perform(req, mock = ~ mock_httr2(req))))
    })
  )

  # more complicated regex
  stub_request("get", uri_regex = "[Aa].+\\.io/apple/")
  invisible(
    lapply(c('Anounce', 'apple', 'Afar', 'after'), function(z) {
      req <- request(sprintf("https://%s.io/apple", z))
      expect_false(resp_is_error(req_perform(req, mock = ~ mock_httr2(req))))
      req2 <- request(sprintf("https://%s.io/fruit", z))
      expect_error(req_perform(req2, mock = ~ mock_httr2(req2)),
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
      # res <- GET(url, path = z)
      req <- request(url) %>% req_url_path_append(z)
      res <- req_perform(req, mock = ~ mock_httr2(req))
      expect_is(res, "httr2_response")
      expect_true(grepl(res$url, file.path(url, z), ignore.case = TRUE))
    })
  )
})

stub_registry_clear()
