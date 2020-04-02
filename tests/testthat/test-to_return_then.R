context("to_return: then")

enable()
webmockr_reset()

test_that("to_return: then", {
  stub <- stub_request("get", "https://httpbin.org/get?stuff=things")
  to_return(stub, status = 200, body = "foobar", headers = list(a = 5))
  to_return(stub, status = 200, body = "bears", headers = list(b = 6))
  
  cli <- crul::HttpClient$new(url = "https://httpbin.org/")
  x1 <- cli$get("get", query = list(stuff="things"))
  x2 <- cli$get("get", query = list(stuff="things"))
  x3 <- cli$get("get", query = list(stuff="things"))

  # first should have foobar
  expect_equal(x1$parse("UTF-8"), "foobar")
  # second should have bears
  expect_equal(x2$parse("UTF-8"), "bears")
  # third should have bears again, and so on
  expect_equal(x3$parse("UTF-8"), "bears")
})

webmockr_reset()

test_that("to_return: webmockr_reset allows multiple requests to start from beginning", {
  stub <- stub_request("get", "https://httpbin.org/get?stuff=things")
  to_return(stub, status = 200, body = "foobar", headers = list(a = 5))
  to_return(stub, status = 200, body = "bears", headers = list(b = 6))
  
  cli <- crul::HttpClient$new(url = "https://httpbin.org/")
  x1 <- cli$get("get", query = list(stuff="things"))
  x2 <- cli$get("get", query = list(stuff="things"))

  expect_equal(x1$parse("UTF-8"), "foobar")
  expect_equal(x2$parse("UTF-8"), "bears")
  
  # no reset - both requests give 2nd to_return body
  z1 <- cli$get("get", query = list(stuff="things"))
  z2 <- cli$get("get", query = list(stuff="things"))

  expect_equal(z1$parse("UTF-8"), "bears")
  expect_equal(z2$parse("UTF-8"), "bears")

  # RESET - requests give back expected body (have to make stub again)
  webmockr_reset()
  stub <- stub_request("get", "https://httpbin.org/get?stuff=things")
  to_return(stub, status = 200, body = "foobar", headers = list(a = 5))
  to_return(stub, status = 200, body = "bears", headers = list(b = 6))

  w1 <- cli$get("get", query = list(stuff="things"))
  w2 <- cli$get("get", query = list(stuff="things"))

  expect_equal(w1$parse("UTF-8"), "foobar")
  expect_equal(w2$parse("UTF-8"), "bears")
})

webmockr_reset()
disable()
