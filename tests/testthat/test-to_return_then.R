context("to_return: then")

enable()
stub_registry_clear()

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

stub_registry_clear()
disable()
