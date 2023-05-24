context("stub_registry")

test_that("stub_registry: works", {
  # before any stubs creatd
  expect_output(print(stub_registry()), "Registered Stubs")
  expect_equal(length(stub_registry()$request_stubs), 0)

  # after a stub creatd
  stub_request("get", "https://scottchamberlain.info")
  expect_equal(length(stub_registry()$request_stubs), 1)
  expect_match(stub_registry()$request_stubs[[1]]$to_s(),
    "GET: https://scottchamberlain.info")

  # stub with body
  stub_request('post', uri = hb('/post')) %>%
     wi_th(
       body = list(y=crul::upload(system.file("CITATION")))
     )
  expect_equal(length(stub_registry()$request_stubs), 2)
  expect_match(stub_registry()$request_stubs[[2]]$to_s(),
    "POST: https://hb.opencpu.org/post")
  expect_match(stub_registry()$request_stubs[[2]]$to_s(),
    "CITATION")
  expect_match(stub_registry()$request_stubs[[2]]$to_s(),
    "text/plain")

  stub_registry_clear()

  # stub with > 1 to_return()
  s <- stub_request("get", hb("/get"))
  to_return(s, status = 200, body = "foobar", headers = list(a = 5))
  to_return(s, status = 200, body = "bears", headers = list(b = 6))
  expect_equal(length(stub_registry()$request_stubs), 1)
  expect_equal(length(stub_registry()$request_stubs[[1]]$to_s()), 1)
  expect_match(stub_registry()$request_stubs[[1]]$to_s(), "foobar")
  expect_match(stub_registry()$request_stubs[[1]]$to_s(), "bears")
})

test_that("stub_registry fails well", {
  expect_error(stub_registry(4), "unused argument")
})
