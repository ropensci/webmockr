context("HttpLibAdapaterRegistry")

test_that("HttpLibAdapaterRegistry: structure", {
  expect_is(HttpLibAdapaterRegistry, "R6ClassGenerator")

  aa <- HttpLibAdapaterRegistry$new()

  expect_is(aa, "HttpLibAdapaterRegistry")

  expect_null(aa$adapters)
  expect_is(aa$clone, "function")
  expect_is(aa$print, "function")
  expect_is(aa$register, "function")

  expect_output(print(aa), "HttpLibAdapaterRegistry")
})

test_that("HttpLibAdapaterRegistry: behaves as expected", {
  skip_on_cran()

  aa <- HttpLibAdapaterRegistry$new()
  aa$register(CrulAdapter$new())

  expect_length(aa$adapters, 1)
  expect_is(aa$adapters[[1]], "CrulAdapter")
  expect_equal(aa$adapters[[1]]$name, "CrulAdapter")

  expect_output(print(aa), "HttpLibAdapaterRegistry")
  expect_output(print(aa), "CrulAdapter")
})

test_that("HttpLibAdapaterRegistry: behaves as expected", {
  skip_on_cran()

  aa <- HttpLibAdapaterRegistry$new()
  aa$register(HttrAdapter$new())

  expect_length(aa$adapters, 1)
  expect_is(aa$adapters[[1]], "HttrAdapter")
  expect_equal(aa$adapters[[1]]$name, "HttrAdapter")

  expect_output(print(aa), "HttpLibAdapaterRegistry")
  expect_output(print(aa), "HttrAdapter")
})

test_that("HttpLibAdapaterRegistry fails well", {
  x <- HttpLibAdapaterRegistry$new()

  expect_error(x$register(), "argument \"x\" is missing")
  expect_error(x$register(4),
               "'x' must be an adapter, such as CrulAdapter")
})
