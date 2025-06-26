test_that("HttpLibAdapaterRegistry: structure", {
  expect_s3_class(HttpLibAdapaterRegistry, "R6ClassGenerator")

  aa <- HttpLibAdapaterRegistry$new()

  expect_s3_class(aa, "HttpLibAdapaterRegistry")

  expect_null(aa$adapters)
  expect_type(aa$clone, "closure")
  expect_type(aa$print, "closure")
  expect_type(aa$register, "closure")

  expect_output(print(aa), "HttpLibAdapaterRegistry")
})

test_that("HttpLibAdapaterRegistry: behaves as expected", {
  skip_on_cran()

  aa <- HttpLibAdapaterRegistry$new()
  aa$register(CrulAdapter$new())

  expect_length(aa$adapters, 1)
  expect_s3_class(aa$adapters[[1]], "CrulAdapter")
  expect_equal(aa$adapters[[1]]$name, "CrulAdapter")

  expect_output(print(aa), "HttpLibAdapaterRegistry")
  expect_output(print(aa), "CrulAdapter")
})

test_that("HttpLibAdapaterRegistry: behaves as expected", {
  skip_on_cran()

  aa <- HttpLibAdapaterRegistry$new()
  aa$register(HttrAdapter$new())

  expect_length(aa$adapters, 1)
  expect_s3_class(aa$adapters[[1]], "HttrAdapter")
  expect_equal(aa$adapters[[1]]$name, "HttrAdapter")

  expect_output(print(aa), "HttpLibAdapaterRegistry")
  expect_output(print(aa), "HttrAdapter")
})

test_that("HttpLibAdapaterRegistry: behaves as expected", {
  skip_on_cran()

  aa <- HttpLibAdapaterRegistry$new()
  aa$register(Httr2Adapter$new())

  expect_length(aa$adapters, 1)
  expect_s3_class(aa$adapters[[1]], "Httr2Adapter")
  expect_equal(aa$adapters[[1]]$name, "Httr2Adapter")

  expect_output(print(aa), "HttpLibAdapaterRegistry")
  expect_output(print(aa), "Httr2Adapter")
})

test_that("HttpLibAdapaterRegistry fails well", {
  x <- HttpLibAdapaterRegistry$new()

  expect_error(x$register(), "argument \"x\" is missing")
  expect_error(
    x$register(4),
    "'x' must be an adapter, such as CrulAdapter"
  )
})
