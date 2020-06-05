context("partial matching")

test_that("include/exclude", {
  aa <- including(list(foo = "bar"))
  expect_is(aa, "list")
  expect_equal(length(aa), 1)
  expect_named(aa, "foo")
  expect_true(attr(aa, "partial_match"))
  expect_is(attr(aa, "partial_type"), "character")
  expect_equal(attr(aa, "partial_type"), "include")

  bb <- excluding(list(foo = "bar"))
  expect_is(bb, "list")
  expect_equal(length(bb), 1)
  expect_named(bb, "foo")
  expect_true(attr(bb, "partial_match"))
  expect_is(attr(bb, "partial_type"), "character")
  expect_equal(attr(bb, "partial_type"), "exclude")
})
