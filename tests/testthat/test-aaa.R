context("test-aaa")

test_that("null switch works as intended", {
  expect_equal(null_switch(NULL, 2), 2)
  expect_equal(null_switch(1, 2), 1)
})
