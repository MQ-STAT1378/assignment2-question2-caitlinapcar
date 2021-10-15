test_that("Multiserver Works", {
  expect_error(Multiserver())
  expect_output(Multiserver(5, 10, 2))
})
