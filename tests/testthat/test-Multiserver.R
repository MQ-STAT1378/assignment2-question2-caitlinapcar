test_that("Multiserver Works", {
  expect_error(Multiserver()) #does the package show an error message
  expect_output(Multiserver(5, 10, 2)) #does the package print an output to the console
})
