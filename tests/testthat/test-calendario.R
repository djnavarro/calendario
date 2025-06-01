test_that("calendario objects can be created", {
  expect_no_error(calendario$new())
  expect_s3_class(calendario$new(), c("calendario", "R6"))
})
