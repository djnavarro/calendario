test_that("calendario objects can be created", {
  expect_no_error(Calendario$new())
  expect_s3_class(Calendario$new(), c("Calendario", "R6"))
})
