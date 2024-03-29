context("test-regDIF")

test_that("Stop for negative tuning values.", {
  expect_error(regDIF(ida[,1:6],
                      ida[,7:9],
                      tau = -1),
               "Tau values must be non-negative.")
})
