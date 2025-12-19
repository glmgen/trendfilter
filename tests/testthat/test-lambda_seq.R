test_that("get_lambda_seq works", {
  # user specified sequence (note that there is no error checking)
  # This directly accesses the cpp code
  # There are no default arguments to avoid seg faults

  # User provided, no change (but converts to double)
  expect_equal(
    get_lambda_seq_r(10:1, 0.0, -1, 1e-5, 10L),
    as.double(10:1)
  )

  # length 10 seq between 10 and .1, ignores lambda_min_ratio
  expect_equal(
    get_lambda_seq_r(double(10), 10, .1, 1e-5, n_lambda = 10L),
    10^seq(log10(10), log10(.1), length.out = 10)
  )

  # uses lambda_min_ratio
  expect_equal(
    get_lambda_seq_r(double(10), 10, -1, 1e-4, 10),
    10^seq(log10(10), log10(1e-3), length.out = 10)
  )

  # allow 0 for lambda_min
  # We should see log-scale seq from 10 to lambda_min_ratio*lambda_max
  # of length 9, then a 0.
  expect_equal(
    get_lambda_seq_r(double(10), 10, 0, 1e-4, 10),
    c(10^seq(log10(10), log10(1e-3), length.out = 9), 0)
  )

  # allow 0 for lambda glmgen/trendfilter#18
  expect_equal(
    get_lambda_seq_r(double(1), -1, -1, 1e-4, 1),
    0
  )
})
