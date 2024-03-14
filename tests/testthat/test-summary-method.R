test_that("summary method outputs correctly", {
  data("HolzingerSwineford1939", package = "lavaan")
  model <- "
   visual  =~ x1 + x2 + x3
   textual =~ x4 + x5 + x6
   speed   =~ x7 + x8 + x9
   "
  fit <- lavaan::sem(model, HolzingerSwineford1939)
  expect_true(inherits(fit, "lavaan"))
  mediationObj <-
    test_indirect(
      model = fit,
      method = "dop",
      conf.level = 0.95,
      estimate = 0.5,
      conf.low = 0.45,
      conf.high = 0.55,
      se = 0.05
    )

  # Capture the output of the summary method
  output <- capture.output(summary(mediationObj)) |> print()

  # Test if the output contains expected lines
  expect_true(any(grepl("Method Used:  dop ", output)))
  expect_true(any(grepl("Confidence Level: 95.00%", output)))
  expect_true(any(grepl("Point Estimate of Mediated Effect: 0.5000", output)))
  expect_true(any(grepl("Confidence Interval: \\[0.4500, 0.5500\\] ", output)))
  expect_true(any(grepl("Standard Error: 0.0500", output)))
})
