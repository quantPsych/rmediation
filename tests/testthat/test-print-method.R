test_that("Print method outputs correctly for MediationCI objects", {
  # Load Holzinger and Swineford (1939) dataset
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

  # Use capture.output to catch the print output
  output <- capture.output(print(mediationObj)) |> print()

  # Check if the output contains expected strings
  expect_true(any(grepl("MediationCI Object", output)))
  expect_true(any(grepl("Method:  dop ", output)))
  expect_true(any(grepl("Confidence Level: 95.00%", output, fixed = TRUE)))
  expect_true(any(grepl("Estimate: 0.5000", output)))
})
