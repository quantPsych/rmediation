test_that("show method prints correctly", {
  # Load Holzinger and Swineford (1939) dataset
  data("HolzingerSwineford1939", package = "lavaan")
  model <- "
   visual  =~ x1 + x2 + x3
   textual =~ x4 + x5 + x6
   speed   =~ x7 + x8 + x9
   "
  fit <- lavaan::sem(model, HolzingerSwineford1939)
  expect_true(inherits(fit, "lavaan"))
  # Create a MediationCI object
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

  # Capture the output of the show method
  output <- capture.output(show(mediationObj)) |> print()
  # Test if the output contains expected lines
  expect_true(any(grepl("Method Used:  dop ", output)))
  expect_true(any(grepl("Point Estimate: 0.5000", output)))
})
