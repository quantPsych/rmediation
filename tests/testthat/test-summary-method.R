
# library(testthat)
# library(RMediation) # Assuming RMediation is the name of your package

  data("HolzingerSwineford1939", package = "lavaan")
  hs <- HolzingerSwineford1939[paste0("x", 1:9)]
  hs <- mice::ampute(hs, prop = 0.1, mech = "MAR")$amp
  imputed_data <- mice::mice(hs, m = 3, maxit = 3, seed = 12345, printFlag = FALSE)
  model <- "
   visual  =~ x1 + x2 + x3
   textual =~ x4 + x5 + x6
   speed   =~ x7 + x8 + x9
   "
fit <- lavaan::sem(model, hs)
summary(fit)

sem_imputed_data <- set_sem(imputed_data, model)
summary(sem_imputed_data)

test_that("summary method for SemImputedData objects provides comprehensive summary", {
  summary_output <- capture.output(summary(sem_imputed_data))

  # Test that the summary includes the SEM method
  expect_true(any(grepl("SEM Method Used: lavaan", summary_output)))

  # Test that the summary includes the number of imputations
  # This example assumes a direct way to obtain the number of imputations
  # Adjust based on how 'n_imp' or equivalent is actually implemented
  expect_true(any(grepl("Number of Imputations:", summary_output)))

  # Test that the summary includes sections for original data, fitted model, and imputed data
  expect_true(any(grepl("Original Data Summary:", summary_output)))
  expect_true(any(grepl("Fitted Model Summary:", summary_output)))
  expect_true(any(grepl("Imputed Data Summary", summary_output)))
})


# test_that("summary method outputs correctly", {
#   data("HolzingerSwineford1939", package = "lavaan")
#   model <- "
#    visual  =~ x1 + x2 + x3
#    textual =~ x4 + x5 + x6
#    speed   =~ x7 + x8 + x9
#    "
#   fit <- lavaan::sem(model, HolzingerSwineford1939)
#   expect_true(inherits(fit, "lavaan"))
#   mediationObj <-
#     test_indirect(
#       model = fit,
#       method = "dop",
#       conf.level = 0.95,
#       estimate = 0.5,
#       conf.low = 0.45,
#       conf.high = 0.55,
#       se = 0.05
#     )
#
#   # Capture the output of the summary method
#   output <- capture.output(summary(mediationObj)) |> print()
#
#   # Test if the output contains expected lines
#   expect_true(any(grepl("Method Used:  dop ", output)))
#   expect_true(any(grepl("Confidence Level: 95.00%", output)))
#   expect_true(any(grepl("Point Estimate of Mediated Effect: 0.5000", output)))
#   expect_true(any(grepl("Confidence Interval: \\[0.4500, 0.5500\\] ", output)))
#   expect_true(any(grepl("Standard Error: 0.0500", output)))
# })
