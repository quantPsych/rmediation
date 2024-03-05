test_that("lav_mice returns correct output", {
  # Prepare a small dataset with missing values
  data(HolzingerSwineford1939, package = "lavaan")
  hs_short <- HolzingerSwineford1939[paste0("x", 1:9)]
  hs_short <- mice::ampute(hs_short, prop = 0.1, mech = "MAR")$amp

  # Perform multiple imputation
  imputed_data <- mice(hs_short, m = 3, maxit = 5, seed = 12345, print = FALSE)

  # Simple SEM model specification with lavaan and sem function
  model <- '
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9
  visual ~~ textual
  visual ~~ speed
  textual ~~ speed
  '
  #expect_silent(model <- sem(model, data = hs_short))
  expect_no_error(results <- RMediation::lav_mice(model, imputed_data))
  #str(results)
  #results$est |> dplyr::bind_rows()
  #lapply(results$fit, summary)
  # Verify the output
  expect_length(results$analyses, imputed_data$m)
  expect_true(all(sapply(results$analyses, function(x) is(x, "lavaan"))))
  expect_true(inherits(results, "semMice"))
  expect_true(inherits(results, "mira"))
  expect_true(inherits(results, "lav"))

}
)