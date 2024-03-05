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
  results <- RMediation::lav_mice(model, imputed_data)

  # Verify the output
  expect_length(results$analyses, imputed_data$m)
  expect_true(all(sapply(results$analyses, function(x) is(x, "lavaan"))))


}
)