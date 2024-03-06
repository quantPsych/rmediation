test_that("mx_mice returns correct output", {
  # Prepare a small dataset with missing values
  data(HolzingerSwineford1939, package = "lavaan")
  hs_short <- HolzingerSwineford1939[paste0("x", 1:9)]
  hs_short <- mice::ampute(hs_short, prop = 0.1, mech = "MAR")$amp

  # Perform multiple imputation
  imputed_data <- mice(hs_short, m = 3, maxit = 5, seed = 12345, print = FALSE)

  # Define a simple SEM model using OpenMx
  manifestVars <- paste0("x", 1:9)
  latVar <- c("visual", "textual", "speed")

  mxModel <- mxModel("SimpleModel",
    type = "RAM",
    manifestVars = manifestVars,
    latentVars = latVar,
    mxPath(from = "visual", to = c("x1", "x2", "x3")),
    mxPath(from = "textual", to = c("x4", "x5", "x6")),
    mxPath(from = "speed", to = c("x7", "x8", "x9")),
    mxPath(from = manifestVars, arrows = 2),
    mxPath(from = latVar, arrows = 2, free = FALSE, values = 1.0),
    mxPath(from = "one", to = manifestVars, arrows = 1, free = TRUE, values = 0.1),
    mxPath(from = "one", to = latVar, arrows = 1, free = FALSE, values = 0),
    mxData(hs_short, type = "raw")
  )

  # Call mx_mice
  results <- mx_mice(mxModel, imputed_data)

  # Verify the output
  expect_length(results$analyses, imputed_data$m)
  expect_true(all(sapply(results$analyses, inherits, "MxModel")),
    info = "All items in the results should be MxModel objects."
  )
})
