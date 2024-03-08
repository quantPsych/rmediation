test_that("pool.semMice returns correct values", {
  data(HolzingerSwineford1939, package = "lavaan")
  hs_short <- HolzingerSwineford1939[paste0("x", 1:9)]
  hs_short <- mice::ampute(hs_short, prop = 0.03, mech = "MAR")$amp

  # Perform multiple imputation
  imputed_data <- mice::mice(hs_short, m = 7, maxit = 5, seed = 12345, print = FALSE)

  # Define a simple SEM model using OpenMx
  manifestVars <- paste0("x", 1:9)
  latVar <- c("visual", "textual", "speed")

  model_mx <- OpenMx::mxModel("SimpleModel",
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
  fits_mx <- RMediation::mx_mice(model_mx, imputed_data, silent = TRUE, suppressWarnings = TRUE)

  # Simple SEM model specification with lavaan and sem function
  model_lav <- "
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9
  visual ~~ textual
  visual ~~ speed
  textual ~~ speed
  "
  fits_lav <- RMediation::lav_mice(model_lav, imputed_data, auto.var = TRUE, auto.fix.first = TRUE, auto.cov.lv.x = TRUE)

  # Compare the results
  res1 <- RMediation::pool(fits_mx)
  res2 <- pool(fits_lav)
 
  # Check if the results are named
  expect_named(res1)
  expect_named(res2)

  # check weather outputs res2 is a proper class of tibble?
  expect_true(tibble::is_tibble(res1))
  expect_true(tibble::is_tibble(res2))
})
