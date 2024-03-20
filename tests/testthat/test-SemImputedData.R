test_that("run_sem executes correctly with lavaan models", {
  # Assuming you have a predefined `SemImputedData` object for lavaan models
  # Load Holzinger and Swineford (1939) dataset
  data("HolzingerSwineford1939", package = "lavaan")
  # Introduce missing data
  df_complete <-
    na.omit(HolzingerSwineford1939[c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")])
  amp <- mice::ampute(df_complete, prop = 0.2, mech = "MAR")
  data_with_missing <- amp$amp

  # Perform multiple imputation
  imputed_data <-
    mice::mice(
      data_with_missing,
      m = 3,
      maxit = 3,
      seed = 12345,
      printFlag = FALSE
    )

  model <- "
 visual  =~ x1 + x2 + x3
 textual =~ x4 + x5 + x6
 speed   =~ x7 + x8 + x9
 "
  sem_data <-
    set_sem(model, data = imputed_data, conf_int = TRUE, conf_level = 0.95)
  # result <- run_sem(sem_data)
  expect_no_error(result <- run_sem(sem_data))
  #   expect_no_error(lapply(result@results, summary))
  # Check that the result is as expected
  # This will depend on the output format of run_sem
  # Example:
  expect_true(inherits(result, "SemResults"))
})


test_that("run_sem executes correctly with OpenMx models", {
  # Assuming you have a predefined `SemImputedData` object for OpenMx models
  # Load Holzinger and Swineford (1939) dataset
  data("HolzingerSwineford1939", package = "lavaan")
  # Introduce missing data
  df_complete <-
    na.omit(HolzingerSwineford1939[c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")])
  amp <- mice::ampute(df_complete, prop = 0.05, mech = "MAR")
  data_with_missing <- amp$amp

  # Perform multiple imputation
  imputed_data <-
    mice::mice(
      data_with_missing,
      m = 3,
      maxit = 3,
      seed = 12345,
      printFlag = FALSE
    )

  # Define a simple SEM model using OpenMx
  manifestVars <- paste0("x", 1:9)
  latVar <- c("visual", "textual", "speed")

  mx_model <- mxModel(
    "SimpleModel",
    type = "RAM",
    manifestVars = manifestVars,
    latentVars = latVar,
    OpenMx::mxPath(from = "visual", to = c("x1", "x2", "x3")),
    OpenMx::mxPath(from = "textual", to = c("x4", "x5", "x6")),
    OpenMx::mxPath(from = "speed", to = c("x7", "x8", "x9")),
    OpenMx::mxPath(from = manifestVars, arrows = 2),
    OpenMx::mxPath(
      from = latVar,
      arrows = 2,
      free = FALSE,
      values = 1.0
    ),
    OpenMx::mxPath(
      from = "one",
      to = manifestVars,
      arrows = 1,
      free = FALSE,
      values = 0
    ),
    OpenMx::mxPath(
      from = "one",
      to = latVar,
      arrows = 1,
      free = FALSE,
      values = 0
    )
  )
  sem_data <-
    set_sem(mx_model, data = imputed_data)

  expect_no_error(result <- run_sem(sem_data))

  # Check that the result is as expected
  expect_true(inherits(result, "SemResults"))
})
