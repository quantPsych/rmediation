library(testthat)
library(lavaan)
library(OpenMx)

# Sample lavaan model syntax for testing
lav_model_syntax <- "
  # latent variables
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
  # regressions
    dem60 ~ ind60
  # covariances
    x1 ~~ y1
  # variances
    x1 ~~ x1
    y1 ~~ y1
"

# Sample data
data("PoliticalDemocracy", package = "lavaan")

# Tes if it fit throws an error when the model is character string.

test_that("fit throws an error when the model is character string", {
  str_model <- " some random string"
  expect_false(is_fit(str_model))
})

test_that("is_fit returns TRUE for a fitted lavaan model", {
  lav_model <- lavaan::sem(lav_model_syntax, data = PoliticalDemocracy)
  expect_true(is_fit(lav_model))
})

test_that("is_fit returns FALSE for an unfitted lavaan model", {
  lav_model <- lavaan::lavaan(model = lav_model_syntax, model.type = "sem", auto.var = TRUE, auto.fix.first = TRUE, auto.cov.lv.x = TRUE)
  expect_false(is_fit(lav_model))
})

test_that("is_fit returns TRUE for a fitted OpenMx model", {
  data <- na.omit(PoliticalDemocracy)
  latentVars <- c("ind60", "dem60")
  manifestVars <- names(data)
  mxModel <- mxModel("One Factor",
    type = "RAM",
    latentVars = latentVars,
    manifestVars = manifestVars,
    mxPath(from = "ind60", to = paste0("x", 1:3)),
    mxPath(from = "dem60", to = paste0("y", 1:4)),
    mxPath(from = "one", to = c(manifestVars, latentVars), free = FALSE, values = 0),
    mxPath(from = c(manifestVars, latentVars), arrows = 2),
    mxPath(from = "ind60", to = "dem60", arrows = 2, free = FALSE, values = 1),
    mxData(data, type = "raw")
  )
  mxModel_run <- mxRun(mxModel)
  expect_true(is_fit(mxModel_run))
})

test_that("is_fit returns FALSE for an unfitted OpenMx model", {
  data <- na.omit(PoliticalDemocracy)
  latentVars <- c("ind60", "dem60")
  manifestVars <- names(data)
  mxModel <- mxModel("One Factor",
    type = "RAM",
    latentVars = latentVars,
    manifestVars = manifestVars,
    mxPath(from = "ind60", to = paste0("x", 1:3)),
    mxPath(from = "dem60", to = paste0("y", 1:4)),
    mxPath(from = "one", to = c(manifestVars, latentVars), free = FALSE, values = 0),
    mxPath(from = c(manifestVars, latentVars), arrows = 2),
    mxPath(from = "ind60", to = "dem60", arrows = 2, free = FALSE, values = 1),
    mxData(data, type = "raw")
  )
  # mxModel is created but not run with mxRun
  expect_false(is_fit(mxModel))
})
