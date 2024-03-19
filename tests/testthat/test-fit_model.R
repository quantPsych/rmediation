# Define a sample dataset
data("HolzingerSwineford1939", package = "lavaan")

# Define a correct lavaan model syntax as a string
lav_model_syntax <- "visual =~ x1 + x2 + x3"

# Define a correct OpenMx model
mxModelSyntax <- mxModel("OpenMxExampleModel",
  type = "RAM",
  manifestVars = c("x1", "x2", "x3"),
  latentVars = c("visual"),
  mxPath(from = c("x1", "x2", "x3"), arrows = 2),
  mxPath(from = "one", to = c("x1", "x2", "x3"), arrows = 1, free = FALSE, values = 1)
)

# Test fitting a model with correct lavaan syntax
test_that("fit_model fits a model with correct lavaan syntax", {
  expect_s4_class(fit_model(lav_model_syntax, HolzingerSwineford1939), "lavaan")
})

# Test fitting a model with an OpenMx model
test_that("fit_model fits a model with OpenMx model", {
  expect_s4_class(fit_model(mxModelSyntax, HolzingerSwineford1939), "MxModel")
})

# Test fitting a model that is already fitted
test_that("fit_model returns the same model if it's already fitted", {
  fitted_lav_model <- lavaan::sem(lav_model_syntax, data = HolzingerSwineford1939)
  expect_identical(fit_model(fitted_lav_model, HolzingerSwineford1939), fitted_lav_model)
})

# Test fitting a model with incorrect lavaan syntax (assuming `is_lav_syntax` can handle incorrect syntax)
test_that("fit_model handles incorrect lavaan syntax gracefully", {
  incorrect_lav_model_syntax <- "visual =~ x5-x3"
  expect_error(fit_model(incorrect_lav_model_syntax, HolzingerSwineford1939))
})
