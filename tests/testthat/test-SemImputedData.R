library(testthat)
library(RMediation) # Replace with the name of your package
library(mice)

# Test for successful creation of SemImputedData objects
test_that("SemImputedData objects are created successfully with valid inputs", {
  data(mtcars)
  mtcars_mis <- mtcars
  mtcars_mis$mpg[1:5] <- NA
  imputed_data <- mice(mtcars_mis, m = 2, maxit = 2, print = FALSE)

  sem_imputed_data <- new("SemImputedData", data = imputed_data, method = "lavaan")
  expect_true(inherits(sem_imputed_data, "SemImputedData"))
})

# Test for handling invalid 'method' input
test_that("SemImputedData objects creation fails with invalid 'method'", {
  data(mtcars)
  mtcars_mis <- mtcars
  mtcars_mis$mpg[1:5] <- NA
  imputed_data <- mice(mtcars_mis, m = 2, maxit = 2, print = FALSE)
  expect_error(
    new("SemImputedData", data = imputed_data, method = "invalidMethod"),
    "'method' must be either 'lavaan' or 'OpenMx'"
  )
})
