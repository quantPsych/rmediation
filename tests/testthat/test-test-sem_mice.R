library(testthat)
library(mice) # For mids object
library(lavaan) # For SEM model
library(OpenMx) # For MxModel

# Assuming lav_mice and mx_mice are properly defined and work as expected
# and you have defined `is_lav_syntax` function correctly

context("Testing sem_mice function")

# Test with a valid mids object and a lavaan model syntax
test_that("sem_mice works with lavaan syntax", {
  data(mtcars)
  mtcars_mis <- mtcars
  mtcars_mis$mpg[1:5] <- NA  # Introduce some missing values
  
  imputed_data <- mice::mice(mtcars_mis, m = 5, maxit = 2, method = 'pmm', print = FALSE)
  SEM_model <- 'mpg ~ cyl + disp'
  
  expect_silent(sem_mice(SEM_model, imputed_data))
})

# Test with a valid mids object and a MxModel object
test_that("sem_mice works with MxModel object", {
  data(mtcars)
  mtcars_mis <- mtcars
  mtcars_mis$mpg[1:5] <- NA  # Introduce some missing values
  
  imputed_data <- mice(mtcars_mis, m = 5, maxit = 2, method = 'pmm', print = FALSE)
  model <- mxModel(model="BasicModel",
                   manifestVars = c("mpg", "cyl", "disp"),
                   mxPath(from=c("cyl", "disp"), to="mpg", arrows = 1, free = T, labels = c("b1", "b2") ),
                   mxPath(from = 'one', to =  c("mpg", "cyl", "disp"), arrows = 1, free = c(TRUE, FALSE, FALSE), labels = c("b0", NA, NA) ),
                   mxPath(from = "mpg", to="mpg",arrows = 2,free = TRUE, labels = "mse" ),
                   type = "RAM",
                   mxData(mtcars, type = "raw"))
  
  expect_error(sem_mice(model, imputed_data))
})

# Test with invalid 'mids' object
test_that("sem_mice stops with invalid 'mids' object", {
  expect_error(sem_mice("mpg ~ cyl + disp", list()), "'mids' must be a 'mids' object")
})

