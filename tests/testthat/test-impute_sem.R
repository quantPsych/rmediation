test_that("impute_sem creates valid objects with correct inputs", {
  mtcars_mis <- mice::mice(mtcars, m = 2, maxit = 2, seed = 123, print = FALSE)

  # Test for lavaan method
  sem_data_lavaan <- impute_sem(data = mtcars_mis, method = "lavaan")
  expect_true(inherits(sem_data_lavaan, "SemImputedData"))
  expect_equal(sem_data_lavaan@method, "lavaan")

  # Test for OpenMx method
  sem_data_openmx <- impute_sem(data = mtcars_mis, method = "OpenMx")
  expect_true(inherits(sem_data_openmx, "SemImputedData"))
  expect_equal(sem_data_openmx@method, "OpenMx")
})

test_that("impute_sem handles invalid inputs correctly", {
  # Invalid 'data' input
  # expect_error(
  #   impute_sem(data = list(), method = "lavaan"),
  #   "'data' must be a 'mids' object from the 'mice' package."
  # )

  # Invalid 'method' input
  mtcars_mis <- mice::mice(mtcars, m = 2, maxit = 2, seed = 123, print = FALSE)
  expect_error(
    impute_sem(data = mtcars_mis, method = "invalidMethod"),
    "'method' must be either 'lavaan' or 'OpenMx'."
  )
})
