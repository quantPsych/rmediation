test_that("tidy.logLik returns expected data frame for valid input", {
  # Create a mock log-likelihood object
  ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
  ## Page 9: Plant Weight Data.
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)
  lm.D9 <- lm(weight ~ group)
  logLik_object <- logLik(lm.D9)

  # Call the function
  result <- RMediation::tidy(logLik_object) |> print()

  # Check if the result is as expected
  expect_equal(nrow(result), 1) # Check number of rows
  expect_equal(ncol(result), 3) # Check number of columns
  expect_equal(colnames(result), c("term", "estimate", "df")) # Check column names
  expect_equal(result$term, "logLikelihood") # Check term column value
  expect_equal(result$df, 3) # Check df column value
})

# Additional test cases can be added as needed
