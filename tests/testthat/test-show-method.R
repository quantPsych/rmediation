library(testthat)
library(RMediation) # Assuming your package is named RMediation

### ============================================================================
### Test the show method for SemImputedData
### ============================================================================

# Define a function to create a SemImputedData object
create_sem <- function(method = "lavaan") {
  data("HolzingerSwineford1939", package = "lavaan")
  hs_data <- HolzingerSwineford1939[paste0("x", 1:9)] |> mice::ampute(prop = 0.1, mech = "MAR")
  hs_data <- hs_data$amp
  imp_data <- mice::mice(hs_data, m = 5, maxit = 3, seed = 12345, printFlag = FALSE)

  cat("\n Number of imputations: ", n_imp(imp_data), "\n")

  model_lav <- "visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9"

  sem_data <- set_sem(imp_data, model_lav)
  return(list(sem_imputed_data = sem_data))
}

res <- create_sem()$sem_imputed_data
print(res)

test_that("show method for SemImputedData displays correctly", {
  # Capture the output of the show method
  mock_sem_imputed_data <- create_sem()$sem_imputed_data
  output <- capture.output(show(mock_sem_imputed_data))

  # Test if the output contains specific lines indicating the method, number of imputations, etc.
  expect_true(grepl("Model Setup:", output[1]))
  #expect_true(grepl("Confidence level: 0.95", output))

  # Optionally, you can test the length of the output to match expected number of lines
  expect_equal(length(output), 16) # Adjust based on your show method's output
})
