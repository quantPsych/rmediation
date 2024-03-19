# Positive definite matrix (all eigenvalues > 0)
test_that("Positive definite matrix returns TRUE", {
  A <- matrix(c(2, -1, -1, 2), nrow = 2)
  expect_true(is_pd(A))
  expect_true(det(A) > 0)
  expect_no_error(chol(A, pivot = TRUE))
})

# Positive semi-definite matrix (some eigenvalues = 0)
test_that("Positive semi-definite matrix returns FALSE", {
  B <- matrix(c(2, -2, -2, 2), nrow = 2)
  expect_false(is_pd(B))
  expect_false(det(B) > 0)
  expect_warning(chol(B, pivot = TRUE))
})

# Indefinite matrix (negative eigenvalues present)
test_that("Indefinite matrix returns FALSE", {
  C <- matrix(c(-2, 2, 2, -2), nrow = 2)
  expect_false(is_pd(C))
  expect_false(det(C) > 0)
  expect_warning(chol(C, pivot = TRUE))
})

# Non-square matrix (should not be considered positive definite)
test_that("Non-square matrix returns FALSE", {
  D <- matrix(1:6, nrow = 2)
  expect_false(is_pd(D))
  expect_error(chol(D, pivot = TRUE))
  expect_error(det(D) > 0)
})

# Non-symmetric matrix (should return FALSE due to symmetry check)
test_that("Non-symmetric matrix returns FALSE", {
  E <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_false(is_pd(E))
  expect_warning(chol(E, pivot = TRUE))
  expect_false(det(E) > 0)
})

# Example from the function documentation
test_that("Documentation example works as expected", {
  A <- matrix(c(1, 2, 2, 4), nrow = 2)
  expect_false(is_pd(A))
  expect_false(det(A) > 0)
  expect_warning(chol(A, pivot = TRUE))
})
