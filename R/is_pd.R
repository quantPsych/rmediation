#' @title Checks if a matrix object is positive definite
#'
#' @description Determines if a matrix is positive definite (all eigenvalues
#' are strictly positive) by attempting Cholesky decomposition.
#'
#' @param x A numeric matrix.
#'
#' @return Returns `TRUE` if the matrix is positive definite, `FALSE` otherwise.
#'
#' @examples
#'
#' # Example of a positive definite matrix
#' A <- matrix(c(1, 2, 2, 4), nrow = 2)
#' is_pd(A) # Should return TRUE
#' @export
#' @rdname is_pd

setGeneric("is_pd", function(x, quiet = FALSE, ...) {
  standardGeneric("is_pd")
})

setMethod("is_pd",
  signature = "matrix",
  definition = function(x, quiet = FALSE, ...) {
    # # Check for symmetry
    # if (!isSymmetric(x)) {
    #   if (!quiet) print("Matrix is not symmetric, and thus, not positive definite.")
    #   return(FALSE)
    # }

    # Attempt Cholesky decomposition for efficiency and stability
    tryCatch(
      {
        chol(x, pivot = TRUE) # If Cholesky succeeds, return TRUE (positive definite)
        return(TRUE)
      },
      error = function(e) { # the function throws an errro when the matrix is not symmetric
        # If Cholesky fails, return FALSE (not positive definite)
        if (!quiet) print(e)
        return(FALSE)
      },
      warning = function(w) { # the function throws a warning when the matrix is not positive definite
        # If Cholesky fails, return FALSE (not positive definite)
        if (!quiet) print(w)
        return(FALSE)
      }
    )
  }
)
