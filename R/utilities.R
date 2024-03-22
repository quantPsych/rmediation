### ----------------------------------------------------------------------------
### n_imp
### ----------------------------------------------------------------------------

#' Get Number of Imputations from a mids Object
#'
#' This function returns the number of imputations stored in a `mids` object
#' created by the `mice` package.
#'
#' @param x A `mids` object representing multiple imputed datasets.
#' @return An integer representing the number of imputations.
#'
#' @usage n_imp(x)
#' @docType methods
#' @export
#' @examples
#' \dontrun{
#' # Assuming `imputed_data` is a mids object created by the mice package
#' n_imp(imputed_data)
#' }

setGeneric("n_imp", function(x) {
  standardGeneric("n_imp")
})

#' @rdname n_imp
#' @docType methods
#' @export

setMethod("n_imp", "mids", function(x) {
  if (!inherits(x, "mids")) {
    stop("The provided object is not a mids object.")
  }
  x$m
})
