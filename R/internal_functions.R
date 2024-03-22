#' Determine the Type of a SEM Model
#'
#' This internal function checks whether the provided model argument is
#' a valid "lavaan" syntax, a "lavaan" object, or an "MxModel" object from the
#' OpenMx package. It is used internally within the package to handle model
#' input flexibly.
#'
#' @param model The model to be checked. Can be a character string representing
#'   "lavaan" syntax, a "lavaan" object, or an "MxModel" object from the OpenMx
#'   package.
#'
#' @return A character string indicating the type of the model: "lavaan_syntax"
#'   for a valid "lavaan" syntax, "lavaan" for a "lavaan" object, or "MxModel"
#'   for an "MxModel" object.
#'
#' @examples
#' \dontrun{
#' # Assuming 'lavaan_model_syntax' is a character string with valid lavaan syntax
#' print(model_type(lavaan_model_syntax))
#'
#' # Assuming 'lavaan_obj' is a "lavaan" object created with the lavaan package
#' print(model_type(lavaan_obj))
#'
#' # Assuming 'mx_model_obj' is an "MxModel" object created with the OpenMx package
#' print(model_type(mx_model_obj))
#' }
#'
#' @keywords internal
#' @noRd
model_type <- function(model) {
    if (is.character(model)) {
        if (is_lav_syntax(model, quiet = TRUE)) {
            return("lavaan_syntax")
        } else {
            stop("The provided model syntax is not a valid 'lavaan' syntax.")
        }
    } else if (inherits(model, "lavaan")) {
        return("lavaan")
    } else if (inherits(model, "MxModel")) {
        return(c("MxModel", "OpenMx"))
    } else {
        stop("The provided model is neither a 'lavaan' syntax, 'lavaan' object, nor 'MxModel' object in 'OpenMx'.")
    }
}

### ----------------------------------------------------------------------------
### vcov_lav
### ----------------------------------------------------------------------------
#' Extract the sampling covariance matrix from a lavaan object
#' @param x A lavaan object
#' @return A matrix
#' @importFrom lavaan lavTech
#' @keywords internal
#' @noRd
vcov_lav <- function(x) {
    lavaan::lavTech(x, what = "vcov", add.labels = TRUE) # This is useful
}

