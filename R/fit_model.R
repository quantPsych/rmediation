#' Fit a Structural Equation Model
#'
#' Fits a structural equation model to provided data using either the `lavaan` or `OpenMx` package.
#'
#' @param model A character string representing `lavaan` model syntax, a `lavaan` model object, or an `OpenMx::MxModel` object.
#' @param data A data frame containing the data to which the model will be fitted.
#'
#' @return A fitted model object from either `lavaan` or `OpenMx`, depending on the input.
#' @importFrom lavaan sem
#' @importFrom OpenMx mxRun mxModel mxData
#' @export
#' @examples
#' \dontrun{
#' data("HolzingerSwineford1939", package = "lavaan")
#' lav_model_syntax <- "visual =~ x1 + x2 + x3"
#' fitted_model <- fit_model(lav_model_syntax, HolzingerSwineford1939)
#' }
fit_model <- function(model, data) {
  if (!is_lav_syntax(model, quiet = TRUE) && !inherits(model, "lavaan") && !inherits(model, "MxModel")) {
    stop("The model must be a character string, a lavaan model object, or an OpenMx model object.")
  }

  if (!is.data.frame(data)) stop("The data must be a data frame.")

  # Check if model is a character string and attempt to fit it using lavaan
  if (is.character(model) && is_lav_syntax(model)) {
    return(lavaan::sem(model, data = data))
  }

  # If model is a lavaan object and has not been fitted, fit the model
  if (inherits(model, "lavaan") && !is_fit(model)) {
    return(lavaan::sem(model, data = data, do.fit = TRUE))
  }

  # If model is an OpenMx model and has not been fitted, fit the model
  if (inherits(model, "MxModel") && !is_fit(model)) {
    model_n <- OpenMx::mxModel(model, mxData(data, type = "raw"))
    return(OpenMx::mxRun(model_n))
  }

  # If model is already fitted or does not match any of the above criteria, return the model unchanged
  return(model)
}
