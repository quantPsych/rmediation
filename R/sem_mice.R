#' Fit a structural equation model to multiply imputed data
#'
#' This function fits a structural equation model to multiply imputed data. It
#' is an extension for the [lavaan::sem()] and [OpenMx::MxModel] functions to
#' handle [mice::mids] objects from the [mice] package. It allows for both a
#' structural equation model syntax as a character string or a pre-fitted
#' `lavaan` or `OpenMx` model object. This function is a wrapper for the
#' [lav_mice] and [mx_mice] functions.
#'
#' @param model Either a character string representing the structural equation
#'  model to be fitted or a pre-fitted `lavaan` or `OpenMx` model object.
#' @param mids A `mice::mids` object from the `mice` package.
#' @param ... Additional arguments to be passed to `[lavaan::sem]` or
#' `[OpenMx::mxRun]`.
#' @return A list of `lavaan` or `OpenMx` model fits, one for each imputed
#' dataset. The class of the returned object is `lav` for lavaan /`mx` for `MxModel`, `semlist`, and `list`.
#' @examples
#' # library(mice)
#' # library(lavaan)
#' # Load Holzinger and Swineford (1939) dataset
#' data("HolzingerSwineford1939", package = "lavaan")
#' # Introduce missing data
#' df_complete <- na.omit(HolzingerSwineford1939)
#' amp <- mice::ampute(df_complete, prop = 0.2, mech = "MAR")
#' data_with_missing <- amp$amp
#'
#' # Perform multiple imputation
#' imputed_data <- mice::mice(data_with_missing, m = 3, maxit = 5, seed = 12345, printFlag = FALSE)
#'
#' # fit the Holzinger and Swineford (1939) example model
#' HS_model <- "visual  =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'            speed   =~ x7 + x8 + x9 "
#' # Fit the SEM model without running the model
#' fit_HS <- lavaan::sem(HS_model, data = data_with_missing, do.fit = FALSE)
#' # Fit the SEM model without pooling to each imputed dataset
#' fit_list <- sem_mice(HS_model, imputed_data)
#' fit_list <- dplyr::map_dfr(fit_list, coef, .id = "imputation") |> print()
#' @import mice
#' @importFrom lavaan lavaan sem parameterEstimates
#' @importFrom OpenMx mxModel mxRun
#' @importFrom mice complete as.mira
#' @export
#' @rdname sem_mice
#' @author Davood Tofighi \email{dtofighi@@gmail.com}

sem_mice <- function(model, mids, ...) {
  # Ensure 'mids' is a 'mids' object
  if (!inherits(mids, "mids")) {
    stop("'mids' must be a 'mids' object from the 'mice' package.")
  }

  if (inherits(model, "MxModel")) {
    return(mx_mice(model, mids, ...))
  } else if (is_lav_syntax(model) || inherits(model, "lavaan")) {
    return(lav_mice(model, mids, ...))
  } else {
    stop("The model must be either a 'MxModel' object or a character string representing a lavaan model syntax.")
  }
}
