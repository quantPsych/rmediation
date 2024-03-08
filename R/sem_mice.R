#' Fit a structural equation model to multiply imputed data
#'
#' This function fits a structural equation model to multiply imputed data. It
#' is an extension for the `lavaan::sem()` and `OpenMx::MxModel` functions to
#' handle `mice::mids` objects from the `mice` package. It allows for both a
#' structural equation model syntax as a character string or a pre-fitted
#' `lavaan` or `OpenMx` model object.
#'
#' @param model Either a character string representing the structural equation
#'  model to be fitted or a pre-fitted `lavaan` or `OpenMx` model object.
#' @param mids A `mice::mids` object from the `mice` package.
#' @param ... Additional arguments to be passed to `lavaan::sem()` or
#' `OpenMx::mxRun`.
#' @return A list of `lavaan` or `OpenMx` model fits, one for each imputed
#' dataset.
#' @examples
#' \dontrun{
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
#' fit_list1 <- sem_mice(HS_model, imputed_data)
#' # 'fit_list1' now contains a list of lavaan objects, one for each imputed dataset
#' # Fit the SEM model without pooling to each imputed dataset using a pre-fitted model object
#' fit_list2 <- sem_mice(fit_HS, imputed_data)
#'
#' # Assuming mx_mice is correctly defined in your environment
#' fits <- mx_mice(model, imputed_data)
#' summary(fits[[1]])
#' }
#' @import mice
#' @importFrom lavaan lavaan sem parameterEstimates
#' @importFrom dplyr case_when
#' @importFrom OpenMx mxModel mxRun
#' @importFrom lavaan sem
#' @importFrom OpenMx mxRun
#' @importFrom mice complete
#' @importFrom lavaan parameterEstimates
#' @importFrom stats update
#' @importFrom dplyr bind_rows select
#' @export
#' @rdname sem_mice
#' @author Davood Tofighi \email{dtofighi@@gmail.com}



sem_mice <- function(model, mids, ...) {
    # Ensure 'mids' is a 'mids' object
    if (!inherits(mids, "mids")) {
        stop("'mids' must be a 'mids' object from the 'mice' package.")
    }
    # Ensure 'mxModel' is an OpenMx model object

    dplyr::case_when(
        inherits(model, "MxModel") ~ mx_mice(model, mids, ...),
        is_lav_syntax(model, mids$data) ~ lav_mice(model, mids, ...),
        TRUE ~ stop("The model is not a valid lavaan or OpenMx model syntax.")
    )
}
