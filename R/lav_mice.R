#' Fit SEM Model to Each Dataset in a MIDS Object Without Pooling
#'
#' Fits a SEM model to each dataset in a [mice::mids] object without pooling the results.
#' This function is an extension for the [lavaan::sem()] function to handle [mice::mids] objects from the [mice] package.
#' It allows for both a SEM model syntax as a character string or a pre-fitted [lavaan] model object.
#'
#' @param model Either a character string representing the SEM model to be fitted or a pre-fitted [lavaan] model object.
#' @param mids A [mice::mids] object from the [mice] package.
#' @param ... Additional arguments to be passed to [lavaan::sem()].
#' @return A list of [lavaan] model fits, one for each imputed dataset.
#' @export
#' @import mice
#' @importFrom lavaan sem parameterEstimates
#' @importFrom mice mice complete pool as.mira
#' @importFrom stats update
#' @importFrom dplyr bind_rows select
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
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
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 "
#' # Fit the SEM model without running the model
#' fit_HS <- lavaan::sem(HS_model, data = data_with_missing, do.fit = FALSE)
#' # Fit the SEM model without pooling to each imputed dataset
#' fit_list1 <- lav_mice(HS_model, imputed_data)
#' # 'fit_list1' now contains a list of lavaan objects, one for each imputed dataset
#' # Fit the SEM model without pooling to each imputed dataset using a pre-fitted model object
#' fit_list2 <- lav_mice(fit_HS, imputed_data)
#' # 'fit_list2' now contains a list of lavaan objects, one for each imputed dataset
#' }
lav_mice <- function(model, mids, ...) {
  # Ensure 'mids' is a 'mids' object from the 'mice' package
  if (!inherits(mids, "mids")) {
    stop("'mids' must be a 'mids' object from the 'mice' package.")
  }

  # Ensure 'model' is either a character string or a lavaan model object
  ## if (!RMediation::is_valid_lav_syntax(model, mids$data)) {
  ##  stop("The model is not a valid lavaan model syntax.")
  ## }
  # Determine if 'model' is a character string or a lavaan model
  ## is_lav_object <- inherits(model, "lavaan")

  # Extract complete imputed datasets
  data_complete <- mice::complete(mids, action = "all")
  sem_results <-
    data_complete |> purrr::map(lavaan::sem, model = model, ...)
  # Covert it to mice::mira object to be able to use pool function
  sem_results <- mice::as.mira(sem_results)

  class(sem_results) <- c("semlist", "lav", "list", "mira")
  # Return list of SEM model fits
  return(sem_results)
}
