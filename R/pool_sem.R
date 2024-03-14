#' Pool SEM Analysis Results
#'
#' A generic function to pool SEM analysis results from multiple datasets or imputations,
#' offering a unified interface for summarizing findings across different SEM models
#' or imputations. Supports custom implementations for various result types, including
#' those from `lavaan` and `OpenMx`.
#'
#' @param object A `SemResults` object containing the SEM analysis results from
#'   multiple imputations, along with the SEM method used (`lavaan` or `OpenMx`),
#'   and confidence interval settings.
#' @param conf.int Logical; indicates whether confidence intervals should be
#'   calculated for the pooled estimates. Defaults to the value specified in the
#'   `SemResults` object.
#' @param conf.level Numeric; specifies the confidence level to use for the
#'   confidence interval calculations, if `conf.int` is `TRUE`. Defaults to the
#'   value specified in the `SemResults` object.
#' @param ... Additional arguments for future use or extensions.
#'
#' @return A summary object with pooled SEM analysis results, which might include
#'   estimates, standard errors, confidence intervals, etc., depending on the method.
#' @details Refer to method-specific documentation for details on pooling process and assumptions.
#' @usage pool_sem(object, conf.int, conf.level, ...)
#' @examples
#' \dontrun{
#' pooled_lavaan <- pool_sem(lavaan_results) # For `lavaan` results
#' pooled_openmx <- pool_sem(mx_results) # For `OpenMx` results
#' }
#' @export
#' @importFrom dplyr mutate select rename contains group_by summarise ungroup
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble
#' @seealso [lavaan], [OpenMx]
#' @rdname pool_sem
#' @author Davood Tofighi \email{dtofighi@@gmail.com}

setGeneric(
  "pool_sem",
  function(object, conf.int, conf.level, ...) standardGeneric("pool_sem")
)

#' Pool SEM Results from Multiple Imputations
#'
#' This function pools the results of structural equation modeling (SEM) analyses
#' performed on multiple imputed datasets. It supports pooling for models analyzed
#' with either the \code{lavaan} or \code{OpenMx} package. The function extracts and pools
#' relevant statistics (e.g., estimates, standard errors) across all imputations,
#' considering the specified confidence interval settings.
#'
#' @return A \code{data.frame} containing the pooled results of the SEM analyses. The
#'  column names adhere to tidy conventions and include the following columns:
#'   - `term`: The name of the parameter being estimated.
#'   - `estimate`: The pooled estimate of the parameter.
#'   - `std.error`: The pooled standard error of the estimate.
#'   - `statistic`: The pooled test statistic (e.g., z-value, t-value).
#'   - `p.value`: The pooled p-value for the test statistic.
#'   - `conf.low`: The lower bound of the confidence interval for the estimate.
#'   - `conf.high`: The upper bound of the confidence interval for the estimate.
#'
#' @examples
#' \dontrun{
#' # Assuming `sem_results` is a SemResults object with lavaan model fits:
#' pooled_results <- pool_sem(sem_results)
#'
#' # If you want to calculate and include confidence intervals at a 95% level:
#' pooled_results_ci <- pool_sem(sem_results, conf.int = TRUE, conf.level = 0.95)
#' }
#'
#' @export
#' @seealso \code{\link[lavaan]{lavaan}}, \code{\link[OpenMx]{mxModel}}
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @rdname pool_sem
#' @aliases pool_sem

setMethod("pool_sem", "SemResults", function(object, conf.int, conf.level, ...) {
  if (object@method == "lavaan") {
    # Assuming a function to extract fit indices from a lavaan object
    lapply(
      object@results,
      extract_lav,
      conf.int = object@conf.int,
      conf.level = object@conf.level
    ) # Note: ensure the function names used here also follow snake_case convention
  } else if (object@method == "OpenMx") {
    # Similar for OpenMx
    lapply(
      object@results,
      extract_mx,
      conf.int = object@conf.int,
      conf.level = object@conf.level
    ) # Adjusted to snake_case
  } else {
    stop("Unsupported method specified in SemResults")
  }
})

## Helper functions for pooling results from lavaan and OpenMx objects
## These functions should be customized based on the structure of your lavaan and OpenMx objects
## and the specific information you need to extract for pooling.

extract_lav <- function(fit,
                        conf.int = FALSE,
                        conf.level = 0.95) {
  # Extract the relevant information from a lavaan object
  # This function should be customized based on the structure of your lavaan objects
  # and the specific information you need to extract for pooling

  nimp <- length(fit)
  pooled_est <- fit |>
    purrr::map_dfr(broom::tidy, conf.int = conf.int, .id = "imp") |>
    dplyr::select(
      .data$term,
      .data$estimate,
      .data$std.error,
      .data$statistic,
      .data$p.value
    ) |>
    dplyr::group_by(.data$term) |>
    dplyr::summarise(
      q_est = mean(.data$estimate),
      bet_var = var(.data$estimate),
      w_var = sum(.data$std.error^2) / nimp
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(tot_var = .data$w_var + .data$bet_var * (1 + 1 / nimp))
  return(pooled_est)
}

extract_mx <- function(fit,
                       conf.int = FALSE,
                       conf.level = 0.95) {
  # Extract the relevant information from a MxModel object
  # This function should be customized based on the structure of your lavaan objects
  # and the specific information you need to extract for pooling

  nimp <- length(fit)
  pooled_est <- fit |>
    purrr::map_dfr(RMediation::tidy, conf.int = conf.int, .id = "imp") |>
    dplyr::select(
      .data$term,
      .data$estimate,
      .data$std.error,
      .data$statistic,
      .data$p.value
    ) |>
    dplyr::group_by(.data$term) |>
    dplyr::summarise(
      q_est = mean(.data$estimate),
      bet_var = var(.data$estimate),
      w_var = sum(.data$std.error^2) / nimp
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(tot_var = .data$w_var + .data$bet_var * (1 + 1 / nimp))
  return(pooled_est)
}
