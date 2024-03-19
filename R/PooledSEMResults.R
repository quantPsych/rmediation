#' Pooled SEM Analysis Results Class
#'
#' An S4 class to represent pooled results from SEM analysis across multiple imputations or datasets.
#' It contains pooled estimates, standard errors, test statistics, p-values, and confidence intervals
#' for each parameter estimated across multiple imputations.
#'
#' @slot tidy_table data.frame A data frame containing the pooled results of the SEM analyses. The column names adhere to tidy conventions and include the following columns:
#' - `term`: The name of the parameter being estimated.
#' - `estimate`: The pooled estimate of the parameter.
#' - `std.error`: The pooled standard error of the estimate.
#' - `p.value`: The pooled p-value for the test statistic.
#' - `conf.low`: The lower bound of the confidence interval for the estimate.
#' - `conf.high`: The upper bound of the confidence interval for the estimate.
#' @slot cov_total matrix The pooled total covariance matrix of the parameter estimates.
#' @slot cov_between matrix The pooled between-imputation covariance matrix of the parameter estimates.
#' @slot cov_within matrix The pooled within-imputation covariance matrix of the parameter estimates.
#' @slot method character The method used for SEM analysis ('lavaan' or 'OpenMx').
#' @slot conf.int logical Whether to calculate confidence intervals for the pooled estimates. default is `FALSE`.
#' @slot conf.level numeric The confidence level used in the interval calculation. default is `0.95`.
#'
#' @import methods
#' @importFrom methods setClass setValidity setMethod validObject
#' @exportClass PooledSEMResults
#' @name PooledSEMResults
#' @rdname PooledSEMResults-class
#' @docType class
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @aliases PooledSEMResults PooledSEMResults-class

PooledSEMResults <- setClass(
  "PooledSEMResults",
  slots = c(
    tidy_table = "data.frame",
    cov_total = "matrix",
    cov_between = "matrix",
    cov_within = "matrix",
    method = "character",
    conf.int = "logical",
    conf.level = "numeric"
  )
)

setValidity("PooledSEMResults", function(object) {
  messages <- character(0)

  # check if results is an empty data frame
  if (!is.data.frame(object@results) || nrow(object@results) == 0 || ncol(object@results) == 0) {
    messages <-
      c(
        messages,
        "The results must be a non-empty data frame."
      )
  }
  requiredColumns <-
    c(
      "term",
      "estimate",
      "std.error",
      "statistic",
      "p.value"
    )
  # Check for required columns
  if (!all(requiredColumns %in% colnames(object@results))) {
    messages <-
      c(
        messages,
        "The results data frame must contain all required columns: term, estimate, std.error, statistic, p.value."
      )
  }
  if (!is.data.frame(object@coef_pool) || nrow(object@coef_pool) == 0 || ncol(object@coef_pool) == 0) {
    messages <-
      c(
        messages,
        "The coef_pool must be a non-empty data frame."
      )
  }
  # check if cov_total is a positive definite symmetric matrix
  # if (!is.matrix(object@cov_total) || nrow(object@cov_total) == 0 || ncol(object@cov_total) == 0) {
  #   messages <-
  #     c(
  #       messages,
  #       "The cov_total must be a non-empty matrix."
  #     )
  # } else if (!isSymmetric(object@cov_total, tol = 1e-8)) {
  #   messages <-
  #     c(
  #       messages,
  #       "The cov_total must be a symmetric matrix."
  #     )
  # } else if (!is_pd(object@cov_total)) {
  #   messages <-
  #     c(
  #       messages,
  #       "The cov_total must be a positive definite matrix."
  #     )
  # }
  # if (length(messages) == 0) {
  #   TRUE
  # } else {
  #   messages
  # }
})


### =============================================
### Methods for the PooledSEMResults Class
### ============================================

### ---------------------------------------------
### Method: pool_sem
### class: "SemResults"
### Returns: PooledSEMResults
### Role: Constructor, Pool results from multiple imputation analyses
### ---------------------------------------------

#' Pool SEM Analysis Results
#'
#' A generic function to pool SEM analysis results from multiple datasets or imputations.
#'
#' @description
#' `pool_sem` pools SEM analysis results, supporting `lavaan` and `OpenMx` models.
#' It calculates pooled estimates, standard errors, confidence intervals, and more.
#'
#' @param object `SemResults` object with SEM analysis results.
#' @param ... Additional arguments for extensions.
#'
#' @return `PooledSEMResults` object containing pooled SEM analysis results.
#' @details Refer to method-specific documentation for details on pooling process and assumptions.
#' @examples
#' \dontrun{
#' # Assuming `sem_results` is a `SemResults` object with `lavaan` model fits:
#' pooled_results <- pool_sem(sem_results, conf.int = TRUE, conf.level = 0.95)
#' print(pooled_results)
#' }
#' @importFrom dplyr mutate select rename contains group_by summarise ungroup
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble
#' @importFrom broom tidy
#' @seealso [lavaan], [OpenMx]
#' @author Davood Tofighi \email{dtofighi@@gmail.com}

setGeneric(
  "pool_sem",
  function(object,
           ...) {
    standardGeneric("pool_sem")
  }
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
#' @rdname pool_sem
#' @aliases pool_sem

setMethod("pool_sem", "SemResults", function(object) {
  if (object@method %in% c("lavaan", "OpenMx")) {
    tidy_table <- pool_tidy(object)
  } else {
    stop(paste(
      "Unsupported method specified in SemResults:",
      object@method
    ))
  }
  cov_res <- pool_cov(object)

  PooledSEMResults(
    tidy_table = tidy_table,
    cov_total = cov_res[["cov_total"]],
    cov_within = cov_res[["cov_within"]],
    cov_between = cov_res[["cov_between"]],
    method = object@method,
    n_imputations = object@n_imputations,
    conf.int = object@conf.int,
    conf.level = object@conf.level
  )
})

### ---------------------------------------------
### Helper functions for pooling results from lavaan and OpenMx objects
### These functions should be customized based on the structure of your lavaan and OpenMx objects and the specific information you need to extract for pooling.
### ---------------------------------------------

pool_tidy <- function(object) {
  ## Extract the relevant information from a SemResults object and return a tidy data frame
  n_imputations <- object@n_imputations
  x <- object@estimate_df
  x |>
    dplyr::group_by(.data$term) |>
    dplyr::summarise(est = mean(.data$estimate), var_b = var(.data$estimate), var_w = mean(.data$std.error^2), var_tot = .data$var_w + .data$var_b * (1 + 1 / n_imputations), se = sqrt(.data$var_tot), p.value = exp(mean(log(.data$p.value)))) |>
    dplyr::ungroup() |>
    dplyr::rename(estimate = .data$est, std.error = .data$se) |>
    dplyr::relocate(.data$term, .data$estimate, .data$std.error, .data$p.value, .data$var_b, .data$var_w, .data$var_tot)
}

pool_cov <- function(object) {
  ## Extract the relevant information from a SemResults object and return a list of covariance matrices
  cov_between <- object@coef_df |>
    dplyr::select(-.imp) |>
    cov()
  cov_within <- Reduce("+", object@cov_df) / object@n_imputations
  cov_total <- cov_between * (1 + 1 / object@n_imputations) + cov_within
  return(list(cov_total = cov_total, cov_between = cov_between, cov_within = cov_within))
}
