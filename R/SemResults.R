#' @title SemResults Class
#'
#' @description An S4 class for storing the results of SEM analysis
#' performed on multiply imputed datasets. Supports `lavaan` and `OpenMx`.
#'
#' @slot results A list of SEM model fits for each imputed dataset.
#' @slot estimate_df Data frame of parameter estimates and standard errors.
#' @slot coef_df Data frame of coefficient estimates for each imputed dataset.
#' @slot cov_df List of covariance matrices of coefficient estimates.
#' @slot method SEM package used for analysis: 'lavaan' or 'OpenMx'.
#' @slot conf_int Logical; if confidence intervals are included.
#' @slot conf_level Confidence level for confidence intervals.
#' @import methods
#' @exportClass SemResults
#' @docType class
#' @name SemResults
#' @rdname SemResults-class
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
# Define the SemResults S4 class
SemResults <- setClass(
  "SemResults",
  slots = c(
    results = "list",
    estimate_df = "data.frame",
    coef_df = "data.frame",
    cov_df = "list",
    method = "character",
    conf_int = "logical",
    conf_level = "numeric"
  ),
  prototype = list(
    results = list(),
    estimate_df = data.frame(),
    coef_df = data.frame(),
    cov_df = list(),
    method = "lavaan",
    conf_int = FALSE,
    conf_level = 0.95
  )
)

# Set validity method for SemResults
setValidity("SemResults", function(object) {
  messages <- character(0)

  if (!is.list(object@results) || length(object@results) == 0) {
    messages <- c(messages, "results must be a non-empty list.")
  }
  if (!is.data.frame(object@estimate_df)) {
    messages <- c(messages, "estimate_df must be a data frame.")
  }
  if (!is.data.frame(object@coef_df)) {
    messages <- c(messages, "coef_df must be a data frame.")
  }
  if (!is.list(object@cov_df) || length(object@cov_df) == 0) {
    messages <- c(messages, "cov_df must be a non-empty list.")
  }
  if (!object@method %in% c("lavaan", "OpenMx")) {
    messages <-
      c(messages, "method must be either 'lavaan' or 'OpenMx'.")
  }
  if (!is.logical(object@conf_int) ||
    length(object@conf_int) != 1) {
    messages <- c(messages, "conf_int must be a single logical value.")
  }
  if (object@conf_int) {
    if (!is.numeric(object@conf_level) ||
      object@conf_level <= 0 || object@conf_level >= 1) {
      messages <-
        c(messages, "conf_level must be a numeric value between 0 and 1.")
    }
  }
  # Add validations here based on the above corrections

  if (length(messages) == 0) {
    TRUE
  } else {
    messages
  }
})

### ============================================================================
### Methods for the SemResults class
### ============================================================================

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
#'
#' @return `PooledSEMResults` object containing pooled SEM analysis results.
#' @details Refer to method-specific documentation for details on pooling process and assumptions.
#' @importFrom dplyr mutate select rename contains group_by summarise ungroup
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble
#' @importFrom broom tidy
#' @export
#' @seealso [lavaan], [OpenMx]
#' @author Davood Tofighi \email{dtofighi@@gmail.com}

setGeneric(
  "pool_sem",
  function(object) {
    standardGeneric("pool_sem")
  }
)

#' Pool SEM Results from Multiple Imputations
#'
#' This function pools the results of structural equation modeling (SEM)
#' analyses performed on multiple imputed datasets.
#' It supports pooling for models analyzed with either the \code{lavaan}
#' or \code{OpenMx} package.  The function extracts and pools
#' relevant statistics (e.g., estimates, standard errors) across
#' all imputations, considering the specified confidence interval settings.
#'
#' @return A \code{data.frame} containing the pooled results of the SEM analyses. The column names adhere to tidy conventions and include the following columns:
#'   - `term`: The name of the parameter being estimated.
#'   - `estimate`: The pooled estimate of the parameter.
#'   - `std_error`: The pooled standard error of the estimate.
#'   - `statistic`: The pooled test statistic (e.g., z-value, t-value).
#'   - `p_value`: The pooled p-value for the test statistic.
#'   - `conf_low`: The lower bound of the confidence interval for the estimate.
#'   - `conf_high`: The upper bound of the confidence interval for the estimate.
#'
#' @examples
#' \dontrun{
#' # Assuming `sem_results` is a SemResults object with lavaan model fits:
#' pooled_results <- pool_sem(sem_results)
#'
#' # If you want to calculate and include confidence intervals at a 95% level:
#' pooled_results_ci <- pool_sem(sem_results, conf_int = TRUE, conf_level = 0.95)
#' }
#'
#' @export
#' @rdname pool_sem
#' @aliases pool_sem

setMethod("pool_sem", signature = "SemResults", function(object) {
  if (!object@method %in% c("lavaan", "OpenMx")) {
    stop(
      "Unsupported method specified in SemResults: ",
      object@method
    )
  }

  # Assuming pool_tidy and pool_cov are correctly implemented
  tidy_table <- pool_tidy(object)
  cov_res <- pool_cov(object)

  # Ensure PooledSEMResults class is defined with the correct slots
  PooledSEMResults(
    tidy_table = tidy_table,
    cov_total = cov_res$cov_total,
    cov_between = cov_res$cov_between,
    cov_within = cov_res$cov_within,
    method = object@method,
    conf_int = object@conf_int,
    conf_level = object@conf_level
  )
})

### ============================================================================
### Helper functions for pooling results from lavaan and OpenMx objects
### These functions should be customized based on the structure of your lavaan and OpenMx objects and the specific information you need to extract for pooling.
### ============================================================================

#' Pool Tidy Results
#'
#' This function extracts and pools relevant statistics (e.g., estimates, standard errors) across all imputations, considering the specified confidence interval settings, and returns a tidy data frame.
#' @param object `SemResults` object with SEM analysis results.
#' @return A `data.frame` containing the pooled results of the SEM analyses.
#' @details This function extracts and pools relevant statistics (e.g., estimates, standard errors) across all imputations, considering the specified confidence interval settings.
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @importFrom broom tidy
#' @keywords internal
#' @noRd
setGeneric(
  "pool_tidy",
  function(object) {
    standardGeneric("pool_tidy")
  }
)

setMethod("pool_tidy", signature = "SemResults", function(object) {
  ## Extract the relevant information from a SemResults object and return a tidy data frame
  n_imputations <- length(object@results)
  x <- object@estimate_df
  x |>
    dplyr::group_by(.data$term) |>
    dplyr::summarise(
      est = mean(.data$estimate),
      var_b = var(.data$estimate),
      var_w = mean(.data$std_error^2),
      var_tot = .data$var_w + .data$var_b * (1 + 1 / n_imputations),
      se = sqrt(.data$var_tot),
      p_value = exp(mean(log(.data$p_value)))
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(estimate = .data$est, std_error = .data$se) |>
    dplyr::relocate(
      .data$term,
      .data$estimate,
      .data$std_error,
      .data$p_value,
      .data$var_b,
      .data$var_w,
      .data$var_tot
    ) |>
    tibble::as_tibble()
})


#' Pool Covariance Matrices
#'
#' This function extracts and pools relevant covariance matrices across all imputations. It returns a list of covariance matrices.
#' @param object `SemResults` object with SEM analysis results.
#' @return A list of covariance matrices.
#' @details This function extracts and pools relevant covariance matrices across all imputations.
#' @keywords internal
#' @noRd
#' @importFrom dplyr select
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @importFrom broom tidy
#' @importFrom stats cov

setGeneric(
  "pool_cov",
  function(object) {
    standardGeneric("pool_cov")
  }
)

setMethod("pool_cov", signature = "SemResults", function(object) {
  ## Extract the relevant information from a SemResults object and return a list of covariance matrices
  n_imputations <- length(object@results)
  cov_between <- object@coef_df |>
    dplyr::select(-.data$.imp) |>
    cov()
  cov_within <- Reduce("+", object@cov_df) / n_imputations
  cov_total <- cov_between * (1 + 1 / n_imputations) + cov_within
  return(list(
    cov_total = cov_total,
    cov_between = cov_between,
    cov_within = cov_within
  ))
})
