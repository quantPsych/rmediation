#' SemResults Class
#'
#' An S4 class for storing the results of structural equation modeling analysis
#' performed on multiply imputed datasets. This class can accommodate results
#' from [lavaan], [OpenMx], or potentially other SEM software, providing
#' a unified interface to access and manipulate SEM results.
#'
#' @slot results A list of SEM model fits, where each element represents the
#'   results from one imputed dataset. The specific content and structure of
#'   each element can vary based on the SEM method used (e.g., objects of
#'   class `lavaan` or `MxModel`). The `results` slot is the primary storage
#'   location for SEM results, and it is expected to be a list of length equal
#'   to the number of imputed datasets.
#' @slot method A character string specifying the SEM package used for the
#'   analysis, with valid options being "lavaan" for `lavaan` model fits
#'   or "OpenMx" for `OpenMx` model fits. Defaults to "lavaan".
#' @slot conf.int A logical value indicating whether confidence intervals are
#'   included in the SEM results. Defaults to `FALSE`.
#' @slot conf.level A numeric value specifying the confidence level for
#'   confidence intervals, which must be between 0 and 1. Defaults to 0.95.
#' @exportClass SemResults
#' @docType class
#' @import methods
#' @importFrom methods setClass setValidity setMethod validObject
#' @name SemResults-class
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @seealso [SemImputedData-class]

setClass(
  "SemResults",
  slots = list(
    results = "list",
    # Holds SEM model fits
    method = "character",
    # Indicates the SEM analysis method used
    conf.int = "logical",
    # Indicates whether confidence intervals are included
    conf.level = "numeric" # Specifies the confidence level
  )
)

setValidity("SemResults", function(object) {
  messages <- character()
  if (!object@method %in% c("lavaan", "OpenMx")) {
    messages <-
      c(messages, "method must be either 'lavaan' or 'OpenMx'")
  }
  if (length(object@conf.level) != 1 ||
    object@conf.level <= 0 || object@conf.level >= 1) {
    messages <-
      c(messages, "conf.level must be a numeric value between 0 and 1")
  }
  if (length(messages) == 0) {
    TRUE
  } else {
    messages
  }
})

setMethod("initialize", "SemResults", function(.Object,
                                               results,
                                               method = "lavaan",
                                               conf.int = FALSE,
                                               conf.level = 0.95,
                                               ...) {
  # Basic validation could be performed here if needed
  if (!method %in% c("lavaan", "OpenMx")) {
    stop("Method must be either 'lavaan' or 'OpenMx'")
  }

  if (!is.logical(conf.int) || length(conf.int) != 1) {
    stop("conf.int must be a single logical value")
  }

  if (!is.numeric(conf.level) ||
    length(conf.level) != 1 || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a single numeric value between 0 and 1")
  }

  .Object@results <- results
  .Object@method <- method
  .Object@conf.int <- conf.int
  .Object@conf.level <- conf.level

  # Further customization based on provided ... arguments could be done here

  validObject(.Object) # Check for object validity after initialization
  return(.Object)
})

### =============================================
### Methods for the SemResults class
### ============================================

### ---------------------------------------------
### run_sem method: Constructor Method
### ---------------------------------------------
#' @title Run a SEM model
#'
#' @description
#' A generic function to run and analyze multiply imputed data sets.
#'
#' @param object A `SemImputedData` object
#' @param model A character string specifying the SEM model
#' @param conf.int A numeric value specifying the confidence interval level. Defaults to FALSE.
#' @param conf.level A numeric value specifying the confidence level. Defaults to 0.95.
#' @param ... Additional arguments passed to either [lavaan::sem] or [OpenMx::MxModel].
#' @return A `SemResults` object
#' @usage run_sem(object, model, conf.int, conf.level, ...)
#' @export
#' @rdname run_sem
#' @aliases run_sem
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @importFrom methods setGeneric

setGeneric(
  "run_sem",
  function(object, model, conf.int = FALSE, conf.level = .95, ...) {
    standardGeneric("run_sem")
  }
)

#' @title Run SEM Analysis on Imputed Data
#'
#' @description
#' This method facilitates running SEM analysis using either lavaan or OpenMx
#' on multiply imputed datasets contained within a [SemImputedData-class] object.
#' @export
#' @importFrom mice complete as.mira
#' @importFrom methods new setMethod setGeneric
#' @importFrom lavaan sem
#' @importFrom OpenMx mxModel mxRun mxData imxVerifyModel
#' @importFrom purrr map
#' @rdname run_sem
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @seealso [SemImputedData-class]
#' @examples
#' \dontrun{
#' # Load Holzinger and Swineford (1939) dataset
#' data("HolzingerSwineford1939", package = "lavaan")
#' # Introduce missing data
#' df_complete <- na.omit(HolzingerSwineford1939[paste0("x", 1:9)])
#' amp <- mice::ampute(df_complete, prop = 0.1, mech = "MAR")
#' data_with_missing <- amp$amp
#' # Perform multiple imputation
#' imputed_data <- mice::mice(data_with_missing, m = 3, maxit = 3, seed = 12345, printFlag = FALSE)
#' sem_data <- impute_sem(data = imputed_data, method = "lavaan")
#' model <- "
#'  visual  =~ x1 + x2 + x3
#'  textual =~ x4 + x5 + x6
#'  speed   =~ x7 + x8 + x9
#'  "
#' ## Note that the model is specified as a string
#' res <- run_sem(sem_data, model)
#' lapply(res@results, summary)
#' }
setMethod("run_sem", "SemImputedData", function(object, model, conf.int = FALSE, conf.level = .95, ...) {
  # Ensure 'mids' is a 'mids' object from the 'mice' package
  if (!inherits(object@data, "mids")) {
    stop("'mids' must be a 'mids' object from the 'mice' package.")
  }
  if (object@method == "lavaan") {
    sem_list <- lav_mice(model, object@data, ...)
  } else if (object@method == "OpenMx") {
    sem_list <- mx_mice(model, object@data, ...)
  } else {
    stop("Unsupported method specified in SemImputedData")
  }
  # Create a SemResults object with the collected SEM fits
  sem_results_object <- new("SemResults", results = sem_list$analyses, method = object@method, conf.int = conf.int, conf.level = conf.level)

  return(sem_results_object)
})

lav_mice <- function(model, mids, ...) {
  # Extract complete imputed datasets
  data_complete <- mice::complete(mids, action = "all")
  sem_results <-
    data_complete |> purrr::map(lavaan::sem, model = model, ...)
  # Covert it to mice::mira object to be able to use pool function
  sem_results <- mice::as.mira(sem_results)
  # Return list of SEM model fits
  return(sem_results)
}

mx_mice <- function(model, mids, ...) {
  # Ensure 'mxModel' is an OpenMx model object
  if (!inherits(model, "MxModel")) {
    stop("'model' must be an 'MxModel' object from the 'OpenMx' package.")
  }
  verified <- OpenMx::imxVerifyModel(model)
  if (!verified) {
    stop("The mxModel object failed verification.")
  }
  # Extract complete imputed datasets
  data_complete <- mice::complete(mids, action = "all")
  # Fit the model to each imputed dataset
  mx_results <- data_complete |> purrr::map(\(df) {
    mxDataObj <- OpenMx::mxData(df, type = "raw")
    updatedModel <- OpenMx::mxModel(model, mxDataObj)
    OpenMx::mxRun(updatedModel, ...)
  })
  # Convert the list of OpenMx model fits to a mira object
  mx_results <- mice::as.mira(mx_results)
  return(mx_results)
}


### ---------------------------------------------
### pool_sem method: Extractor Method
### ---------------------------------------------

#' @title Pool SEM Analysis Results
#'
#' @description
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
