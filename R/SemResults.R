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
#' @slot esimate_df A data frame containing the parameter estimates and standard errors, statistics, p values from running the SEM model on the imputed data.
#' @slot coef_df A data frame containing $m$ rows of the coefficient estimates of the SEM model for each imputed dataset.
#' @slot cov_df A list of $m$ the covariance matrices of the coefficient estimates of the SEM model for each imputed dataset. These are "within-imputation" covariance matrices.
#' @slot method A character string specifying the SEM package used for the
#'   analysis, with valid options being "lavaan" for `lavaan` model fits
#'   or "OpenMx" for `OpenMx` model fits. This slot inherits from the `SemImputedData` class.
#' @slot conf.int A logical value indicating whether confidence intervals are
#'   included in the SEM results. Defaults to `FALSE`. This slot inherits from the `SemImputedData` class.
#' @slot conf.level A numeric value specifying the confidence level for
#'   confidence intervals, which must be between 0 and 1. Defaults to 0.95. This slot inherits from the `SemImputedData` class.
#' @exportClass SemResults
#' @docType class
#' @name SemResults
#' @rdname SemResults-class
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @aliases SemResults SemResults-class
SemResults <- setClass(
  "SemResults",
  slots = c(
    results = "list", # Holds SEM model fits
    estimate_df = "data.frame", # Holds parameter estimates and standard errors
    coef_df = "data.frame", # Holds coefficient estimates
    cov_df = "list", # Holds covariance matrices
    method = "character", # Indicates the SEM analysis method used
    conf.int = "logical", # Indicates whether confidence intervals are included
    conf.level = "numeric" # Specifies the confidence level
  )
)

setValidity("SemResults", function(object) {
  messages <- character()

  # check if the results slot is a list and not empty
  if (!is.list(object@results) || length(object@results) == 0) {
    messages <-
      c(messages, "results must be a non-empty list")
  }
  # chech if estimate_df is a data frame
  if (!is.data.frame(object@estimate_df)) {
    messages <-
      c(messages, "estimate_df must be a data frame")
  }
  # chech if coef_df is a data frame
  if (!is.data.frame(object@coef_df)) {
    messages <-
      c(messages, "coef_df must be a data frame")
  }
  # check if cov_df is a non-empty list
  if (!is.list(object@cov_df) || length(object@cov_df) == 0) {
    messages <-
      c(messages, "cov_df must be a non-empty list")
  }
  if (!object@method %in% c("lavaan", "OpenMx")) {
    messages <-
      c(messages, "method must be either 'lavaan' or 'OpenMx'")
  }
  if (length(object@conf.level) != 1 ||
    object@conf.level <= 0 || object@conf.level >= 1) {
    messages <-
      c(messages, "conf.level must be a numeric value between 0 and 1")
  }
  if (!is.logical(object@conf.int) || length(object@conf.int) != 1) {
    messages <-
      c(messages, "conf.int must be a single logical value")
  }
  if (length(object@results) == 0) {
    messages <-
      c(messages, "results must be a non-empty list")
  }

  if (length(messages) == 0) {
    TRUE
  } else {
    messages
  }
})
#
# setMethod("initialize", "SemResults", function(.Object,
#                                                results = list(),
#                                                estimate_df = data.frame(),
#                                                coef_df = data.frame(),
#                                                cov_df = list(),
#                                                method = "lavaan",
#                                                conf.int = FALSE,
#                                                conf.level = 0.95,
#                                                ...) {
#   # check if the results is a non-empty list and it's element classes are either "lavaan" or "MxModel"
#   if (!is.list(results) || length(results) == 0) {
#     stop("results must be a non-empty list")
#   }
#   if (!all(sapply(results, \(x) inherits(x, "lavaan") || inherits(x, "MxModel")))) {
#     stop("results must contain objects of class 'lavaan' or 'MxModel'")
#   }
#   n_imp <- length(results) # number of imputed datasets
#   # Check if the estimate_df is a data frame with the following columns, "term", "estimate", "std.error", "statistic", "p.value"
#
#   required_cols <- c("term", "estimate", "std.error", "statistic", "p.value")
#   if (!is.data.frame(estimate_df) || !all(required_cols %in% colnames(estimate_df))) {
#     stop("estimate_df must be a data frame with columns 'term', 'estimate', 'std.error', 'statistic', 'p.value'")
#   }
#   # Check if the coef_df is a non-empty data frame and has n_imp rows
#   if (!is.data.frame(coef_df) || nrow(coef_df) != n_imp) {
#     stop(paste0("coef_df must be a non-empty data frame with ", n_imp, " rows"))
#   }
#   # check cov_df is a non-empty list and has n_imp elements
#   if (!is.list(cov_df) || length(cov_df) != n_imp) {
#     stop(paste0("cov_df must be a non-empty list with ", n_imp, " elements"))
#   }
#   # Basic validation could be performed here if needed
#   if (!method %in% c("lavaan", "OpenMx")) {
#     stop("Method must be either 'lavaan' or 'OpenMx'")
#   }
#   # check if conf.int is a single logical value
#   if (!is.logical(conf.int) || length(conf.int) != 1) {
#     stop("conf.int must be a single logical value")
#   }
#   # check if conf.level is a single numeric value between 0 and 1
#   if (!is.numeric(conf.level) ||
#     is.numeric(conf.level) != 1 || conf.level <= 0 || conf.level >= 1) {
#     stop("conf.level must be a single numeric value between 0 and 1")
#   }
#
#   .Object@results <- results
#   .Object@estimate_df <- estimate_df
#   .Object@coef_df <- coef_df
#   .Object@cov_df <- cov_df
#   .Object@method <- method
#   .Object@conf.int <- conf.int
#   .Object@conf.level <- conf.level
#
#   return(.Object)
# })

### ============================================================================
### Methods for the SemResults class
### ============================================================================


### ----------------------------------------------------------------------------
### run_sem method
### ----------------------------------------------------------------------------
#' Run a SEM model
#'
#' A generic function to run and analyze multiply imputed data sets.
#'
#' @param object A `SemImputedData` object
#' @param ... Additional arguments passed to either [lavaan::sem] or [OpenMx::MxModel].
#' @return A `SemResults` object
#' @usage run_sem(object, ...)
#' @export
#' @rdname run_sem
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @importFrom methods setGeneric

setGeneric(
  "run_sem",
  function(object, ...) {
    standardGeneric("run_sem")
  }
)

#' Run SEM Analysis on Imputed Data
#'
#' This method facilitates running SEM analysis using either lavaan or OpenMx
#' on multiply imputed datasets contained within a [SemImputedData-class] object.
#' @export
#' @rdname run_sem
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
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
#' sem_data <- set_sem(data = imputed_data, method = "lavaan")
#' model <- "
#'  visual  =~ x1 + x2 + x3
#'  textual =~ x4 + x5 + x6
#'  speed   =~ x7 + x8 + x9
#'  "
#' ## Note that the model is specified as a string
#' res <- run_sem(sem_data, model)
#' }
setMethod("run_sem", "SemImputedData", function(object, ...) {
  if (!inherits(object@data, "mids")) {
    stop("'object@data' must be a 'mids' object from the 'mice' package.")
  }

  # Dynamically select the appropriate function based on the SEM method.
  sem_fn <- switch(tolower(object@method),
    "lavaan" = lav_mice,
    "openmx" = mx_mice,
    stop("Unsupported method specified: ", object@method)
  )

  # Run the SEM model on the imputed datasets
  sem_results <- sem_fn(object@data, object@model, ...)

  # Extract the results from the imputed datasets
  vcov_sem <- switch(tolower(object@method),
    "lavaan" = vcov_lav,
    "openmx" = vcov,
    stop("Unsupported method specified: ", object@method)
  )

  coef_sem <- switch(tolower(object@method),
    "lavaan" = lavaan::coef,
    "openmx" = coef,
    stop("Unsupported method specified: ", object@method)
  )
  # Extract the tidy results from the estimated SEM models
  estimate_df <- purrr::map_dfr(sem_results, tidy, .id = ".imp") # long tidy table of estimates across imputed datasets
  # Extract the coefficients from the estimated SEM models
  coef_df <- purrr::map_dfr(sem_results, coef_sem, .id = ".imp") # long table of coefficients across imputed datasets
  # Extract the sampling covariance (within covariance) matrices from the estimated SEM models
  cov_df <- purrr::map(sem_results, vcov_sem) # list coefficients estimates sampling covariances across imputed datasets

  # Create a new SemResults object
  SemResults(results = sem_results, estimate_df = estimate_df, coef_df = coef_df, cov_df = cov_df, method = object@method, conf.int = object@conf.int, conf.level = object@conf.level)
})


### ----------------------------------------------------------------------------
### Helper internal functions for run_sem method
### ----------------------------------------------------------------------------
lav_mice <- function(data, model, ...) {
  # Extract complete imputed datasets
  sem_results <-
    mice::complete(data, action = "all") |> purrr::map(lavaan::sem, model = model, ...)
  return(sem_results)
}

mx_mice <- function(data, model, ...) {
  # Ensure 'mxModel' is an OpenMx model object
  if (!inherits(model, "MxModel")) {
    stop("'model' must be an 'MxModel' object from the 'OpenMx' package.")
  }
  verified <- OpenMx::imxVerifyModel(model)
  if (!verified) {
    stop("The mxModel object failed verification.")
  }
  # Extract complete imputed datasets
  data_complete <- mice::complete(data, action = "all")
  # Fit the model to each imputed dataset
  sem_results <- data_complete |> purrr::map(\(df) {
    mxDataObj <- OpenMx::mxData(df, type = "raw")
    updatedModel <- OpenMx::mxModel(model, mxDataObj)
    OpenMx::mxRun(updatedModel)
  })
  return(sem_results)
}
