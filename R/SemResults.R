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
#' @name SemResults
#' @rdname SemResults-class
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @aliases SemResults SemResults-class
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

  if (!is.logical(object@conf.int) || length(object@conf.int) != 1) {
    messages <-
      c(messages, "conf.int must be a single logical value")
  }

  if (!is.list(object@results) || length(object@results) == 0) {
    messages <-
      c(messages, "results must be a non-empty list")
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

  if (!inherits(results[[1]], "lavaan") &&
    !inherits(results[[1]], "MxModel")) {
    stop("results must contain objects of class 'lavaan' or 'MxModel'")
  }
  .Object@results <- results
  .Object@method <- method
  .Object@conf.int <- conf.int
  .Object@conf.level <- conf.level

  # Further customization based on provided ... arguments could be done here

  validObject(.Object) # Check for object validity after initialization
  return(.Object)
})

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
#' @param model A character string specifying the SEM model
#' @param conf.int A logical value specifying the confidence interval level. Defaults to FALSE.
#' @param conf.level A numeric value specifying the confidence level. Defaults to 0.95.
#' @param ... Additional arguments passed to either [lavaan::sem] or [OpenMx::MxModel].
#' @return A `SemResults` object
#' @usage run_sem(object, model, conf.int, conf.level, ...)
#' @export
#' @rdname run_sem
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @importFrom methods setGeneric

setGeneric(
  "run_sem",
  function(object, model, conf.int = FALSE, conf.level = .95, ...) {
    standardGeneric("run_sem")
  }
)

#' Run SEM Analysis on Imputed Data
#'
#' This method facilitates running SEM analysis using either lavaan or OpenMx
#' on multiply imputed datasets contained within a [SemImputedData-class] object.
#' @export
#' @importFrom methods new setMethod setGeneric
#' @importFrom lavaan sem
#' @importFrom OpenMx mxModel mxRun mxData imxVerifyModel
#' @importFrom purrr map
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
#' sem_data <- impute_sem(data = imputed_data, method = "lavaan")
#' model <- "
#'  visual  =~ x1 + x2 + x3
#'  textual =~ x4 + x5 + x6
#'  speed   =~ x7 + x8 + x9
#'  "
#' ## Note that the model is specified as a string
#' res <- run_sem(sem_data, model)
#' }
setMethod("run_sem", signature(object = "SemImputedData"), function(object, model, conf.int = FALSE, conf.level = .95, ...) {
  if (!inherits(object@data, "mids")) {
    stop("'object@data' must be a 'mids' object from the 'mice' package.")
  }

  # Dynamically select the appropriate function based on the SEM method.
  sem_fn <- switch(tolower(object@method),
    "lavaan" = lav_mice,
    "openmx" = mx_mice,
    stop("Unsupported method specified: ", object@method)
  )

  sem_results <- sem_fn(object@data, model, ...)

  # Ensure results are returned in a consistent format.
  new("SemResults", results = sem_results, method = object@method, conf.int = conf.int, conf.level = conf.level)
})


### ----------------------------------------------------------------------------
### Helper functions for run_sem method
### ----------------------------------------------------------------------------
lav_mice <- function(mids, model, ...) {
  # Extract complete imputed datasets
  sem_results <-
    mice::complete(mids, action = "all") |> purrr::map(lavaan::sem, model = model, ...)
  return(sem_results)
}

mx_mice <- function(mids, model, ...) {
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
  sem_results <- data_complete |> purrr::map(\(df) {
    mxDataObj <- OpenMx::mxData(df, type = "raw")
    updatedModel <- OpenMx::mxModel(model, mxDataObj)
    OpenMx::mxRun(updatedModel, ...)
  })
  return(sem_results)
}
