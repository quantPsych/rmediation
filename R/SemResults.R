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
