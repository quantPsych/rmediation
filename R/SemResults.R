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
