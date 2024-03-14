#' MediationCI Class
#'
#' An S4 class to represent the results of mediation analysis, including
#' the mediated effect estimate, confidence interval, and additional
#' statistics.
#'
#' @slot method character The method used to calculate the confidence interval.
#' @slot conf.level numeric The confidence level used in the interval calculation.
#' @slot estimate numeric The point estimate of the mediated effect.
#' @slot conf.low numeric The lower bound of the confidence interval.
#' @slot conf.high numeric The upper bound of the confidence interval.
#' @slot se numeric The standard error of the mediated effect estimate.
#' @slot model ANY An object of class 'lavaan' or 'MxModel' representing the SEM model.
#' @slot additional list A slot to hold any additional results or diagnostics.
#' @name MediationCI-class
#' @exportClass MediationCI
#' @seealso [print], [summary]
#' @keywords internal
setClass(
  "MediationCI",
  slots = list(
    method = "character",
    conf.level = "numeric",
    estimate = "numeric",
    conf.low = "numeric",
    conf.high = "numeric",
    se = "numeric",
    model = "ANY", # Stores SEM model object from lavaan or OpenMx
    additional = "list" # Holds additional results or diagnostics
  )
)

setValidity("MediationCI", function(object) {
  if (!any(tolower(object@method) %in% c("dop", "mc", "asymp", "all"))) {
    return("Invalid method specified. Valid methods are 'dop', 'MC', 'asymp', 'all'.")
  }
  if (object@conf.level <= 0 || object@conf.level >= 1) {
    return("Confidence level must be between 0 and 1.")
  }
  if (!is.numeric(object@conf.low) || !is.numeric(object@conf.high)) {
    return("Confidence bounds 'conf.low' and 'conf.high' must be numeric.")
  }
  if (!is.null(object@model) && !(inherits(object@model, "lavaan") || inherits(object@model, "MxModel"))) {
    return("The 'model' slot must be an object of class 'lavaan' or 'MxModel'.")
  }
  if (!is.numeric(object@estimate) || !is.numeric(object@se)) {
    return("The 'estimate' and 'se' slots must be numeric.")
  }
  if (!is.list(object@additional)) {
    return("The 'additional' slot must be a list.")
  }
  TRUE
})

setMethod("initialize", "MediationCI", function(.Object, method = "dop", conf.level = 0.95, estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, se = NA_real_, model = NULL, additional = list()) {
  .Object@method <- method
  .Object@conf.level <- conf.level
  .Object@estimate <- estimate
  .Object@conf.low <- conf.low
  .Object@conf.high <- conf.high
  .Object@se <- se
  .Object@model <- model
  .Object@additional <- additional

  validityMsg <- validObject(.Object)
  if (is.character(validityMsg)) {
    stop(validityMsg, call. = FALSE)
  }

  .Object
})
