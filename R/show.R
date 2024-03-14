#' Show Summary of a MediationCI Object
#'
#' Displays a concise summary of a `MediationCI` object, including the method
#' used for confidence interval computation, confidence level, point estimate of the
#' mediated effect, confidence interval bounds, and the type of SEM model attached.
#'
#' @param object An object of class [MediationCI-class].
#' @return Invisible. The function is called for its side effect of printing
#'   the summary.
#' @rdname show-method
#' @examples
#' \dontrun{
#' # Assuming `mediationResult` is an existing MediationCI object
#' show(mediationResult)
#' }
#'
#' @seealso [show], [MediationCI-class]
#' @exportMethod show

setMethod("show", "MediationCI", function(object) {
  cat("Summary of MediationCI Object\n")
  cat("======================================\n")
  cat("Method Used: ", object@method, "\n")
  cat(sprintf("Confidence Level: %.2f%%\n", object@conf.level * 100))
  cat(sprintf("Point Estimate: %.4f\n", object@estimate))
  ci_text <- paste0("Confidence Interval: [", sprintf("%.4f", object@conf.low), ", ", sprintf("%.4f", object@conf.high), "]")
  cat(ci_text, "\n")
  cat(sprintf("Standard Error: %.4f\n", object@se))

  # Brief model summary
  if (!is.null(object@model)) {
    modelName <- ifelse(inherits(object@model, "lavaan"), "Lavaan Model", "OpenMx Model")
    cat("Model Type: ", modelName, "\n")
  } else {
    cat("No SEM Model Attached.\n")
  }

  cat("\n") # Add an extra line for spacing
})
