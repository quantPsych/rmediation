#' Print Method for MediationCI Objects
#'
#' Provides a concise overview of a MediationCI object, displaying key information
#' including the estimation method, confidence level, mediated effect estimate, and
#' confidence interval.
#'
#' @param x A [MediationCI-class] object.
#' @exportMethod print
#' @export
#' @rdname print-method
#' @seealso [print], [MediationCI-class]
#' @examples
#' \dontrun{
#' # Assuming `mediationResult` is an existing MediationCI object
#' print(result)
#' }
setMethod("print", "MediationCI", function(x) {
  cat("MediationCI Object\n")
  cat("---------------\n")
  cat("Method: ", x@method, "\n")
  cat(sprintf("Confidence Level: %.2f%%\n", x@conf.level * 100))
  cat(sprintf("Estimate: %.4f\n", x@estimate))
  ci_text <- paste0("[", sprintf("%.4f", x@conf.low), ", ", sprintf("%.4f", x@conf.high), "]")
  cat("Confidence Interval: ", ci_text, "\n")
  cat("\n") # Add an extra line for spacing
})
