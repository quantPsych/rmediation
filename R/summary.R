#' Summary Method for MediationCI Objects
#'
#' Provides a detailed summary of a [MediationCI-class] object, including the method used,
#' confidence level, estimate, confidence interval, standard error, and any
#' additional information.
#'
#' @param object A [MediationCI-class] object.
#' @seealso [summary], [MediationCI-class]
#' @exportMethod summary
#' @rdname summary-method
#' @examples
#' \dontrun{
#' # Assuming `mediationResult` is an existing MediationCI object
#' summary(result)
#' }
setMethod("summary", "MediationCI", function(object) {
  cat("Detailed Summary of MediationCI Object\n")
  cat("======================================\n")
  cat("Method Used: ", object@method, "\n")
  cat(sprintf("Confidence Level: %.2f%%\n", object@conf.level * 100))
  cat(sprintf("Point Estimate of Mediated Effect: %.4f\n", object@estimate))

  ci_text <- paste0("Confidence Interval: [", sprintf("%.4f", object@conf.low), ", ", sprintf("%.4f", object@conf.high), "]")
  cat(ci_text, "\n")

  cat(sprintf("Standard Error: %.4f\n", object@se))

  # Summarizing the model object
  if (!is.null(object@model)) {
    modelName <- ifelse(inherits(object@model, "lavaan"), "Lavaan Model", "OpenMx Model")
    cat("Model Type: ", modelName, "\n")
    # Here you could add a brief summary specific to the model, but be mindful of output length
  } else {
    cat("No SEM Model Attached.\n")
  }

  # This assumes the ellipsis (...) mechanism or a flexible slot isn't used for additional arbitrary data
  # If there were other specific slots or handling mechanisms for extra analysis details, they should be summarized here

  cat("\n") # Add an extra line for spacing
  invisible(object)
})
