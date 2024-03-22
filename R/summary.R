#' Summary Method for SemImputedData Objects
#'
#' Provides a comprehensive summary of an SemImputedData object, including the SEM method used,
#' the number of imputations, basic information about the imputed data, and summaries of the
#' original data and fitted model.
#'
#' @param object An object of class SemImputedData.
#' @param ... Further arguments passed to or from other methods.
#' @return A textual summary of the SemImputedData object.
#' @importFrom mice convergence
#' @export
#' @examples
#' \dontrun{
#' # Assuming `sem_imputed_data` is an SemImputedData object
#' summary(sem_imputed_data)
#' }
setMethod("summary", "SemImputedData", function(object, ...) {
  cat("Summary of SemImputedData Object\n")
  cat("--------------------------------\n")

  # Display the SEM method used
  cat("SEM Method Used:", object@method, "\n\n")

  # Display the number of imputations
  cat("Number of Imputations:", object@n_imputations, "\n\n")

  # Summary of the original data
  if (!is.null(object@original_data)) {
    cat("Original Data Summary:\n")
    print(summary(object@original_data))
    cat("\n")
  } else {
    cat("Original data not available.\n\n")
  }

  # Summary of the fitted model (if applicable)
  if (!is.null(object@fit_model)) {
    cat("Fitted Model Summary:\n")
    print(summary(object@fit_model))
    cat("\n")
  } else {
    cat("Fitted model not available.\n\n")
  }

  # Summary of the 'data' slot of the 'mids' object
 if (!is.null(object@data)) {
    cat("Imputed Data Summary ('data' slot of 'mids' object):\n")
    print(object@data)
    cat("\n")
  } else {
    cat("Imputed data ('data' slot of 'mids' object) not available.\n\n")
  }
  invisible(object)
})

### =================================================================================================
### Summary Method for MediationCI Objects
### =================================================================================================


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
#' @keywords internal
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
