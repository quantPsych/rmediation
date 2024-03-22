#' @title Show SemImputedData
#'
#' @description Method to display a concise summary of a SemImputedData object.
#' This method is automatically called when you print a SemImputedData object.
#'
#' @param object The SemImputedData object to be displayed.
#' @return This method does not return a value but displays a summary of the SemImputedData object.
#' @export
#' @examples
#' \dontrun{
#' data("HolzingerSwineford1939", package = "lavaan")
#' hs_data <- HolzingerSwineford1939[paste0("x", 1:9)] |> mice::ampute()
#' hs_data <- hs_data$amp
#' imp_data <- mice::mice(HolzingerSwineford1939, m = 5)
#' model_lav <- "visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9"
#' imp_data <- mice::mice(hs_data, m = 5)
#' sem_data <- SemImputedData(imp_data, model_lav)
#' show(sem_data)
#' }
setMethod("show", "SemImputedData", function(object) {
  cat("Model Setup:\n")
  cat("Number of imputations:", object@n_imputations, "\n")
  cat("Confidence intervals included:", object@conf_int, "\n")
  cat("Confidence level:", object@conf_level, "\n")
  cat("Original data:\n", "Sample Size:", nrow(object@original_data),"\n")
  print(object@original_data[1:5,])
  cat("Method:", object@method, "\n")
  cat("Model:", object@model, "\n")
})


### ============================================================================
### For Future Development
### ============================================================================

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
#' @keywords internal

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
