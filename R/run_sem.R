#' Run a SEM model
#'
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
#' @aliases run_sem
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
  # Ensure 'mids' is a 'mids' object from the 'mice' package
  if (!inherits(mids, "mids")) {
    stop("'mids' must be a 'mids' object from the 'mice' package.")
  }
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
  # Ensure 'mids' is a 'mids' object
  if (!inherits(mids, "mids")) {
    stop("'mids' must be a 'mids' object from the 'mice' package.")
  }
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
