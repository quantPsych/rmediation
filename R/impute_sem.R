#' Create SemImputedData Object
#'
#' Constructs a new `SemImputedData` object for structural equation modeling (SEM) analysis
#' on multiply imputed datasets. This function ensures that the provided data is a [mice::mids]
#' object from the `mice` package and that the specified SEM analysis method is supported.
#'
#' @param data A `mids` object from the `mice` package containing multiply imputed datasets.
#' @param method A character string specifying the SEM analysis method.
#'        Valid options are "lavaan" or "OpenMx". Defaults to "lavaan".
#' @return An object of class `SemImputedData`.
#' @usage impute_sem(data, method)
#' @seealso [mice::mids] [lavaan] [OpenMx]
#' @examples
#' \dontrun{
#' data("HolzingerSwineford1939", package = "lavaan")
#' df_complete <- na.omit(HolzingerSwineford1939)
#' amp <- mice::ampute(df_complete, prop = 0.2, mech = "MAR")
#' imputed_data <- mice::mice(amp$amp, m = 3, maxit = 3, seed = 12345, printFlag = FALSE)
#' sem_data <- impute_sem(data = imputed_data, method = "lavaan")
#' str(sem_data)
#' }
#' @export
setGeneric("impute_sem", function(data, method = "lavaan") {
  standardGeneric("impute_sem")
})

#' @rdname impute_sem
#' @export
setMethod("impute_sem", signature(data = "mids"), function(data, method = "lavaan") {
  if (!inherits(data, "mids")) {
    stop("'data' must be a 'mids' object from the 'mice' package.", call. = FALSE)
  }

  if (!method %in% c("lavaan", "OpenMx")) {
    stop("'method' must be either 'lavaan' or 'OpenMx'.", call. = FALSE)
  }

  # Capture additional arguments passed through ellipsis
  # additional = list(...)

  # Create and return the SemImputedData object, correctly forwarding '...'
  # Assuming 'SemImputedData' constructor can handle '...'
  new("SemImputedData", data = data, method = method)
})
