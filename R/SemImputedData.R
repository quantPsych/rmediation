#' SemImputedData Class
#'
#' An S4 class to hold multiply imputed datasets for structural equation modeling (SEM) analysis.
#' It facilitates working with imputed data from the `mice` package and supports SEM analysis
#' using either the `lavaan` or `OpenMx` packages.
#'
#' @slot data An object of class `mids` from the `mice` package, representing multiply imputed datasets.
#' @slot method A character string indicating the SEM package to be used for analysis.
#' Valid options are "lavaan" or "OpenMx". Defaults to "lavaan".
#' @slot conf.int A logical value indicating whether confidence intervals are
#'   included in the SEM results. Defaults to `FALSE`.
#' @slot conf.level A numeric value specifying the confidence level for
#'   confidence intervals, which must be between 0 and 1. Defaults to 0.95.
#' @importFrom methods setClass setValidity setMethod validObject
#' @exportClass SemImputedData
#' @name SemImputedData
#' @rdname SemImputedData
#' @aliases SemImputedData SemImputedData-class
#' @docType class
#' @author Davood Tofighi \email{dtofighi@@gmail.com}

setClass("SemImputedData",
  slots = c(
    data = "mids", # Ensuring 'data' is specifically a 'mids' object
    method = "character", # 'lavaan' or 'OpenMx',
    conf.int = "logical", # Whether to include confidence intervals in the SEM results
    conf.level = "numeric" # The confidence level for confidence intervals
  )
)

setValidity("SemImputedData", function(object) {
  messages <- character()

  if (!inherits(object@data, "mids")) {
    messages <-
      c(
        messages,
        "'data' must be a 'mids' object from the 'mice' package."
      )
  }

  if (!object@method %in% c("lavaan", "OpenMx")) {
    messages <-
      c(messages, "'method' must be either 'lavaan' or 'OpenMx'.")
  }

  if (length(messages) == 0) {
    TRUE
  } else {
    messages
  }
})


setMethod("initialize", "SemImputedData",
          function(.Object, data, method = "lavaan", conf.int = FALSE, conf.level = 0.95) {
            if (!inherits(data, "mids")) {
              stop("'data' must be a 'mids' object from the 'mice' package.", call. = FALSE)
            }
            if (!method %in% c("lavaan", "OpenMx")) {
              stop("'method' must be either 'lavaan' or 'OpenMx'.", call. = FALSE)
            }
            if (!is.logical(conf.int) || length(conf.int) != 1) {
              stop("conf.int must be a single logical value.", call. = FALSE)
            }
            if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
              stop("conf.level must be a numeric value between 0 and 1.", call. = FALSE)
            }

            .Object@data <- data
            .Object@method <- method
            .Object@conf.int <- conf.int
            .Object@conf.level <- conf.level

            return(.Object)
          }
)


### =========================================================
### SemResults Methods
### =========================================================

#' Create SemImputedData Object
#'
#' Constructs a new `SemImputedData` object for structural equation modeling (SEM) analysis
#' on multiply imputed datasets. This function ensures that the provided data is a [mice::mids]
#' object from the `mice` package and that the specified SEM analysis method is supported.
#'
#' @param data A `mids` object from the `mice` package containing multiply imputed datasets.
#' @param method A character string specifying the SEM analysis method.
#'        Valid options are "lavaan" or "OpenMx". Defaults to "lavaan".
#' @param conf.int A logical value indicating whether confidence intervals are
#'   included in the SEM results. Defaults to `FALSE`.
#' @param conf.level A numeric value specifying the confidence level for
#'  confidence intervals, which must be between 0 and 1. Defaults to 0.95.
#'  If `conf.int` is `FALSE`, this argument is ignored.
#'
#' @return An object of class SemImputedData. See [SemImputedData] for the details of the slots.
#'
#' @details All the arguments `data`, `method`, `conf.int`, and `conf.level` are used to specify the SEM analysis. `impute_sem` is a constructor function for `SemImputedData` class. These methods are used as constructors for the `SemImputedData` class.
#' @usage impute_sem(data, method, conf.int, conf.level)
#' @seealso  [SemImputedData] [mice::mids] [lavaan] [OpenMx]
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
setGeneric("impute_sem", function(data, method = "lavaan", conf.int = FALSE, conf.level = 0.95) {
  standardGeneric("impute_sem")
})


#' @rdname impute_sem
#' @export
setMethod("impute_sem", signature(data = "mids"), function(data, method = "lavaan", conf.int = FALSE, conf.level = 0.95) {
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
  new("SemImputedData", data = data, method = method, conf.int = conf.int, conf.level = conf.level)
})
