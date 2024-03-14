#' SemImputedData Class
#'
#' An S4 class to hold multiply imputed datasets for structural equation modeling (SEM) analysis.
#' It facilitates working with imputed data from the `mice` package and supports SEM analysis
#' using either the `lavaan` or `OpenMx` packages.
#'
#' @slot data An object of class `mids` from the `mice` package, representing multiply imputed datasets.
#' @slot method A character string indicating the SEM package to be used for analysis.
#' Valid options are "lavaan" or "OpenMx". Defaults to "lavaan".
#' @importFrom methods setClass
#' @name SemImputedData-class
#' @exportClass SemImputedData
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @seealso [SemResults-class]
#'
setClass("SemImputedData",
  slots = c(
    data = "mids",
    # Ensuring 'data' is specifically a 'mids' object
    method = "character"
    # 'lavaan' or 'OpenMx'
  )
)

#' @importFrom methods setValidity
setValidity("SemImputedData", function(object) {
  messages <- character()

  if (!object@method %in% c("lavaan", "OpenMx")) {
    messages <-
      c(messages, "'method' must be either 'lavaan' or 'OpenMx'.")
  }

  if (!inherits(object@data, "mids")) {
    messages <-
      c(
        messages,
        "'data' must be a 'mids' object from the 'mice' package."
      )
  }

  if (length(messages) == 0) {
    TRUE
  } else {
    messages
  }
})


#' @importFrom methods setMethod validObject
setMethod(
  "initialize",
  "SemImputedData",
  function(.Object, data, method = "lavaan") {
    # Check if 'data' is a 'mids' object
    if (!inherits(data, "mids")) {
      stop("'data' must be a 'mids' object from the 'mice' package.",
        call. = FALSE
      )
    }
    .Object@data <- data
    .Object@method <- method

    validObject(.Object) # Ensure the object is valid
    return(.Object)
  }
)
