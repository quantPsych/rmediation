#' Determine If a SEM Model Has Been Fitted
#'
#' This function checks whether a structural equation modeling (SEM) model,
#' represented by either a `lavaan` object or an `MxModel` object from the `OpenMx`
#' package, has been fitted. The function offers a convenient way to programmatically
#' verify if the model fitting step has been executed for a given model object.
#'
#' @param model A model object, either of class `lavaan` or `MxModel`, representing
#' the SEM model to be checked.
#' @param ... Additional arguments affecting the method dispatched
#' (currently not used but available for future extensions).
#' @return A logical value: `TRUE` if the model has been fitted, and `FALSE` otherwise.
#'
#' @usage is_fit(model,...)
#' @importClassesFrom lavaan lavaan
#' @importClassesFrom OpenMx MxModel
#' @importFrom lavaan lavInspect
#' @importFrom OpenMx imxVerifyModel
#' @seealso [lavaan], [MxModel]
#' @examples
#' \dontrun{
#' # Assuming 'lav_model' is a lavaan model object
#' lav_model_fit <- is_fit(lav_model)
#'
#' # Assuming 'mx_model' is an OpenMx model object
#' mx_model_fit <- is_fit(mx_model)
#'
#' # Checking the output
#' print(lav_model_fit)
#' print(mx_model_fit)
#' }
#' @export
#' @rdname is_fit
setGeneric("is_fit", function(model, ...) {
  standardGeneric("is_fit")
})

#' @export
setMethod(
  "is_fit",
  signature(model = "ANY"),
  definition = function(model, ...) {
    if (!inherits(model, "lavaan") && !inherits(model, "MxModel")) FALSE
  }
)

#' @export
setMethod(
  "is_fit",
  signature(model = "lavaan"),
  definition = function(model, ...) {
    # if (!inherits(model, "lavaan")) {
    #   stop("The model must be of class 'lavaan'.")
    # }
    lav_opts <- lavaan::lavInspect(model, "options")
    lav_opts$do.fit
  }
)

#' @rdname is_fit
#' @export
setMethod(
  "is_fit",
  signature(model = "MxModel"),
  definition = function(model, ...) {
    # if (!inherits(model, "MxModel")) {
    #   stop("The model must be of class 'MxModel'.")
    # }
    model@.wasRun
  }
)
