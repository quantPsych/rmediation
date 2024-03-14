#' Performing Tests of Indirect Effects
#'
#' Perform tests and compute confidence intervals for indirect effects.
#'
#' @param model Fitted [OpenMx::MxModel] or [lavaan::lavaan] object representing the mediation model.
#' @param method Character, the method used to compute the confidence interval. The default is "dop" for the distribution of the product method. Other options include "MC" for the Monte Carlo method.
#' @param conf.level Numeric, the confidence level used in the interval calculation. The default is 0.95.
#' @param estimate Numeric, the point estimate of the mediated effect.
#' @param conf.low Numeric, the lower bound of the confidence interval.
#' @param conf.high Numeric, the upper bound of the confidence interval.
#' @param se Numeric, the standard error of the mediated effect estimate.
#' @param ... Additional arguments to customize the object creation process.
#' @return An object of class `MediationCI`.
#' @details The `test_indirect` function creates instances of the `MediationCI` class, encapsulating confidence intervals for mediation analysis results. The `MediationCI` class is an S4 class that stores the results of mediation analysis, including the mediated effect estimate, confidence interval, and additional statistics.
#' @usage test_indirect(model, method, conf.level, estimate, conf.low, conf.high, se, ...)
#' @examples
#' \dontrun{
#' my_fitted_model <- ... # Assume fitting a model with OpenMx or lavaan
#' my_mediation_ci <- test_indirect(model = my_fitted_model, conf.level = 0.95)
#' }
#' @seealso [MediationCI-class]
#' @export
#' @rdname test_indirect
#' @keywords internal
setGeneric("test_indirect", function(model,
                                     method = "dop",
                                     conf.level = 0.95,
                                     estimate,
                                     conf.low,
                                     conf.high,
                                     se,
                                     ...) {
  standardGeneric("test_indirect")
})


#' @rdname test_indirect
#' @description
#' Method for `lavaan` model objects. This method specifically handles models fitted with the `lavaan` package.
#' @param model A `lavaan` model object This can be a fitted model object or a model specification.
#' @note This method treats the `model` argument as a `lavaan` model object and may handle additional arguments unique to `lavaan` analysis.
#' @export
#' @keywords internal
setMethod(
  "test_indirect", signature(model = "lavaan"),
  function(model,
           method = "dop",
           conf.level = 0.95,
           estimate,
           conf.low,
           conf.high,
           se,
           ...) {
    if (missing(model)) {
      stop("Argument 'model' is required.", call. = FALSE)
    }

    new(
      "MediationCI",
      model = model,
      method = method,
      conf.level = conf.level,
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      se = se,
      additional = list(...)
    )
  }
)

#' @rdname test_indirect
#' @description
#' Method for `OpenMx` model objects. This method specifically handles models fitted with the `OpenMx` package.
#' @param model An `OpenMx` model object. This can be a fitted model object or a model specification.
#' @note This method treats the `model` argument as an `OpenMx` model object and may handle additional arguments unique to `OpenMx` analysis.
#' @export
#' @keywords internal
setMethod("test_indirect", signature(model = "MxModel"), function(model,
                                                                  method = "dop",
                                                                  conf.level = 0.95,
                                                                  ...) {
  if (missing(model)) {
    stop("Argument 'model' is required.", call. = FALSE)
  }

  new(
    "MediationCI",
    model = model,
    method = method,
    conf.level = conf.level,
    estimate = estimate,
    conf.low = conf.low,
    conf.high = conf.high,
    se = se,
    additional = list(...)
  )
})
