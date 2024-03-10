#' Function to check if lavaan model syntax is valid
#'
#' This function checks if the lavaan model syntax is valid. It does so by
#' attempting to parse and fit the model in a safe environment. If the model
#' syntax is invalid, the function will return an error message.
#' @param model A character string representing the lavaan model to be fitted.
#' @return A logical value indicating whether the model syntax is valid.
#' @name is_lav_syntax
#' @aliases is_lav_syntax is_lavaan
#' @rdname is_lav_syntax
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @importFrom OpenMx mxModel mxData mxRun omxLapply imxVerifyModel
#' @importFrom mice mice complete
#' @importFrom lavaan lavaanify 
#' @export
#' @examples
#' bad_model <- "y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9"
#' data(HolzingerSwineford1939, package = "lavaan")
#' is_lav_syntax(bad_model, HolzingerSwineford1939)
#' good_model <- "visual =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed =~ x7 + x8 + x9
#' visual ~ speed
#' textual ~ speed"
#' is_lav_syntax(good_model, HolzingerSwineford1939)
# Function to check if model syntax is valid
is_lav_syntax <- function(model) {
  result <- try(lavaan::lavaanify(model))
  check_syntax <- if (inherits(result, "try-error")) {
    # Code to execute if an error occurs
    print("Syntax error occurred. Please check your model.")
    FALSE
  } else {
    TRUE
  }
  check_syntax
}
