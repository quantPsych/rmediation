#' Function to check if lavaan model syntax is valid
#'
#' This function checks if the lavaan model syntax is valid. It does so by
#' attempting to parse and fit the model in a safe environment. If the model
#' syntax is invalid, the function will return an error message.
#' @param model A character string representing the lavaan model to be fitted.
#' @param quiet A logical value indicating whether to suppress the error message. default is FALSE.
#'
#' @return A logical value indicating whether the model syntax is valid.
#'
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
#' is_lav_syntax(bad_model)
#' good_model <- "visual =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed =~ x7 + x8 + x9
#' visual ~ speed
#' textual ~ speed"
#' is_lav_syntax(good_model)
is_lav_syntax <- function(model, quiet = FALSE) {
  if (!is.character(model)) {
    if (!quiet) print("The model syntax must be a character string.")
    return(FALSE)
  } else {
    check_syntax <- try(lavaan::lavaanify(model))
    if (inherits(check_syntax, "try-error")) {
      if (!quiet) print("A lavaan syntax error occurred!")
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}
