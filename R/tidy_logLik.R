#' Creates a data.frame for a log-likelihood object
#'
#' @param x A log-likelihood object, typically returned by [logLik].
#' @param ... Additional arguments (not used)
#' @return A [data.frame] with columns:
#'  \describe{
#'  \item{term}{The term name}
#'  \item{estimate}{The log-likelihood value}
#'  \item{df}{The degrees of freedom}
#'  }
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @name tidy.logLik
#' @rdname tidy_logLik
#' @seealso \code{\link[stats]{logLik}}
#' @importFrom stats logLik
#' @importFrom generics tidy
#' @importFrom tibble tibble
#' @export
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @examples
#' fit <- lm(mpg ~ wt, data = mtcars)
#' logLik_fit <- logLik(fit)
#' tidy(logLik_fit)
tidy.logLik <- function(x, ...) {
  # Extract the log-likelihood value and the degrees of freedom
  stopifnot(inherits(x, "logLik"))

  logLik_value <- as.numeric(x)
  df <- attr(x, "df")

  tidy_df <- tibble::tibble(
    term = "logLikelihood",
    estimate = logLik_value,
    df = df
  )

  return(tidy_df)
}
