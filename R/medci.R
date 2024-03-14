#' Confidence Interval for the Mediated Effect
#'
#' Computes confidence intervals for the mediated effect, supporting various methods.
#'
#' @param mu.x Mean of \eqn{x}.
#' @param mu.y Mean of \eqn{y}.
#' @param se.x Standard error of \eqn{x}.
#' @param se.y Standard error of \eqn{y}.
#' @param rho Correlation between \eqn{x} and \eqn{y} (-1 < \code{rho} < 1).
#' @param alpha Significance level for the CI (default: 0.05).
#' @param type Method for CI: "dop" (default), "MC", "asymp", or "all".
#' @param n.mc Sample size for Monte Carlo method when \code{type="MC"} (default: 1e5).
#' @param ... Additional arguments for future development.
#' @return A list containing CI, estimates, and additional information based on the `type`.
#' @export
#' @keywords mediation confidence-interval
#' @importFrom stats rnorm qnorm pnorm dnorm cor density coef cov deriv integrate quantile sd var uniroot vcov
#' @importFrom graphics par arrows axis curve legend lines mtext plot points text title abline segments
#' @importFrom grDevices dev.new
#' @importFrom methods is
#' @importFrom boot boot boot.ci
#' @references Craig, C. C. (1936). On the frequency function of \eqn{xy}.
#'   \emph{The Annals of Mathematical Statistics}, \bold{7}, 1--15.
#'
#'   MacKinnon, D. P., Fritz, M. S., Williams, J., and Lockwood, C. M. (2007).
#'   Distribution of the product confidence limits for the indirect effect:
#'   Program PRODCLIN. \emph{Behavior Research Methods}, \bold{39}, 384--389.
#'
#'   Meeker, W. and Escobar, L. (1994). An algorithm to compute the CDF of the
#'   product of two normal random variables. \emph{Communications in Statistics:
#'   Simulation and Computation}, \bold{23}, 271--280.
#'
#'   Tofighi, D. and MacKinnon, D. P. (2011). RMediation: An R package for mediation analysis
#'   confidence intervals. \emph{Behavior Research Methods}, \bold{43},
#'   692--700. \doi{doi:10.3758/s13428-011-0076-x}
#'
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @examples
#' res <- medci(mu.x = .2, mu.y = .4, se.x = 1, se.y = 1, type = "dop")
#' @seealso \code{\link{lavaan}}, \code{\link[OpenMx]{mxModel}}
medci <- function(mu.x, mu.y, se.x, se.y, rho = 0, alpha = .05, type = "dop", n.mc = 1e5, ...) {
  # Validate inputs (optional, based on user needs and function exposure)
  stopifnot(
    is.numeric(mu.x), is.numeric(mu.y), is.numeric(se.x), is.numeric(se.y),
    is.numeric(alpha), is.numeric(rho), alpha > 0 & alpha < 1, rho > -1 & rho < 1
  )

  type <- tolower(type)

  if (type == "all") {
    return(list(
      dop = medciDop(mu.x, mu.y, se.x, se.y, rho, alpha),
      mc = medciMC(mu.x, mu.y, se.x, se.y, rho, alpha, n.mc = n.mc),
      asymp = medciAsymp(mu.x, mu.y, se.x, se.y, rho, alpha)
    ))
  } else if (type %in% c("dop", "mc", "asymp")) {
    fn <- get(paste0("medci", toupper(substr(type, 1, 1)), substr(type, 2, nchar(type))))
    return(fn(mu.x, mu.y, se.x, se.y, rho, alpha, n.mc = n.mc))
  } else {
    stop("Invalid type specified. Use 'all', 'dop', 'mc', or 'asymp'.")
  }
}


medciAsymp <-
  function(mu.x, mu.y, se.x, se.y, rho = 0, alpha = .05, ...) {
    p <- alpha / 2
    quantMean <- mu.x * mu.y + se.x * se.y * rho
    quantSE <- sqrt(se.y^2 * mu.x^2 + se.x^2 * mu.y^2 + 2 * mu.x * mu.y * rho * se.x * se.y + se.x^2 * se.y^2 + se.x^2 * se.y^2 * rho^2)
    CI <- quantMean + c(qnorm(alpha / 2), qnorm(1 - alpha / 2)) * quantSE
    res <- list(CI, quantMean, quantSE)
    names(res) <- c(paste((1 - alpha) * 100, "% ", "CI", sep = ""), "Estimate", "SE")
    return(res)
  }


medciAsymp <-
  function(mu.x, mu.y, se.x, se.y, rho = 0, alpha = .05, ...) {
    p <- alpha / 2
    quantMean <- mu.x * mu.y + se.x * se.y * rho
    quantSE <- sqrt(se.y^2 * mu.x^2 + se.x^2 * mu.y^2 + 2 * mu.x * mu.y * rho * se.x * se.y + se.x^2 * se.y^2 + se.x^2 * se.y^2 * rho^2)
    CI <- quantMean + c(qnorm(alpha / 2), qnorm(1 - alpha / 2)) * quantSE
    res <- list(CI, quantMean, quantSE)
    names(res) <- c(paste((1 - alpha) * 100, "% ", "CI", sep = ""), "Estimate", "SE")
    return(res)
  }

medciDop <-
  function(mu.x, mu.y, se.x, se.y, rho = 0, alpha = .05, ...) {
    p <- alpha / 2
    q.l <- qprodnormalMeeker(p, mu.x = mu.x, mu.y = mu.y, se.x = se.x, se.y = se.y, rho = rho, lower.tail = TRUE)
    q.u <- qprodnormalMeeker(p, mu.x = mu.x, mu.y = mu.y, se.x = se.x, se.y = se.y, rho = rho, lower.tail = FALSE)
    CI <- c(q.l[[1]], q.u[[1]]) # confidence interval
    quantMean <- mu.x * mu.y + se.x * se.y * rho
    quantSE <- sqrt(se.y^2 * mu.x^2 + se.x^2 * mu.y^2 + 2 * mu.x * mu.y * rho * se.x * se.y + se.x^2 * se.y^2 + se.x^2 * se.y^2 * rho^2)
    res <- list(CI, quantMean, quantSE)
    names(res) <- c(paste((1 - alpha) * 100, "% ", "CI", sep = ""), "Estimate", "SE")
    return(res)
  }


# medci <- function(mu.x, mu.y, se.x, se.y, rho = 0, alpha = .05, type = "dop", plot = FALSE, plotCI = FALSE, n.mc = 1e5, ...) {
#   if (!is.numeric(mu.x)) {
#     stop("Argument mu.x must be numeric!")
#   }
#   if (!is.numeric(mu.y)) {
#     stop("Argument mu.y must be numeric!")
#   }
#   if (!is.numeric(se.x)) {
#     stop("Argument se.x must be numeric!")
#   }
#   if (!is.numeric(se.y)) {
#     stop("Argument se.y must be numeric!")
#   }
#   if (!is.numeric(alpha)) {
#     stop("Argument alpha  must be numeric!")
#   }
#   if (!is.numeric(rho)) {
#     stop("Argument rho  must be numeric!")
#   }
#   if (alpha <= 0 || alpha >= 1) {
#     stop("alpha must be between 0 and 1!")
#   }
#   if (rho <= -1 || rho >= 1) {
#     stop("rho must be between -1 and 1!")
#   }
#   if (!is.numeric(n.mc) || is.null(n.mc)) {
#     n.mc <- 1e5
#   } # sets n.mc to default
#   if (plot == TRUE) {
#     mean.v <- c(mu.x, mu.y)
#     var.mat <- matrix(c(se.x^2, se.x * se.y * rho, se.x * se.y * rho, se.y^2), 2)
#     x_y <- matrix(rnorm(2 * n.mc), ncol = n.mc)
#     x_y <- crossprod(chol(var.mat), x_y) + mean.v
#     x_y <- t(x_y)
#     xy <- x_y[, 1] * x_y[, 2]
#     se.xy <- sqrt(se.y^2 * mu.x^2 + se.x^2 * mu.y^2 + 2 * mu.x * mu.y * rho * se.x * se.y + se.x^2 * se.y^2 + se.x^2 * se.y^2 * rho^2)
#     mu.xy <- mu.x * mu.y + rho * se.x * se.y
#     max1 <- mu.xy + 6 * se.xy
#     min1 <- mu.xy - 6 * se.xy
#     if (min1 > 0 || max1 < 0) {
#       xrange <- round(seq(min1, max1, length = 7), 1)
#     } else {
#       xrange <- round(cbind(seq(min1, 0, length = 3), seq(0, max1, length = 3)), 1)
#     }
#     xy <- xy[xy > min1 & xy < max1]
#     plot(density(xy), xlab = expression(paste("Product ", (italic(xy)))), ylab = "Density", axes = FALSE, xlim = c(min1, max1), main = "", ...)
#     axis(1, xrange)
#     axis(2)
#     smidge <- par("cin") * abs(par("tcl"))
#     text(max1 - (max1 - min1) / 7, (par("usr")[4]), pos = 1, bquote(mu == .(round(mu.xy, 3))), ...)
#     text(max1 - (max1 - min1) / 7, (par("usr")[4] - 1.5 * par("cxy")[2]), pos = 1, bquote(sigma == .(round(se.xy, 3))), ...)
#     if (plotCI) {
#       yci <- par("usr")[3] + diff(par("usr")[3:4]) / 25
#       yci <- 0
#       MedCI <- medciMeeker(mu.x, mu.y, se.x, se.y, rho, alpha)
#       arrows(MedCI[[1]][1], yci, MedCI[[1]][2], yci, length = smidge, angle = 90, code = 3, cex = 1.5, ...)
#       points(mu.xy, yci, pch = 19, cex = 1.5, ...)
#       text(max1 - (max1 - min1) / 7, (par("usr")[4] - 3 * par("cxy")[2]), pos = 1, paste("LL=", round(MedCI[[1]][1], 3)), ...)
#       text(max1 - (max1 - min1) / 7, (par("usr")[4] - 4.5 * par("cxy")[2]), pos = 1, paste("UL=", round(MedCI[[1]][2], 3)), ...)
#     }
#   }
#
#   if (type == "all" || type == "All" || type == "ALL") {
#     MCCI <- medciMC(mu.x, mu.y, se.x, se.y, rho, alpha, n.mc = n.mc)
#     asympCI <- medciAsymp(mu.x, mu.y, se.x, se.y, rho, alpha) # added 3/28/14-DT
#     MeekerCI <- medciMeeker(mu.x, mu.y, se.x, se.y, rho, alpha)
#     res <- list(MeekerCI, MCCI, asympCI)
#     names(res) <- c("dop", "Monte Carlo", "Asymptotic Normal")
#     return(res)
#   } else if (type == "DOP" || type == "dop" || type == "prodclin") {
#     MeekerCI <- medciMeeker(mu.x, mu.y, se.x, se.y, rho, alpha)
#     return(MeekerCI)
#   } else if (type == "MC" || type == "mc" || type == "Mc") {
#     MCCI <- medciMC(mu.x, mu.y, se.x, se.y, rho, alpha, n.mc = n.mc)
#     return(MCCI)
#   } else if (type == "Asymp" || type == "asymp") {
#     asympCI <- medciAsymp(mu.x, mu.y, se.x, se.y, rho, alpha) # Modified/ added 3/28/14
#     return(asympCI)
#   } else {
#     stop("Wrong type! please specify type=\"all\", \"DOP\", \"prodclin\",\"MC\", or \"asymp\" ")
#   }
# }
