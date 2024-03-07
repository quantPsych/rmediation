#' Tidy an MxModel Object
#'
#' Extracts parameter estimates from an OpenMx model and formats them into a tidy dataframe.
#'
#' @param model An object of class `MxModel` resulting from an SEM fit using OpenMx.
#' @param conf.int Logical, whether to include confidence intervals in the output.
#' @param conf.level The confidence level to use for the confidence intervals.
#' @param ... Additional arguments (currently not used).
#' @return A tibble with one row per parameter and columns for parameter names, estimates,
#'         standard errors, and optionally confidence intervals.
#' @export
#' @import OpenMx
#' @importFrom dplyr mutate select rename
#' @importFrom tibble tibble as_tibble
#' @importFrom stats qnorm pnorm
#' @name tidy.MxModel
#' @rdname tidy_MxModel
#' @examples
#' \dontrun{
#' # Load Holzinger and Swineford (1939) dataset
#' data("HolzingerSwineford1939", package = "lavaan")
#' # Simple SEM model specification with OpenMx
#' manifestVars <- paste0("x", 1:9)
#' latVar <- c("visual", "textual", "speed")
#' model <- mxModel("Simple SEM",
#'   type = "RAM",
#'   manifestVars = manifestVars,
#'   latentVars = latVar,
#'   mxPath(from = "visual", to = c("x1", "x2", "x3")),
#'   mxPath(from = "textual", to = c("x4", "x5", "x6")),
#'   mxPath(from = "speed", to = c("x7", "x8", "x9")),
#'   mxPath(from = manifestVars, arrows = 2),
#'   mxPath(from = latVar, arrows = 2, free = FALSE, values = 1.0),
#'   mxPath(from = "one", to = manifestVars, arrows = 1, free = TRUE, values = 1.0),
#'   mxPath(from = "one", to = latVar, arrows = 1, free = FALSE, values = 0),
#'   mxData(HolzingerSwineford1939, type = "raw")
#' )
#
#' # Fit the model
#' fit0<- mxRun(model)
#' RMediation::tidy(fit0)
#' }

tidy.MxModel <-
  function(model,
           conf.int = FALSE,
           conf.level = 0.95,
           ...) {
    # Ensure the input is an OpenMx model
    if (!inherits(model, "MxModel")) {
      stop("Input must be an MxModel object from OpenMx.")
    }

    # Extract parameter estimates
    tidy_df <-
      summary(model)$parameters |>
      dplyr::select(-contains("bound"), -row, -matrix) |>
      dplyr::rename(
        term = name,
        label = col,
        estimate = Estimate,
        std.error = Std.Error
      ) |>
      dplyr::mutate(
        statistic = estimate / std.error,
        p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE)
      ) |>
      tibble::as_tibble()

    # Add confidence intervals if requested
    if (conf.int) {
      q <- qnorm((1 + conf.level) / 2)
      tidy_df <- tidy_df |>
        dplyr::mutate(
          conf.low = estimate - q * std.error,
          conf.high = estimate + q * std.error
        ) |>
        tibble::as_tibble()
    }

    return(tidy_df)
  }