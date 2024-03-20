#' Tidy an MxModel Object
#'
#' Extracts parameter estimates from an [MxModel] from the [OpenMx] model and formats them into a tidy dataframe.
#'
#' @param x An object of class [MxModel] resulting from an SEM fit using OpenMx.
#' @param conf_int Logical, whether to include confidence intervals in the output.
#' @param conf_level  The confidence level to use for the confidence intervals.
#' @param ... Additional arguments (currently not used).
#' @return A tibble with one row per parameter and columns for parameter names, estimates,
#'         standard errors, and optionally confidence intervals.
#' @export
#' @import OpenMx
#' @importFrom dplyr mutate select rename contains
#' @importFrom tibble tibble as_tibble
#' @importFrom stats qnorm pnorm
#' @importFrom rlang .data
#' @name tidy.MxModel
#' @rdname tidy_MxModel
#' @seealso [OpenMx] [MxModel] [summary.MxModel]
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
#'   mxPath(from = "one", to = manifestVars, arrows = 1, free = FALSE, values = 0),
#'   mxPath(from = "one", to = latVar, arrows = 1, free = FALSE, values = 0),
#'   mxData(HolzingerSwineford1939, type = "raw")
#' )
#' #
#' # Fit the model
#' fit0 <- mxRun(model)
#' RMediation::tidy(fit0)
#' }
tidy.MxModel <-
  function(x,
           conf_int = FALSE,
           conf_level = 0.95,
           ...) {
    # Ensure the input is an OpenMx model
    if (!inherits(x, "MxModel")) {
      stop("Input must be an MxModel object from OpenMx.")
    }

    # Extract parameter estimates
    tidy_df <-
      summary(x)$parameters |>
      dplyr::select(-dplyr::contains("bound"), -.data$row, -.data$matrix) |>
      dplyr::rename(
        term = .data$name,
        label = .data$col,
        estimate = .data$Estimate,
        std_error = .data$Std.Error
      ) |>
      dplyr::mutate(
        statistic = .data$estimate / .data$std_error,
        p.value = 2 * pnorm(abs(.data$statistic), lower.tail = FALSE)
      ) |>
      tibble::as_tibble()

    # Add confidence intervals if requested
    if (conf_int) {
      q <- qnorm((1 + conf_level) / 2)
      tidy_df <- tidy_df |>
        dplyr::mutate(
          conf_low = .data$estimate - q * .data$std_error,
          conf_high = .data$estimate + q * .data$std_error
        ) |>
        tibble::as_tibble()
    }

    return(tidy_df)
  }
