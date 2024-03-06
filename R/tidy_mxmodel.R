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
#' # Assuming `MxModel` is an already fitted OpenMx model:
#' tidy_openmx(mxModel)
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
      dplyr::select(-contains("bound"),-row,-matrix) |>
      dplyr::rename(
        term = name,
        label = col,
        estimate = Estimate,
        std.error = Std.Error
      ) |>
      dplyr::mutate(statistic = estimate / std.error,
                    p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE)) |>
      tibble::as_tibble()

    # Add confidence intervals if requested
    if (conf.int) {
      q <- qnorm((1 + conf.level) / 2)
      tidy_df <- tidy_df |>
        dplyr::mutate(ci.lower = estimate - q * std.error,
                      ci.upper = estimate + q * std.error) |>
        tibble::as_tibble()
    }

    return(tidy_df)
  }
