#' Pooling function for 'mira' objects of class 'semMice'
#' @param x An object of class 'mira' representing the results of fitting a SEM model to multiply imputed datasets
#' @param ... Additional arguments to be passed to the pooling function
#' @return A pooled 'mira' object representing the pooled results of fitting a SEM model to multiply imputed datasets
#' @examples
#' \dontrun{
#' # library(mice)
#' # library(lavaan)
#' # Load Holzinger and Swineford (1939) dataset
#' data("HolzingerSwineford1939", package = "lavaan")
#' # Introduce missing data
#' df_complete <- na.omit(HolzingerSwineford1939)
#' amp <- mice::ampute(df_complete, prop = 0.2, mech = "MAR")
#' data_with_missing <- amp$amp
#'
#' # Perform multiple imputation
#' imputed_data <- mice::mice(data_with_missing, m = 3, maxit = 5, seed = 12345, printFlag = FALSE)
#'
#' # fit the Holzinger and Swineford (1939) example model
#' HS_model <- "visual  =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed   =~ x7 + x8 + x9 "
#'
#' # Fit the SEM model without running the model
#' fit_HS <- lavaan::sem(HS_model, data = data_with_missing, do.fit = FALSE)
#'
#' # Fit the SEM model without pooling to each imputed dataset
#' fit_list <- lav_mice(HS_model, imputed_data)
#' pooled_fit <- pool(fit_list)
#' }
#'
#' @export
#' @import purrr dplyr mice
#' @importFrom purrr map map_dfr
#' @importFrom dplyr select mutate bind_rows filter summarise group_by ungroup
#' @importFrom mice pool mice complete
#' @importFrom rlang .data
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @aliases pool.semlist
#' @rdname pool_semlist
#' @name pool.semlist
#' @seealso \code{\link{pool}}

# Define the new S3 method for objects of class 'mira'
pool.semlist <- function(x, ...) {
  # Check the specific type of 'mira' object (mx_mice or lav_mice results)
  if (inherits(x, "semlist") && inherits(x, "mira"))
    # Handle pooling for mx_mice results
    # This will require extracting relevant information from your mira object
    # and performing the pooling operation specific to mx_mice results
    pooling_function(x)
    else
      stop("Unsupported mira/semMice object type for pooling.")
}

pooling_function <- function(mira_object, conf.int = FALSE, ...) {
  # Custom pooling logic for semMice results
  # Similar to mx_mice, this depends on the structure of your semMice objects
  # and the specific pooling operation needed for lav_mice results

  # Extract the fitted models as a list from your mira object
  fits <- mira_object$analyses

  if (length(fits) == 0) {
    stop("No fitted models found in the mira object")
  }

  if (inherits(fits[[1]], "lavaan")) {
    return(extract_lav(fits, conf.int = conf.int))
  } else if (inherits(fits[[1]], "MxModel")) {
    return(extract_mx(fits, conf.int = conf.int))
  } else {
    stop("Unsupported fitted model type for pooling.")
  }
}
# fits |> purrr::map_df(parameterEstimates) |> subset(select = "estimate")

#  # Extract log-likelihoods and df from each imputed dataset
#  loglik_df <-
#    fits |> purrr::map(logLik) |> purrr::map_dfr(tidy) |> subset(select = "estimate")
#  # Calculate the average log-likelihood and df
#  logLik_bar <- loglik_df |> colMeans()
#
#  return(list(
#    Q_bar = Q_bar,
#    var_total = var_total,
#    logLik_bar = logLik_bar
#  ))
# }


extract_lav <- function(fit, conf.int = conf.int) {
  # Extract the relevant information from a lavaan object
  # This function should be customized based on the structure of your lavaan objects
  # and the specific information you need to extract for pooling
  
  nimp <- length(fit)
  pooled_est <- fit |>
    purrr::map_dfr(broom::tidy, conf.int = conf.int, .id = "imp") |>
    dplyr::select(.data$term, .data$estimate, .data$std.error, .data$statistic, .data$p.value) |>
    dplyr::group_by(.data$term) |>
    dplyr::summarise(
      q_est = mean(.data$estimate),
      bet_var = var(.data$estimate),
      w_var = sum(.data$std.error^2) / nimp
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(tot_var = .data$w_var + .data$bet_var * (1 + 1 / nimp))
  return(pooled_est)
}

extract_mx <- function(fit, conf.int = conf.int) {
  # Extract the relevant information from a MxModel object
  # This function should be customized based on the structure of your lavaan objects
  # and the specific information you need to extract for pooling

  nimp <- length(fit)
  pooled_est <- fit |>
    purrr::map_dfr(RMediation::tidy, conf.int = conf.int, .id = "imp") |>
    dplyr::select(.data$term, .data$estimate, .data$std.error, .data$statistic, .data$p.value) |>
    dplyr::group_by(.data$term) |>
    dplyr::summarise(
      q_est = mean(.data$estimate),
      bet_var = var(.data$estimate),
      w_var = sum(.data$std.error^2) / nimp
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(tot_var = .data$w_var + .data$bet_var * (1 + 1 / nimp))
  return(pooled_est)
}

