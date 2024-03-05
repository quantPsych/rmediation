#' Pooling function for 'mira' objects of class 'semMice'
#' @param object An object of class 'mira' representing the results of fitting a SEM model to multiply imputed datasets
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
#'
#'   }
#'
#' @export
#' @import purrr
#' @importFrom purrr map map_dfr
#' @importFrom mice pool
#' @importFrom mice complete
#' @importFrom mice mice
#' @author Davood Tofighi \email{dtofighi@@gmail.com}
#' @aliases pool.semMice pool_semMice
#' @rdname pool_semMice
#' @name pool.semMice
#' @seealso \code{\link{pool}}

# Define the new S3 method for objects of class 'mira'
pool.semMice <- function(object, ...) {
  # Check the specific type of 'mira' object (mx_mice or lav_mice results)
  if (inherits(object, "semMice")  && inherits(object, "mira")) {
    # Handle pooling for mx_mice results
    # This will require extracting relevant information from your mira object
    # and performing the pooling operation specific to mx_mice results
    pooled_results <- pooling_function(object)
    return(pooled_results) }
    else {
      stop("Unsupported mira/semMice object type for pooling.")
    }
  }

  pooling_function <- function(mira_object, ...) {
    # Custom pooling logic for semMice results
    # Similar to mx_mice, this depends on the structure of your semMice objects
    # and the specific pooling operation needed for lav_mice results

    # Extract the fitted models as a list from your mira object
    fits <- mira_object$analyses
    # Extract the relevant information from your mira object
    Q_df <- fits |> purrr::map_dfr(coef)

    # Extract parameter estimates from each imputed dataset
    Q_bar <- Q_df |> colMeans()

    # Calculate the between-imputation variance
    var_between <- cov(Q_df)
    #extract covaraince mayrices as a list and average covaraince matrices
    var_within <- fits |> lapply(vcov) |> reduce(`+`) / length(fits)

    # total varaince according to Rubin's rule
    var_total <-
      var_within + var_between + (1 + 1 / length(fits)) * var_within

    # Extract log-likelihoods and df from each imputed dataset
    loglik_df <-
      fits |> purrr::map(logLik) |> purrr::map_dfr(tidy) |> subset(select = "estimate")
    # Calculate the average log-likelihood and df
    logLik_bar <- loglik_df |> colMeans()

    return(list(
      Q_bar = Q_bar,
      var_total = var_total,
      logLik_bar = logLik_bar
    ))
  }
