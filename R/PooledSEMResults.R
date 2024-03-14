#' Pooled SEM Analysis Results Class
#'
#' An S4 class to represent pooled results from SEM analysis across multiple imputations or datasets.
#' It contains pooled estimates, standard errors, test statistics, p-values, and confidence intervals
#' for each parameter estimated across multiple imputations.
#'
#' @slot estimates numeric Vector of pooled parameter estimates.
#' @slot stdErrors numeric Vector of pooled standard errors for each parameter estimate.
#' @slot statistics numeric Vector of pooled test statistics (e.g., z-values, t-values) for each parameter.
#' @slot pValues numeric Vector of pooled p-values for each parameter's test statistic.
#' @slot confLow numeric Vector of lower bounds of the confidence intervals for each parameter estimate.
#' @slot confHigh numeric Vector of upper bounds of the confidence intervals for each parameter estimate.
#' @slot method character The method used for SEM analysis ('lavaan' or 'OpenMx').
#' @export PooledSEMResults
setClass(
    "PooledSEMResults",
    slots = c(
        estimates = "numeric",
        stdErrors = "numeric",
        statistics = "numeric",
        pValues = "numeric",
        confLow = "numeric",
        confHigh = "numeric",
        method = "character"
    )
)

#' Initialize Method for PooledSEMResults Class
#'
#' @description Initializes a new PooledSEMResults object with given slot values.
#' @param .Object PooledSEMResults The object to be initialized.
#' @param estimates numeric Initial pooled parameter estimates.
#' @param stdErrors numeric Initial pooled standard errors.
#' @param statistics numeric Initial pooled test statistics.
#' @param pValues numeric Initial pooled p-values.
#' @param confLow numeric Initial lower bounds of the confidence intervals.
#' @param confHigh numeric Initial upper bounds of the confidence intervals.
#' @param method character The SEM analysis method.
#' @return A PooledSEMResults object.
setMethod("initialize", "PooledSEMResults", function(.Object, estimates, stdErrors, statistics, pValues, confLow, confHigh, method) {
    .Object@estimates <- estimates
    .Object@stdErrors <- stdErrors
    .Object@statistics <- statistics
    .Object@pValues <- pValues
    .Object@confLow <- confLow
    .Object@confHigh <- confHigh
    .Object@method <- method

    validObject(.Object)
    return(.Object)
})

#' Validity Check for PooledSEMResults Class
#'
#' @description Checks the validity of a PooledSEMResults object ensuring all slot values are correct.
#' @param object PooledSEMResults The object to check.
#' @return TRUE if the object is valid, otherwise returns a character vector with error messages.
setValidity("PooledSEMResults", function(object) {
    messages <- character(0)

    if (length(object@estimates) != length(object@stdErrors) || length(object@estimates) != length(object@statistics) || length(object@estimates) != length(object@pValues)) {
        messages <- c(messages, "Lengths of estimates, stdErrors, statistics, and pValues must be equal.")
    }
    if (length(object@confLow) != length(object@confHigh)) {
        messages <- c(messages, "Lengths of confLow and confHigh must be equal and match estimates.")
    }
    if (!object@method %in% c("lavaan", "OpenMx")) {
        messages <- c(messages, "Method must be either 'lavaan' or 'OpenMx'.")
    }

    if (length(messages) == 0) {
        return(TRUE)
    } else {
        return(messages)
    }
})




setGeneric(
    "pool_sem",
    function(object, conf.int, conf.level, ...) standardGeneric("pool_sem")
)

setMethod("pool_sem", "SemResults", function(object, conf.int = FALSE, conf.level = 0.95, ...) {
    # Placeholder for pooled result computations
    # Assume extract_lav and extract_mx functions perform the necessary pooling

    if (object@method == "lavaan") {
        pooledData <- extract_lav(object, conf.int, conf.level)
    } else if (object@method == "OpenMx") {
        pooledData <- extract_mx(object, conf.int, conf.level)
    } else {
        stop("Unsupported method specified in SemResults")
    }

    # Construct a PooledSEMResults object
    newPooledResults <- new("PooledSEMResults",
        estimate = pooledData$estimate,
        std.error = pooledData$std.error,
        statistic = pooledData$statistic,
        p.values = pooledData$p.value,
        conf.low = pooledData$conf.low,
        conf.high = pooledData$conf.high,
        method = object@method
    )

    return(newPooledResults)
})

## Helper functions for pooling results from lavaan and OpenMx objects
## These functions should be customized based on the structure of your lavaan and OpenMx objects
## and the specific information you need to extract for pooling.

extract_lav <- function(fit,
                        conf.int = FALSE,
                        conf.level = 0.95) {
    # Extract the relevant information from a lavaan object
    # This function should be customized based on the structure of your lavaan objects
    # and the specific information you need to extract for pooling

    nimp <- length(fit)
    pooled_est <- fit |>
        purrr::map_dfr(broom::tidy, conf.int = conf.int, .id = "imp") |>
        dplyr::select(
            .data$term,
            .data$estimate,
            .data$std.error,
            .data$statistic,
            .data$p.value
        ) |>
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

extract_mx <- function(fit,
                       conf.int = FALSE,
                       conf.level = 0.95) {
    # Extract the relevant information from a MxModel object
    # This function should be customized based on the structure of your lavaan objects
    # and the specific information you need to extract for pooling

    nimp <- length(fit)
    pooled_est <- fit |>
        purrr::map_dfr(RMediation::tidy, conf.int = conf.int, .id = "imp") |>
        dplyr::select(
            .data$term,
            .data$estimate,
            .data$std.error,
            .data$statistic,
            .data$p.value
        ) |>
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
