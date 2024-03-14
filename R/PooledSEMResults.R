#' Pooled SEM Analysis Results Class
#'
#' An S4 class to represent pooled results from SEM analysis across multiple imputations or datasets.
#' It contains pooled estimates, standard errors, test statistics, p-values, and confidence intervals
#' for each parameter estimated across multiple imputations.
#'
#' @slot results data.frame A data frame containing the pooled results of the SEM analyses. The column names adhere to tidy conventions and include the following columns:
#'  - `term`: The name of the parameter being estimated.
#' - `estimate`: The pooled estimate of the parameter.
#' - `std.error`: The pooled standard error of the estimate.
#' - `statistic`: The pooled test statistic (e.g., z-value, t-value).
#' - `p.value`: The pooled p-value for the test statistic.
#' - `conf.low`: The lower bound of the confidence interval for the estimate.
#' - `conf.high`: The upper bound of the confidence interval for the estimate.
#' @slot method character The method used for SEM analysis ('lavaan' or 'OpenMx').
#' @export PooledSEMResults
setClass(
    "PooledSEMResults",
    slots = list(results = "data.frame", method = "character")
)

setMethod("initialize", "PooledSEMResults", function(.Object, results, method) {
    validColumns <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    missingColumns <- setdiff(validColumns, colnames(results))
    if (length(missingColumns) > 0) {
        stop("Missing required columns in results data frame: ", paste(missingColumns, collapse = ", "), call. = FALSE)
    }

    .Object@results <- results
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
