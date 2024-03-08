sem_mice <- function(model, mids, ...) {
    # Ensure 'mids' is a 'mids' object
    if (!inherits(mids, "mids")) {
        stop("'mids' must be a 'mids' object from the 'mice' package.")
    }
    # Ensure 'mxModel' is an OpenMx model object

    dplyr::case_when(
        inherits(model, "MxModel") ~ mx_mice(model, mids, ...),
        RMediation::is_valid_lav_syntax(model, mids$data) ~ lav_mice(model, mids, ...),
        TRUE ~ stop("The model is not a valid lavaan or OpenMx model syntax.")
    )
}
