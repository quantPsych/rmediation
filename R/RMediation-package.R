#' @keywords internal
#' @importFrom utils globalVariables
#' @import mice
#' @importFrom lavaan sem lavInspect lavOptions
#' @importFrom OpenMx mxModel mxRun mxData imxVerifyModel
#' @importFrom purrr map_dfr map
#' @importFrom methods setClass setValidity setMethod validObject setGeneric new
#' @importFrom dplyr mutate select rename contains group_by summarise ungroup
#' @importFrom tibble tibble as_tibble
#' @importFrom broom tidy
"_PACKAGE"

utils::globalVariables(c(".data"))

#' @importFrom generics tidy
#' @export
generics::tidy
