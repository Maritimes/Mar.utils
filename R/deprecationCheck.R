#' Check for Deprecated Parameters
#'
#' This function checks for deprecated parameters and issues a warning if any are found.
#' It is intended for internal use within the package.
#'
#' @param ... Parameters to check for deprecation.
#' @keywords internal
#' @noRd
deprecationCheck <- function(...) {
  params <- list(...)
  deprecated_params <- c("fn.oracle.username", "fn.oracle.password", "fn.oracle.dsn", "usepkg")
  for (param in deprecated_params) {
    if (param %in% names(params) && !is.null(params[[param]]) && !is.na(params[[param]]) && params[[param]] != "_none_") {
      if (param == "usepkg" && params[[param]] %in% c("roracle", "rodbc")) {
        warning(paste("The parameter", param, "with value", params[[param]], "is deprecated. Please use the 'cxn' parameter to pass an existing Oracle connection."))
      } else if (param != "usepkg") {
        warning(paste("The parameter", param, "is deprecated. Please use the 'cxn' parameter to pass an existing Oracle connection."))
      }
    }
  }
}