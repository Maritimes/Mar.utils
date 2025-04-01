#' @title  Check for Deprecated Parameters
#' @description This function checks for deprecated parameters and issues a 
#' warning if any are found.  
#' @param ... Parameters to check for deprecation.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}l
#' @export
deprecationCheck <- function(...) {
  params <- list(...)
  deprecated_params <- c("fn.oracle.username", "fn.oracle.password", "fn.oracle.dsn", "usepkg")
  deprecated_input <- deprecated_params[deprecated_params %in% names(params) & !sapply(params[deprecated_params], is.null) & !sapply(params[deprecated_params], is.na) & params[deprecated_params] != "_none_"]
  
  if (any(deprecated_input %in% c("fn.oracle.username", "fn.oracle.password", "fn.oracle.dsn", "usepkg"))) {
    warning("The parameters 'fn.oracle.username', 'fn.oracle.password', 'fn.oracle.dsn', and 'usepkg' are deprecated. Please use the 'cxn' parameter to pass an existing Oracle connection.")
    deprecated_input <- setdiff(deprecated_input, c("fn.oracle.username", "fn.oracle.password", "fn.oracle.dsn", "usepkg"))
  }
  
  for (param in deprecated_input) {
    warning(paste("The parameter", param, "is deprecated. Please use the 'cxn' parameter to pass an existing Oracle connection."))
  }
}