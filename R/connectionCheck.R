#' Check Connection Type
#'
#' This function checks the type of the provided connection and returns the appropriate command function.
#' It is intended for internal use within the package.
#'
#' @param cxn The connection object to check.
#' @return The command function corresponding to the connection type.
#' @keywords internal
#' @noRd
connectionCheck <- function(cxn) {
  if (inherits(cxn, "RODBC")) {
    thecmd = RODBC::sqlQuery
  } else if (inherits(cxn, "OraConnection")) {
    thecmd = ROracle::dbGetQuery
  } else if (inherits(cxn, "DBIConnection")) {
    thecmd = DBI::dbGetQuery
  } else if (inherits(cxn, "JDBCConnection")) {
    thecmd = RJDBC::dbGetQuery
  } else {
    stop("Invalid connection object provided.")
  }
  return(thecmd)
}