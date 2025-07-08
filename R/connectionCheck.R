#' @title Check Connection Type
#' @description This function checks the type of the provided connection and 
#' returns the appropriate command function. 
#' @param cxn The connection object to check.
#' @return The command function corresponding to the connection type.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}l
#' @export
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
    thecmd = NULL
    warning("Invalid connection object provided.")
  }
  return(thecmd)
}