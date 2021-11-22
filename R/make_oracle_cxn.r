#' @title make_oracle_cxn
#' @description This function facilitates creating a connection to Oracle, and
#' allows connection via RODBC or ROracle, depending on the value of usepkg.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for \code{oracle.username} 
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take 
#' priority over your existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for \code{oracle.password}  
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take 
#' priority over your existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for 
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file), 
#' this can be left and that value will be used.  If a value for this is 
#' provided, it will take priority over your existing value.
#' @param quietly default is \code{FALSE}  This indicates whether or not status messages should be 
#' shown.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This function temporarily changes the system values for "TZ" and "ORA_SDTZ" to "GMT" to 
#' ensure that dates and times extracted from the various databases remain correct.  It does this 
#' by running \code{Sys.setenv(TZ = "GMT")}  and \code{Sys.setenv(ORA_SDTZ = "GMT")}.  Once a 
#' connection is established (or failes to be established), it restores the values that previously 
#' existed. 
#' @export
make_oracle_cxn <- function(usepkg = 'rodbc', 
                            fn.oracle.username ="_none_",
                            fn.oracle.password="_none_",
                            fn.oracle.dsn="_none_",
                            quietly = FALSE) {
  orig_TZ <- Sys.getenv("TZ")
  orig_ORA_SDTZ <- Sys.getenv("ORA_SDTZ")
  Sys.setenv(TZ = "GMT")
  Sys.setenv(ORA_SDTZ = "GMT")
  if (!quietly){
    message("set Sys.setenv(TZ='GMT') and Sys.setenv(ORA_SDTZ='GMT')")
  }
  oracle_cxn = NULL
  use.roracle <-function(oracle.dsn, oracle.username, oracle.password, quietly){
    oracle_cxn <-tryCatch(
      {
        assign('oracle_cxn', ROracle::dbConnect( DBI::dbDriver("Oracle"), oracle.username,oracle.password,oracle.dsn))
      }, 
      error=function(cond){
        message(cond)
      }
    )
    if (class(oracle_cxn)[1]=="OraConnection") {
      if (!quietly) cat("\nSuccessfully connected to Oracle via ROracle\n")
      results = list(usepkg='roracle', channel = oracle_cxn, thecmd=eval(parse(text='ROracle::dbGetQuery')))
      Sys.setenv(TZ = orig_TZ)
      Sys.setenv(ORA_SDTZ = orig_ORA_SDTZ)
      return(results)
    } else {
      if (!quietly) cat("\nROracle attempt failed\n")
      Sys.setenv(TZ = orig_TZ)
      Sys.setenv(ORA_SDTZ = orig_ORA_SDTZ)
      return(-1)
    }
  }
  use.rodbc <-function(oracle.dsn, oracle.username, oracle.password, quietly){
    oracle_cxn <-tryCatch(
      {
        assign('oracle_cxn', RODBC::odbcConnect(oracle.dsn, uid = oracle.username, pwd = oracle.password, believeNRows = F))
      }, 
      error=function(cond){
        message(cond)
      }
    )
    if (class(oracle_cxn)[1]=="RODBC") {
      if (!quietly) cat("\nSuccessfully connected to Oracle via RODBC\n")
      results = list(usepkg='rodbc', channel = oracle_cxn, thecmd=eval(parse(text='RODBC::sqlQuery')))
      Sys.setenv(TZ = orig_TZ)
      Sys.setenv(ORA_SDTZ = orig_ORA_SDTZ)
      return(results)
    } else {
      if (!quietly) cat("\nRODBC attempt failed\n")
      # results = list(usepkg='rodbc', channel = -1, thecmd=NA)
      # return(results)
      Sys.setenv(TZ = orig_TZ)
      Sys.setenv(ORA_SDTZ = orig_ORA_SDTZ)
      return(-1)
    }
  }
  
  if (!is.null(oracle_cxn)){
    if (class(oracle_cxn) == 'RODBC'){
      results = list(usepkg='rodbc', channel = oracle_cxn, thecmd=eval(parse(text='RODBC::sqlQuery')))
      Sys.setenv(TZ = orig_TZ)
      Sys.setenv(ORA_SDTZ = orig_ORA_SDTZ)
      return(results)
    }else if (class(oracle_cxn)[1]=="OraConnection") {
      results = list(usepkg='roracle', channel = oracle_cxn, thecmd=eval(parse(text='ROracle::dbGetQuery')))
      Sys.setenv(TZ = orig_TZ)
      Sys.setenv(ORA_SDTZ = orig_ORA_SDTZ)
      return(results)
    } 
  } else {
    #get connection info - only prompt for values not in rprofile
    if (fn.oracle.username != "_none_"){
      oracle.username= fn.oracle.username
    } else if (exists('oracle.username')){
      oracle.username <- oracle.username
      if (!quietly) cat("\nUsing stored 'oracle.username'")
    }else{
      oracle.username <- readline(prompt = "Oracle Username: ")
      print(oracle.username)
    }
    if (fn.oracle.password != "_none_"){
         oracle.password= fn.oracle.password
    }else if (exists('oracle.password')){
      oracle.password <- oracle.password
      if (!quietly) cat("\nUsing stored 'oracle.password'")
    } else {
      oracle.password <- readline(prompt = "Oracle Password: ")
      print(oracle.password)
    }
    if (fn.oracle.dsn != "_none_"){
         oracle.dsn= fn.oracle.dsn
    }else if (exists('oracle.dsn')){
      oracle.dsn <- oracle.dsn
      if (!quietly) cat("\nUsing stored 'oracle.dsn'")
    }else{
      oracle.dsn <- readline(prompt = "Oracle DSN (e.g. PTRAN): ")
      print(oracle.dsn)
    }
    
    if (usepkg=='roracle'){
      use.roracle(oracle.dsn, oracle.username, oracle.password, quietly)
    }else{
      use.rodbc(oracle.dsn, oracle.username, oracle.password, quietly)
    }
  }
}