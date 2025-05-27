#' @title get_data_tables
#' @description This function allows users to extract tables for which they have select permissions
#' from Oracle.
#' @param schema default is \code{NULL}. This is the schema you want to access 
#' a additional tables from. A value of "<NA>" will check the schema associated 
#' with the logged in user (i.e. the user that made the connection).  An "<NA>"
#' schema is often necessary for ISDB data extractions.
#' @param data.dir  The default is your working directory. If you are hoping to load existing data,
#' this folder should contain a data folder containing your rdata files. If you are extracting data,
#' a data folder will be created under this folder.
#' extracted files to go.
#' @param tables The default value is \code{NULL}.  This is a vector of table 
#' names you want to extract that exist in identified \code{schema} of the the database.
#' @param rownum The default value is \code{NULL}. This is an integer that can be used to limit the 
#' number  of records retrieved from the specified table(s).  If it is left as \code{NULL}, all records 
#' retrieved.  Otherwise, it will return the specified number of rows.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the 
#' connection to Oracle should use \code{'rodbc'} or \code{'roracle'} to 
#' connect.  rodbc is slightly easier to setup, but roracle will extract data ~ 
#' 5x faster.Deprecated; use \code{cxn} instead.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param fn.oracle.username Default is \code{'_none_'}. This is your username 
#' for accessing Oracle objects. If you have a value for \code{oracle.username} 
#' stored in your environment (e.g., from an rprofile file), this can be left 
#' out and that value will be used. If a value for this is provided, it will 
#' take priority over your existing value. Deprecated; use \code{cxn} instead.
#' @param fn.oracle.password Default is \code{'_none_'}. This is your password 
#' for accessing Oracle objects. If you have a value for \code{oracle.password} 
#' stored in your environment (e.g., from an rprofile file), this can be left 
#' out and that value will be used. If a value for this is provided, it will 
#' take priority over your existing value. Deprecated; use \code{cxn} instead.
#' @param fn.oracle.dsn Default is \code{'_none_'}. This is your DSN/ODBC 
#' identifier for accessing Oracle objects. If you have a value 
#' for \code{oracle.dsn} stored in your environment (e.g., from an rprofile 
#' file), this can be left out and that value will be used. If a value for this 
#' is provided, it will take priority over your existing value. Deprecated; use 
#' \code{cxn} instead.
#' @param checkOnly default is \code{FALSE}  This flag allows the function to be 
#' run such that it checks for the existence of the files, but doesn't load them.
#' @param force.extract default is \code{FALSE}  This flag forces a re-extraction of all of the 
#' tables (rather than loading previously extracted versions from data.dir)
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @param fuzzyMatch default is \code{TRUE}.  This allows source data tables to match with
#' (generally) synonymous  schema identifiers (i.e. MARFIS.==MARFISSCI.; RV.==GROUNDFISH.;
#' ISDB.=OBSERVER.)  Changing to False forces an exact match.
#' @param quietly default is \code{FALSE}.  If TRUE, no output messages will be shown.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_data_tables<-function(schema=NULL,
                          data.dir = file.path(getwd(), 'data'),
                          tables = NULL,
                          rownum = NULL,
                          cxn = NULL,
                          usepkg = 'rodbc', 
                          fn.oracle.username ="_none_",
                          fn.oracle.password="_none_",
                          fn.oracle.dsn="_none_",
                          checkOnly = FALSE,
                          force.extract = FALSE,
                          env=.GlobalEnv,
                          fuzzyMatch = TRUE,
                          quietly=TRUE){
  deprecationCheck(fn.oracle.username = fn.oracle.username, 
                   fn.oracle.password = fn.oracle.password, 
                   fn.oracle.dsn = fn.oracle.dsn,
                   usepkg = usepkg)
  schema=toupper(schema)
  tables = toupper(tables)
  if (!quietly){
    if(!checkOnly) {
      message("\nLoading data...")
    }else{
      message("\nVerifying existence of data...")
    }
  } 
  timer.start = proc.time()
  try_load <- function(tables, data.dir, checkOnly, thisenv = env) {

    loadit <- function(x, data.dir, checkOnly) {
      this = paste0(x, ".RData")
      thisP = file.path(data.dir, this)
      if (fuzzyMatch){
        if (grepl(x= thisP,pattern = "MARFIS\\.|MARFISSCI\\.")){
          if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "MARFISSCI",replacement ="MARFIS",ignore.case = T))) {
            thisP = gsub(x= thisP,pattern = "MARFISSCI",replacement ="MARFIS",ignore.case = T)
          }
        } else  if (grepl(x= thisP,pattern = "RV\\.|GROUNDFISH.")){
          if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "GROUNDFISH",replacement ="RV",ignore.case = T))) {
            thisP = gsub(x= thisP,pattern = "GROUNDFISH",replacement ="RV",ignore.case = T)
          }
        } else if (grepl(x= thisP,pattern = "ISDB\\.|OBSERVER.")){
          if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "OBSERVER",replacement ="ISDB",ignore.case = T))) {
            thisP = gsub(x= thisP,pattern = "OBSERVER",replacement ="ISDB",ignore.case = T)
          }
        }
      }
      thisP = gsub(pattern = "//", replacement = "/", x = thisP)
      
      if(checkOnly) {
        if(file.exists(thisP)){
          if (!quietly) message(paste0("\nVerified ", x, "... "))
        }else{
          if (!quietly) message(paste0("\nMissing ", x, " (download will be attempted)... "))
          stop()
        }
      }else{
        load_encrypted(file = thisP,envir = env)
        if (!quietly) message(paste0("\nLoaded ", x, "... "))
      }
      fileAge = file.info(thisP)$mtime
      fileAge = round(difftime(Sys.time(), fileAge, units = "days"), 0)
      if (!quietly) message(paste0(" (Data modified ", fileAge, " days ago.)"))
    }
    sapply(tables, simplify = TRUE, loadit, data.dir, checkOnly)  
    if (!quietly) 
      return(TRUE)
  }
  if (schema == "<NA>"){
    reqd = toupper(tables)
  }else{
    reqd = toupper(paste0(schema, ".", tables))
  }

  res <- NA
  for (r in 1:length(reqd)){
    loadsuccess = tryCatch(
      {
        try_load(reqd[r], data.dir, checkOnly)
      }, 
      warning = function(w) {
        print()
      },
      error=function(cond){
        return(-1)
      }
    )
    res <- c(res, loadsuccess)
  }
  res<- res[!is.na(res)]
  if (all(res %in% 1) & !force.extract){
    t = timer.start - proc.time()
    if (!quietly){
      t = round(t[3], 0) * -1
      if(!checkOnly) {
        message(paste0("\n\n", t, " seconds to load..."))
      }else{ 
        message(paste0("\n\n", t, " seconds to check..."))
      }
    } 
    return(invisible(NULL))
  } else {
    if (is.null(cxn)) {
      oracle_cxn_custom = Mar.utils::make_oracle_cxn(usepkg, fn.oracle.username, fn.oracle.password, fn.oracle.dsn)  
      if (!inherits(oracle_cxn_custom, "list")) {
        message("\nCan't get the data without a DB connection. Aborting.\n")
        return(NULL)
      }
      cxn = oracle_cxn_custom$channel
      thecmd = oracle_cxn_custom$thecmd
    } else {
      thecmd <- Mar.utils::connectionCheck(cxn)
    }
    if (force.extract){
      missingtables = tables
    }else{
      missingtables = tables[which(res==-1)]
    }
    for (i in 1:length(missingtables)){
      if (!quietly) message(paste0("\n","Verifying access to ",missingtables[i]," ..."))
      if(schema=="<NA>"){
        qry = paste0("select '1' from  ",gsub(paste0(schema,"."),"",missingtables[i])," WHERE ROWNUM<=1")
      }else{
       qry = paste0("select '1' from ",schema,".",gsub(paste0(schema,"."),"",missingtables[i])," WHERE ROWNUM<=1")
      }
      m = tryCatch(
        {
          thecmd(cxn, qry, rows_at_time = 1)
        },
        error=function(cond){
          return(-1)
        }
      )
      if (is.numeric(m) && m==-1){
        message("\nCan't find or access the specified table")
        message(qry)
        next
      }else if (is.data.frame(m) && nrow(m) == 0){
        message("\nTable exists but contains no data")
        next
      }
      if(!checkOnly) {
        message(paste0("\n","Extracting ",missingtables[i],"..."))
        table_naked = gsub(paste0(schema,"."),"",missingtables[i])
        if (is.null(rownum)){
          where_N = ""
        }else{
          where_N = paste0(" WHERE rownum <= ", rownum)
        }
        if(schema=="<NA>"){
          qry = paste0("SELECT * from ",table_naked, where_N)
        }else{
          qry = paste0("SELECT * from ", schema, ".",table_naked, where_N)
        }
        result = thecmd(cxn, qry, rows_at_time = 1)
        assign(table_naked, result, envir = env)
        if(schema=="<NA>"){
          save_encrypted(list = table_naked, file = file.path(data.dir, paste0(missingtables[i],".RData")), envir = env)
        }else{
          save_encrypted(list = table_naked, file = file.path(data.dir, paste0(schema,".",missingtables[i],".RData")), envir = env)
        }
        if (!quietly) message(paste("\n","Got", missingtables[i]))
        assign(x = missingtables[i],value = result, envir = env)
        if (!quietly) message(paste0("\n","Loaded ",missingtables[i]))
      }
    } 
    t = timer.start - proc.time()
    if (!quietly){
      t = round(t[3], 0) * -1
      if(!checkOnly) {
        message(paste0("\n\n", t, " seconds to load..."))
      }else{ 
        if (all(res == 1)){
          message(paste0("\n\n", t, " seconds to check..."))
        }else{
          message(paste0("\n\n", t, " seconds to check and download..."))
        }
      }
    } 
  }
  
  
}
