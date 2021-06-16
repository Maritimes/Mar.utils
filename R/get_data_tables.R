#' @title get_data_tables
#' @description This function allows users to extract tables for which they have select permissions
#' from Oracle.
#' @param schema default is \code{NULL}. This is the schema you want to access 
#' a additional tables from.
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
#' 5x faster.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. 
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. 
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. 
#' @param checkOnly default is \code{FALSE}  This flag allows the function to be 
#' run such that it checks for the existence of the files, but doesn't load them.
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @param quietly default is \code{FALSE}.  If TRUE, no output messages will be shown.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_data_tables<-function(schema=NULL,
                          data.dir = file.path(getwd(), 'data'),
                          tables = NULL,
                          rownum = NULL,
                          usepkg = 'rodbc', 
                          fn.oracle.username ="_none_",
                          fn.oracle.password="_none_",
                          fn.oracle.dsn="_none_",
                          checkOnly = FALSE,
                          env=.GlobalEnv,
                          quietly=F){
  schema=toupper(schema)
  tables = toupper(tables)
  if (!quietly){
    if(!checkOnly) {
      cat("\nLoading data...")
    }else{
      cat("\nVerifying existence of data...")
    }
  } 
  timer.start = proc.time()
  try_load <- function(tables, data.dir, checkOnly, thisenv = env) {
    loadit <- function(x, data.dir, checkOnly) {
      this = paste0(x, ".RData")
      thisP = file.path(data.dir, this)
      
      
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
      

      
      
      if(checkOnly) {
        if(file.exists(thisP)){
          if (!quietly) cat(paste0("\nVerified ", x, "... "))
        }else{
          if (!quietly) cat(paste0("\nMissing ", x, " (download will be attempted)... "))
          stop()
        }
      }else{
        load(file = thisP,envir = env)
        if (!quietly) cat(paste0("\nLoaded ", x, "... "))
      }
      fileAge = file.info(thisP)$mtime
      fileAge = round(difftime(Sys.time(), fileAge, units = "days"), 0)
      if (!quietly) cat(paste0(" (Data modified ", fileAge, " days ago.)"))
      # if ((!quietly)  & fileAge > 90) 
      #   cat(paste("\n!!! This data was extracted more than 90 days ago - consider re-extracting it"))
    }
    sapply(tables, simplify = TRUE, loadit, data.dir, checkOnly)  
    if (!quietly) 
    return(TRUE)
  }
  
  reqd = toupper(paste0(schema, ".", tables))
  
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
  if (all(res %in% 1)){
    t = timer.start - proc.time()
    if (!quietly){
      t = round(t[3], 0) * -1
      if(!checkOnly) {
        cat(paste0("\n\n", t, " seconds to load..."))
      }else{ 
        cat(paste0("\n\n", t, " seconds to check..."))
      }
    } 
    return(invisible(NULL))
  } else {
    oracle_cxn_custom = Mar.utils::make_oracle_cxn(usepkg, fn.oracle.username, fn.oracle.password, fn.oracle.dsn)  
    if (!class(oracle_cxn_custom) =="list"){
      cat("\nCan't get the data without a DB connection.  Aborting.\n")
      return(NULL)
    }
    
    missingtables = tables[which(res==-1)]
    for (i in 1:length(missingtables)){
      if (!quietly) cat(paste0("\n","Verifying access to ",missingtables[i]," ..."))
      qry = paste0("select '1' from ",schema,".",gsub(paste0(schema,"."),"",missingtables[i])," WHERE ROWNUM<=1")
      
      
      m = tryCatch(
        {
          oracle_cxn_custom$thecmd(oracle_cxn_custom$channel, qry, rows_at_time = 1)
        },
        error=function(cond){
          return(-1)
        }
      )
      
      if (m==-1){
        cat("\nCan't find or access the specified table")
        break
      }
      cat(paste0("\n","Extracting ",missingtables[i],"..."))
      table_naked = gsub(paste0(schema,"."),"",missingtables[i])
      if (is.null(rownum)){
        where_N = ""
      }else{
        where_N = paste0(" WHERE rownum <= ", rownum)
      }
      qry = paste0("SELECT * from ", schema, ".",table_naked, where_N)
      result= oracle_cxn_custom$thecmd(oracle_cxn_custom$channel, qry, rows_at_time = 1)
      assign(table_naked, result)
      save(list = table_naked, file = file.path(data.dir, paste0(schema,".",missingtables[i],".RData")))
      if (!quietly) cat(paste("\n","Got", missingtables[i]))
      if(!checkOnly) {
        assign(x = missingtables[i],value = get(table_naked), envir = env)
        if (!quietly) cat(paste0("\n","Loaded ",missingtables[i]))
      }
    } 
    t = timer.start - proc.time()
    if (!quietly){
      t = round(t[3], 0) * -1
      if(!checkOnly) {
        cat(paste0("\n\n", t, " seconds to load..."))
      }else{ 
        if (all(res == 1)){
          cat(paste0("\n\n", t, " seconds to check..."))
        }else{
          cat(paste0("\n\n", t, " seconds to check and download..."))
        }
      }
    } 
  }
  
  
}
