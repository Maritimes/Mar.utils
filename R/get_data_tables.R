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
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @param quiet default is \code{FALSE}.  If TRUE, no output messages will be shown.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_data_tables<-function(schema=NULL,
                          data.dir = file.path(getwd(), 'data'),
                          tables = NULL,
                          usepkg = 'rodbc', 
                          fn.oracle.username ="_none_",
                          fn.oracle.password="_none_",
                          fn.oracle.dsn="_none_",
                          env=.GlobalEnv,
                          quiet=F){
  schema=toupper(schema)
  tables = toupper(tables)
  
  try_load <- function(tables, data.dir, thisenv = env) {
    loadit <- function(x, data.dir) {
      this = paste0(x, ".RData")
      thisP = file.path(data.dir, this)
      if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "MARFISSCI",replacement ="MARFIS",ignore.case = T))) thisP = gsub(x= thisP,pattern = "MARFISSCI",replacement ="MARFIS",ignore.case = T)
      if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "GROUNDFISH",replacement ="RV",ignore.case = T))) thisP = gsub(x= thisP,pattern = "GROUNDFISH",replacement ="RV",ignore.case = T)
      if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "OBSERVER",replacement ="ISDB",ignore.case = T))) thisP = gsub(x= thisP,pattern = "OBSERVER",replacement ="ISDB",ignore.case = T)
      
      load(file = thisP,envir = env)
      if (!quiet) cat(paste0("\nLoaded ", x, "... "))
      fileAge = file.info(thisP)$mtime
      fileAge = round(difftime(Sys.time(), fileAge, units = "days"), 0)
      if (!quiet) cat(paste0(" (Data modified ", fileAge, " days ago.)"))
      if ((!quiet)  & fileAge > 90) 
        cat(paste("\n!!! This data was extracted more than 90 days ago - consider re-extracting it"))
    }
    if (!quiet) cat("\nLoading data...")
    timer.start = proc.time()
    sapply(tables, simplify = TRUE, loadit, data.dir)
    elapsed = timer.start - proc.time()
    if (!quiet) cat(paste0("\n\n", round(elapsed[3], 0) * -1, " seconds to load..."))
  }
  
  reqd = toupper(paste0(schema, ".", tables))
  loadsuccess = tryCatch(
    {
      try_load(reqd, data.dir)
    }, 
    warning = function(w) {
      print()
    },
    error=function(cond){
      return(-1)
    }
  )
  if (is.null(loadsuccess)){
    return(invisible(NULL))
  } else if (loadsuccess==-1){
    oracle_cxn_custom = Mar.utils::make_oracle_cxn(usepkg, fn.oracle.username, fn.oracle.password, fn.oracle.dsn)  
    if (!class(oracle_cxn_custom) =="list"){
      cat("\nCan't get the data without a DB connection.  Aborting.\n")
      return(NULL)
    }
    
    for (i in 1:length(tables)){
      if (!quiet) cat(paste0("\n","Verifying access to ",tables[i]," ..."))
      qry = paste0("select '1' from ",schema,".",gsub(paste0(schema,"."),"",tables[i])," WHERE ROWNUM<=1")
      
      
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
      cat(paste0("\n","Extracting ",tables[i],"..."))
      table_naked = gsub(paste0(schema,"."),"",tables[i])
      qry = paste0("SELECT * from ", schema, ".",table_naked)
      res= oracle_cxn_custom$thecmd(oracle_cxn_custom$channel, qry, rows_at_time = 1)
      assign(table_naked, res)
      save(list = table_naked, file = file.path(data.dir, paste0(schema,".",tables[i],".RData")))
      if (!quiet) cat(paste("\n","Got", tables[i]))
      assign(x = tables[i],value = get(table_naked), envir = env)
      if (!quiet) cat(paste0("\n","Loaded ",tables[i]))
    }
  }
}