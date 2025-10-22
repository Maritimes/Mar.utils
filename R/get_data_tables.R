#' @title get_data_tables
#' @description This function allows users to extract tables for which they have select permissions
#' from Oracle.
#' @param schema default is \code{NULL}. This is the schema you want to access 
#' a additional tables from. A value of "<NA>" will check the schema associated 
#' with the logged in user (i.e. the user that made the connection).  An "<NA>"
#' schema is often necessary for ISDB data extractions.
#' @param data.dir  The default is your working directory. If you are hoping to load existing data,
#' this should point to a folder containing your rdata files. If you are extracting data,
#' the data will be saved here.
#' @param tables The default value is \code{NULL}.  This is a vector of table 
#' names you want to extract that exist in identified \code{schema} of the the database.
#' @param rownum The default value is \code{NULL}. This is an integer that can be used to limit the 
#' number  of records retrieved from the specified table(s).  If it is left as \code{NULL}, all records 
#' retrieved.  Otherwise, it will return the specified number of rows.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
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
#' @param extract_user default is \code{NULL}.  This parameter can be used with
#' \code{extract_computer} to load encypted data files extracted by another user
#' and/or computer
#' @param extract_computer  default is \code{NULL}.  This parameter can be used with
#' \code{extract_user} to load encypted data files extracted by another user
#' and/or computer
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_data_tables <- function(schema = NULL,
                            data.dir = file.path(getwd()),
                            tables = NULL,
                            rownum = NULL,
                            cxn = NULL,
                            checkOnly = FALSE,
                            force.extract = FALSE,
                            env = .GlobalEnv,
                            fuzzyMatch = TRUE,
                            quietly = TRUE,
                            extract_user = NULL,
                            extract_computer = NULL) {
  schema = toupper(schema)
  tables = toupper(tables)
  if (!quietly) {
    message(if (!checkOnly) "\nLoading data..." else "\nVerifying existence of data...")
  }
  
  timer.start = proc.time()
  
  try_load <- function(tables, data.dir, checkOnly, thisenv) {
    
    loadit <- function(x, data.dir, checkOnly) {
      this = paste0(x, ".RData")
      thisP = file.path(data.dir, this)
      if (fuzzyMatch) {
        if (grepl(x = thisP, pattern = "MARFIS\\.|MARFISSCI\\.")) {
          if (!file.exists(thisP) & file.exists(gsub(x = thisP, pattern = "MARFISSCI", replacement = "MARFIS", ignore.case = TRUE))) {
            thisP = gsub(x = thisP, pattern = "MARFISSCI", replacement = "MARFIS", ignore.case = TRUE)
          }
        } else if (grepl(x = thisP, pattern = "RV\\.|GROUNDFISH.")) {
          if (!file.exists(thisP) & file.exists(gsub(x = thisP, pattern = "GROUNDFISH", replacement = "RV", ignore.case = TRUE))) {
            thisP = gsub(x = thisP, pattern = "GROUNDFISH", replacement = "RV", ignore.case = TRUE)
          }
        } else if (grepl(x = thisP, pattern = "ISDB\\.|OBSERVER.")) {
          if (!file.exists(thisP) & file.exists(gsub(x = thisP, pattern = "OBSERVER", replacement = "ISDB", ignore.case = TRUE))) {
            thisP = gsub(x = thisP, pattern = "OBSERVER", replacement = "ISDB", ignore.case = TRUE)
          }
        }
      }
      
      thisP = gsub(pattern = "//", replacement = "/", x = thisP)
      tryCatch({
        if (checkOnly) {
          if (file.exists(thisP)) {
            if (!quietly) message(paste0("\nVerified ", x, "... "))
          } else {
            stop("\nMissing ", x, " (download will be attempted)... ")
          }
        } else {
          load_encrypted(file = thisP, envir = thisenv, extract_user = extract_user, extract_computer = extract_computer)
          if (!quietly) message(paste0("\nLoaded ", x, "... "))
        }
        fileAge = file.info(thisP)$mtime
        fileAge = round(difftime(Sys.time(), fileAge, units = "days"), 0)
        if (!quietly) message(paste0(" (Data modified ", fileAge, " days ago.)"))
      }, error = function(e) {
        if (grepl("Failed to decrypt", e$message)) {
          stop("Incorrect decryption credentials provided. Please provide valid credentials or a cxn object with the necessary permissions.")
        } else {
          if (!quietly) message(paste0("\nFailed to load ", x, " due to unexpected error: ", e$message))
          stop(e)
        }
      })
    }
    sapply(tables, simplify = TRUE, loadit, data.dir, checkOnly)
    if (!quietly) return(TRUE)
  }
  
  reqd = if (schema == "<NA>") toupper(tables) else toupper(paste0(schema, ".", tables))
  command <- connectionCheck(cxn)
  loadsuccess <- integer(length(reqd))
  for (r in seq_along(reqd)) {
    loadsuccess[r] <- tryCatch({
      try_load(reqd[r], data.dir, checkOnly, thisenv = env)
      1
    }, error = function(e) {
      if (grepl("Incorrect decryption credentials", e$message)) stop(e$message)
      0
    })
  }
  
  missing_ <- reqd[which(loadsuccess == 0)]
  if (length(missing_) == 0 && !force.extract) {
    t_elapsed <- round((proc.time() - timer.start)[3], 0)
    if (!quietly) message(paste0("\n\n", t_elapsed, " seconds to complete operation."))
    return(invisible(NULL))
  }
  if (is.null(cxn)) {
    message("\nCan't get the data without a DB connection. Aborting.\n")
    return(invisible(NULL))
  }
  
  to_extract_ <- if (force.extract) reqd else missing_
  
  for (tab in to_extract_) {
    message(paste0("\nExtracting ", tab, "..."))
    table_naked <- sub(paste0(schema, "\\."), "", tab)
    wclause <- if (is.null(rownum)) "" else paste0(" WHERE ROWNUM <= ", rownum)
    qry_base <- if (schema == "<NA>") table_naked else paste0(schema, ".", table_naked)
    result <- command(cxn, paste0("SELECT * FROM ", qry_base, wclause), rows_at_time = 1)
    assign(table_naked, result, envir = env)
    save_encrypted(list = table_naked,
                   file = file.path(data.dir, paste0(tab, ".RData")),
                   envir = env)
    if (!quietly) message(paste0("\nGot ", tab))
  }
  
  t_elapsed <- round((proc.time() - timer.start)[3], 0)
  if (!quietly) message(paste0("\n\n", t_elapsed, " seconds to complete operation."))
}