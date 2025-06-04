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
  
  try_load <- function(tables, data.dir, checkOnly, thisenv = env) {
    
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
          load_encrypted(file = thisP, envir = env, extract_user = extract_user, extract_computer = extract_computer)
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
          stop(e) # Re-throw the error if it's not related to decryption failure
        }
      })
    }
    
    sapply(tables, simplify = TRUE, loadit, data.dir, checkOnly)
    if (!quietly) return(TRUE)
  }
  
  reqd = if (schema == "<NA>") toupper(tables) else toupper(paste0(schema, ".", tables))
  
  for (r in seq_along(reqd)) {
    loadsuccess = tryCatch({
      try_load(reqd[r], data.dir, checkOnly)
      1
    }, error = function(e) {
      if (grepl("Incorrect decryption credentials", e$message)) {
        stop(e$message)
      }
      -1
    })
    
    if (loadsuccess == -1 && !is.null(cxn) && !force.extract) {
      message("\nCan't proceed without a DB connection to retrieve missing data. Aborting.\n")
      return(invisible(NULL))
    }
  }
  
  if (!force.extract) {
    t = proc.time() - timer.start
    t = round(t[3], 0)
    if (!quietly) message(paste0("\n\n", t, " seconds to complete operation."))
    return(invisible(NULL))
  }
  
  if (!is.null(cxn)) {
    command = Mar.utils::connectionCheck(cxn)
  } else {
    message("\nCan't get the data without a DB connection. Aborting.\n")
    return(NULL)
  }
  
  missingtables = if (force.extract) tables else tables[which(loadsuccess == -1)]
  
  for (i in seq_along(missingtables)) {
    if (!quietly) message(paste0("\nVerifying access to ", missingtables[i], " ..."))
    table_naked = gsub(paste0(schema, "."), "", missingtables[i])
    qry = if (schema == "<NA>") {
      paste0("SELECT * FROM ", table_naked, " WHERE ROWNUM <= 1")
    } else {
      paste0("SELECT * FROM ", schema, ".", table_naked, " WHERE ROWNUM <= 1")
    }
    
    m = tryCatch(command(cxn, qry, rows_at_time = 1), error = function(e) -1)
    
    if (is.numeric(m) && m == -1) {
      message("\nCan't find or access the specified table")
      next
    } else if (is.data.frame(m) && nrow(m) == 0) {
      message("\nTable exists but contains no data")
      next
    }
    
    if (!checkOnly) {
      message(paste0("\nExtracting ", missingtables[i], "..."))
      qry = if (is.null(rownum)) {
        if (schema == "<NA>") {
          paste0("SELECT * FROM ", table_naked)
        } else {
          paste0("SELECT * FROM ", schema, ".", table_naked)
        }
      } else {
        where_N = paste0(" WHERE ROWNUM <= ", rownum)
        if (schema == "<NA>") {
          paste0("SELECT * FROM ", table_naked, where_N)
        } else {
          paste0("SELECT * FROM ", schema, ".", table_naked, where_N)
        }
      }
      
      result = command(cxn, qry, rows_at_time = 1)
      assign(table_naked, result, envir = env)
      rdata_file = file.path(data.dir, paste0(schema, ".", missingtables[i], ".RData"))
      save_encrypted(list = table_naked, file = rdata_file, envir = env)
      if (!quietly) message(paste("\nGot", missingtables[i]))
    }
  }
  
  t = proc.time() - timer.start
  t = round(t[3], 0)
  if (!quietly) message(paste0("\n\n", t, " seconds to complete operation."))
}