#' @title updateCheck
#' @description This function compares the package with the available version on 
#' github, and prompts the user to update.
#' @param gitPkg default is \code{NULL}. This is the URL to the DESCRIPTION file
#' on github.
#' @family util
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
updateCheck<-function(gitPkg = NULL){
  
  verCleaner <- function(dirtyVer = NULL) {
    if (length(dirtyVer) > 1) 
      dirtyVer = gsub(pattern = "Version: ", replacement = "", 
                      x = dirtyVer[grep(pattern = "Version:", x = dirtyVer)])
    cleanVer = gsub(pattern = "Version: ", replacement = "", x = dirtyVer)
    
    # Check if it's semantic versioning (X.Y.Z) vs date (YYYY.MM.DD)
    if (grepl("^\\d{1,2}\\.\\d{1,2}\\.\\d+$", cleanVer)) {
      # Semantic version - convert to comparable number
      parts <- as.numeric(strsplit(cleanVer, "\\.")[[1]])
      # Pad each part to ensure proper comparison (e.g., 2.10.0 > 2.9.0)
      cleanVer <- sprintf("%03d%03d%03d", parts[1], parts[2], parts[3])
    } else {
      # Date version - use existing logic
      cleanVer = gsub(pattern = "\\.", replacement = "", x = cleanVer)
    }
    return(cleanVer)
  }
  remote  <- tryCatch({
    remURL = paste("https://raw.githubusercontent.com/",gitPkg,"/master/DESCRIPTION", sep = "")
    readLines(remURL)
  },
  warning = function(cond) {
  })
  
  if (is.null(remote)){
    packageStartupMessage("\n",gitPkg,": Can't reach url to check version","\n")
    return(NULL)
  }  

  localVer = utils::packageDescription(strsplit(gitPkg,"/")[[1]][2])$Version
  localVer = verCleaner(localVer)
  remoteVer = verCleaner(remote)
  
  if (localVer == remoteVer){
    packageStartupMessage(paste0("\n", gitPkg,": Latest and greatest version confirmed","\n"))
  }else if (localVer > remoteVer){
    packageStartupMessage("\n","Push to Github!")
  }else if (localVer < remoteVer){
    packageStartupMessage(paste0("\n", gitPkg, ": Old version detected -- v.",gsub('^([0-9]{4})([0-9]{2})([0-9]{2})$','\\1\\.\\2\\.\\3',remoteVer)," is now available"))
    packageStartupMessage("\n","You can run the following code to update this package:")
    packageStartupMessage(paste("\n","devtools::install_github('",gitPkg,"')", sep=""),"\n")
  }
  
}