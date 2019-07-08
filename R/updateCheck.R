#' @title updateCheck
#' @description This function compares the package with the available version on 
#' github, and prompts the user to update.
#' @param gitPkg default is \code{NULL}. This is the URL to the DESCRIPTION file
#' on github.
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
updateCheck<-function(gitPkg = NULL){
  
  verCleaner<-function(dirtyVer = NULL){
    #cleans up version information - can handle:
    #1) the contents of the DESCRIPTION 
    #2) "Version: YYYY.MM.DD
    #3) "YYYY.MM.DD"
    if (length(dirtyVer)>1) dirtyVer = gsub(pattern = "Version: ",replacement = "", x = dirtyVer[grep(pattern = "Version:",x = dirtyVer)])
    cleanVer = gsub(pattern = "Version: ",replacement = "", x = dirtyVer)
    cleanVer = gsub(pattern = "\\.",replacement = "",x = cleanVer)
    return(cleanVer)
  }
  remote  <- tryCatch({
    remURL = paste("https://raw.githubusercontent.com/",gitPkg,"/master/DESCRIPTION", sep = "")
    readLines(remURL)
  },
  warning = function(cond) {
  })
  
  if (is.null(remote)){
    cat("\n","Can't reach url to check version")
    return(NULL)
  }  

  localVer = utils::packageDescription(strsplit(gitPkg,"/")[[1]][2])$Version
  localVer = verCleaner(localVer)
  remoteVer = verCleaner(remote)
  
  if (localVer == remoteVer){
    cat(paste0("\n", gitPkg,": Latest and greatest version confirmed."))
  }else if (localVer > remoteVer){
    cat("\n","Push to Github!")
  }else if (localVer < remoteVer){
    cat(paste0("\n", gitPkg, ": Old version detected -- v.",gsub('^([0-9]{4})([0-9]{2})([0-9]{2})$','\\1\\.\\2\\.\\3',remoteVer)," is now available"))
    cat("\n","You can run the following code to update this package:")
    cat(paste("\n","devtools::install_github('",gitPkg,"')", sep=""))
  }
  
}