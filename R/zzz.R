# .onAttach <- function(libname, pkgname) {
#   localVer = utils::packageDescription('Mar.utils')$Version
#   packageStartupMessage(paste0("Version: ", localVer))
# }

# .onLoad <- function(libname, pkgname){
#   options(stringsAsFactors = FALSE)
#   updateCheck(gitPkg = 'Maritimes/Mar.utils')
# }
