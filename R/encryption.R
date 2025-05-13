#' @title .get_machine_id
#' @description  supports encryption for data files
#' 
#' @keywords internal
.get_machine_id <- function() {
  user <- Sys.info()["user"]
  host <- Sys.info()["nodename"]
  machine_id <- digest::digest(paste0(user, host), algo = "sha256")
  return(machine_id)
}

#' @title .save_encrypted
#' @description saves data files such that they can't be easily loaded outside of the package
#' 
#' @param object R object to save
#' @param file Path to save to
#' @keywords internal
.save_encrypted <- function(object, file) {
  key <- Mar.utils:::.get_machine_id()  # Add dot here
  objname <- deparse(substitute(object))
  obj_data <- list(object)
  names(obj_data) <- objname
  serialized <- serialize(obj_data, NULL)
  encrypted <- openssl::aes_cbc_encrypt(serialized, key = charToRaw(key))
  writeBin(encrypted, file)
}

#' @title .load_encrypted
#' @description loads encrypted data files
#' 
#' @param file Path to encrypted file
#' @param envir Environment where loaded objects will be assigned
#' @keywords internal
.load_encrypted <- function(file, envir = parent.frame()) {
  key <- Mar.utils:::.get_machine_id()  # Add dot here
  encrypted <- readBin(file, "raw", file.size(file))
  tryCatch({
    serialized <- openssl::aes_cbc_decrypt(encrypted, key = charToRaw(key))
    obj_data <- unserialize(serialized)
    for (name in names(obj_data)) {
      assign(name, obj_data[[name]], envir = envir)
    }
    invisible(names(obj_data))
  }, error = function(e) {
    stop("Unable to decrypt data. This file was likely created on a different machine.")
  })
}