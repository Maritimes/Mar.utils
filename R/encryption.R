#' @title .get_machine_id
#' @description supports encryption for data files
#' @param user Optional username override
#' @param host Optional hostname override
#' @keywords internal
.get_machine_id <- function(user = NULL, host = NULL) {
  if(is.null(user)) user <- Sys.info()["user"]
  if(is.null(host)) host <- Sys.info()["nodename"]
  full_hash <- digest::digest(paste0(user, host), algo = "sha256", serialize = FALSE)
  return(substring(full_hash, 1, 32))
}

#' @title .save_encrypted
#' @description saves data files such that they can't be easily loaded outside of the package
#' 
#' @param object R object to save
#' @param list Character vector of object names to save (NULL if using 'object')
#' @param file Path to save to
#' @export
save_encrypted <- function(object = NULL, list = NULL, file) {
  key_str <- Mar.utils:::.get_machine_id()
  raw_key <- openssl::sha256(charToRaw(key_str))
  iv <- openssl::rand_bytes(16)
  
  if (!is.null(object)) {
    # Single object case
    objname <- deparse(substitute(object))
    obj_data <- list(object)
    names(obj_data) <- objname
  } else if (!is.null(list)) {
    # List of objects case
    obj_data <- mget(list, envir = parent.frame())
  } else {
    stop("Either 'object' or 'list' must be provided")
  }
  
  serialized <- serialize(obj_data, NULL)
  encrypted <- openssl::aes_cbc_encrypt(serialized, key = raw_key, iv = iv)
  combined <- c(iv, encrypted)
  writeBin(combined, file)
}

#' @title .load_encrypted
#' @description loads encrypted data files
#' 
#' @param file Path to encrypted file
#' @param extract_user Optional username of original file creator
#' @param extract_computer Optional hostname of original file creator
#' @param envir Environment where loaded objects will be assigned
#' @export
load_encrypted <- function(file, extract_user = NULL, extract_computer = NULL, envir = parent.frame()) {
  key_str <- Mar.utils:::.get_machine_id(user = extract_user, host = extract_computer)
  raw_key <- openssl::sha256(charToRaw(key_str))
  
  combined <- readBin(file, "raw", file.size(file))
  iv <- combined[1:16]
  encrypted <- combined[17:length(combined)]
  
  tryCatch({
    serialized <- openssl::aes_cbc_decrypt(encrypted, key = raw_key, iv = iv)
    obj_data <- unserialize(serialized)
    
    # Assign all objects to the environment
    for (name in names(obj_data)) {
      assign(name, obj_data[[name]], envir = envir)
    }
    invisible(names(obj_data))
  }, error = function(e) {
    if(!is.null(extract_user) || !is.null(extract_computer)) {
      stop("Unable to decrypt data. The provided creator credentials may be incorrect.")
    } else {
      stop("Unable to decrypt data. This file was likely created on a different machine. Try specifying extract_user and extract_computer parameters.")
    }
  })
}