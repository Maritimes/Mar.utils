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

#' @title save_encrypted
#' @description saves data files such that they can't be easily loaded outside of the package
#' 
#' @param object R object to save
#' @param list Character vector of object names to save (NULL if using 'object')
#' @param file Path to save to
#' @param compress logical or character string specifying the compression method.
#' @export
save_encrypted <- function(object = NULL, list = NULL, file, compress = TRUE) {
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
  
  # Serialize first
  serialized <- serialize(obj_data, NULL)
  
  # Apply compression if requested (after serialization, before encryption)
  if (compress) {
    if (is.logical(compress)) {
      serialized <- memCompress(serialized, "gzip")
    } else if (is.character(compress)) {
      if (!compress %in% c("gzip", "bzip2", "xz")) {
        stop("If character, 'compress' must be one of: 'gzip', 'bzip2', or 'xz'")
      }
      serialized <- memCompress(serialized, compress)
    } else {
      stop("'compress' must be logical or character")
    }
  }
  
  # Encrypt the (potentially compressed) serialized data
  encrypted <- openssl::aes_cbc_encrypt(serialized, key = raw_key, iv = iv)
  combined <- c(iv, encrypted)
  writeBin(combined, file)
}

#' @title load_encrypted
#' @description Loads data files, handling both encrypted and unencrypted formats
#' 
#' @param file Path to the file to load
#' @param extract_user Optional username of original file creator
#' @param extract_computer Optional hostname of original file creator
#' @param envir Environment where loaded objects will be assigned
#' @return Names of loaded objects (invisibly)
#' @export
load_encrypted <- function(file, extract_user = NULL, extract_computer = NULL, envir = parent.frame()) {
  # Check if file exists
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  
  # Try to load as encrypted first
  tryCatch({
    key_str <- .get_machine_id(user = extract_user, host = extract_computer)
    raw_key <- openssl::sha256(charToRaw(key_str))
    
    combined <- readBin(file, "raw", file.size(file))
    iv <- combined[1:16]
    encrypted <- combined[17:length(combined)]
    
    serialized <- openssl::aes_cbc_decrypt(encrypted, key = raw_key, iv = iv)
    # Let unserialize handle decompression automatically - no manual decompression needed
    obj_data <- unserialize(serialized)
    
    # Assign all objects to the environment
    for (name in names(obj_data)) {
      assign(name, obj_data[[name]], envir = envir)
    }
    return(invisible(names(obj_data)))
  }, error = function(e) {
    # If decryption fails, try to load as unencrypted RData
    message("Note: File appears to be unencrypted. Loading as standard RData file.")
    tryCatch({
      loaded_objects <- load(file, envir = envir)
      return(invisible(loaded_objects))
    }, error = function(e2) {
      stop("Failed to load file as either encrypted or unencrypted: ", e2$message)
    })
  })
}