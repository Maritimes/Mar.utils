#' @title .get_machine_id
#' @description supports encryption for data files
#' @param user Optional username override
#' @param host Optional hostname override
#' @keywords internal
.get_machine_id <- function(user = NULL, host = NULL) {
  if(is.null(user)) user <- Sys.info()["user"]
  if(is.null(host)) host <- Sys.info()["nodename"]
  user <- tolower(user)
  host <- tolower(host)
  
  full_hash <- digest::digest(paste0(user, host), algo = "sha256", serialize = FALSE)
  return(substring(full_hash, 1, 32))
}

#' @title save_encrypted
#' @description Saves data files encrypted or unencrypted based on a toggle such
#' that they can't be easily loaded by other users.
#' @param object R object to save
#' @param list Character vector of object names to save (NULL if using 'object')
#' @param file Path to save to
#' @param compress logical or character string specifying the compression method.
#' @param envir Environment to look in for objects (when using list)
#' @param encrypt Logical. If TRUE (default), performs encryption. If FALSE, saves normally via base::save.
#' @export
save_encrypted <- function(object = NULL,
                           list = NULL,
                           file,
                           compress = TRUE,
                           envir = parent.frame(),
                           encrypt = TRUE) {
  if (!is.null(object)) {
    objname <- deparse(substitute(object))
    obj_data <- list(object)
    names(obj_data) <- objname
  } else if (!is.null(list)) {
    obj_data <- mget(list, envir = envir)
  } else {
    stop("Either 'object' or 'list' must be provided")
  }
  
  if (!encrypt) {
    if (!is.null(list)) {
      save(list = list, file = file, envir = envir, compress = compress)
    } else {
      save(list = names(obj_data), file = file, envir = list2env(obj_data, parent = emptyenv()), compress = compress)
    }
    return(invisible(NULL))
  }
  
  key_str <- Mar.utils:::.get_machine_id()
  raw_key <- openssl::sha256(charToRaw(key_str))
  iv <- openssl::rand_bytes(16)
  
  serialized <- serialize(obj_data, NULL)
  
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
  
  encrypted <- openssl::aes_cbc_encrypt(serialized, key = raw_key, iv = iv)
  
  # Convert encrypted raw vector to base64 string
  encrypted_base64 <- base64enc::base64encode(encrypted)
  
  # Combine IV and encrypted_base64 as strings and write to file
  writeLines(c(base64enc::base64encode(iv), encrypted_base64), con = file)
  
  invisible(NULL)
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
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  
  tryCatch({
    key_str <- .get_machine_id(user = extract_user, host = extract_computer)
    raw_key <- openssl::sha256(charToRaw(key_str))
    
    # Read the IV and encrypted base64 content from the file
    lines <- readLines(con = file)
    iv <- base64enc::base64decode(lines[1])
    encrypted_base64 <- lines[2]
    
    # Convert the encrypted base64 back to raw bytes for decryption
    encrypted <- base64enc::base64decode(encrypted_base64)
    
    serialized <- openssl::aes_cbc_decrypt(encrypted, key = raw_key, iv = iv)
    decompressed <- memDecompress(serialized, type = "gzip")
    obj_data <- unserialize(decompressed)
    
    for (name in names(obj_data)) {
      assign(name, obj_data[[name]], envir = envir)
    }
    return(invisible(names(obj_data)))
  }, error = function(e) {
    # message("Decryption failed: ", e$message)
    # message("Falling back to unencrypted load (may not succeed).")
    tryCatch({
      loaded_objects <- load(file, envir = envir)
      return(invisible(loaded_objects))
    }, error = function(e2) {
      stop("Failed to load file as either encrypted or unencrypted: ", e2$message)
    })
  })
}