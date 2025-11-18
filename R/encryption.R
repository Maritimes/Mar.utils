#' @title .get_machine_id
#' @description supports encryption for data files
#' @param user Optional username override
#' @param host Optional hostname override
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
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
#' @param envir Environment to look in for objects (when using list)
#' @param encrypt Logical. If TRUE (default), performs encryption. If FALSE, saves normally via base::save.
#' @family file_management
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
save_encrypted <- function(object = NULL,
                           list = NULL,
                           file,
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
      save(list = list, file = file, envir = envir)
    } else {
      save(list = names(obj_data), file = file, envir = list2env(obj_data, parent = emptyenv()))
    }
    return(invisible(NULL))
  }
  
  key_str <- .get_machine_id()
  raw_key <- openssl::sha256(charToRaw(key_str))
  iv <- openssl::rand_bytes(16)
  
  serialized <- serialize(obj_data, NULL)
  
  chunk_size <- 50 * 1024 * 1024
  
  if (length(serialized) > chunk_size) {
    num_chunks <- ceiling(length(serialized) / chunk_size)
    encrypted_chunks <- vector("list", num_chunks)
    
    for (i in seq_len(num_chunks)) {
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, length(serialized))
      chunk <- serialized[start_idx:end_idx]
      encrypted_chunks[[i]] <- openssl::aes_cbc_encrypt(chunk, key = raw_key, iv = iv)
    }
    
    output_lines <- c(
      base64enc::base64encode(iv),
      paste0("CHUNKED:", num_chunks),
      sapply(encrypted_chunks, base64enc::base64encode)
    )
  } else {
    encrypted <- openssl::aes_cbc_encrypt(serialized, key = raw_key, iv = iv)
    encrypted_base64 <- base64enc::base64encode(encrypted)
    output_lines <- c(base64enc::base64encode(iv), encrypted_base64)
  }
  
  writeLines(output_lines, con = file)
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
#' @family file_management
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
load_encrypted <- function(file, extract_user = NULL, extract_computer = NULL, envir = parent.frame()) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  
  first_line_raw <- NULL
  encrypted_format_detected <- FALSE
  
  tryCatch({
    first_line_raw <- base64enc::base64decode(readLines(con = file, n = 1, warn = FALSE))
    if (length(first_line_raw) == 16) {
      encrypted_format_detected <- TRUE
    }
  }, warning = function(w) {}, error = function(e) {})
  
  if (encrypted_format_detected) {
    tryCatch({
      key_str <- .get_machine_id(user = extract_user, host = extract_computer)
      raw_key <- openssl::sha256(charToRaw(key_str))
      
      lines <- readLines(con = file)
      
      is_chunked <- grepl("^CHUNKED:", lines[2])
      
      if (is_chunked) {
        num_chunks <- as.integer(sub("^CHUNKED:", "", lines[2]))
        encrypted_chunks <- lines[3:(2 + num_chunks)]
        
        decrypted_chunks <- vector("list", num_chunks)
        for (i in seq_len(num_chunks)) {
          encrypted <- base64enc::base64decode(encrypted_chunks[i])
          decrypted_chunks[[i]] <- openssl::aes_cbc_decrypt(encrypted, key = raw_key, iv = first_line_raw)
        }
        
        decrypted <- do.call(c, decrypted_chunks)
      } else {
        encrypted_base64 <- lines[2]
        encrypted <- base64enc::base64decode(encrypted_base64)
        
        decrypted <- tryCatch(
          openssl::aes_cbc_decrypt(encrypted, key = raw_key, iv = first_line_raw),
          error = function(e) {
            stop("Unable to decrypt ", file, ".  If you did not extract this data yourself, please ensure that you are supplying the values of 'extract_user' and 'extract_computer' of the person who did.", call. = FALSE)
          }
        )
      }
      
      is_compressed <- length(decrypted) >= 2 && 
        ((decrypted[1] == 0x1f && decrypted[2] == 0x8b) ||
           (decrypted[1] == 0x78))
      
      if (is_compressed) {
        decrypted <- memDecompress(decrypted, type = "unknown")
      }
      
      obj_data <- unserialize(decrypted)
      
      for (name in names(obj_data)) {
        assign(name, obj_data[[name]], envir = envir)
      }
      return(invisible(names(obj_data)))
      
    }, error = function(e) {
      stop("Failed to decrypt the encrypted content: ", e$message)
    })
  } else {
    tryCatch({
      loaded_objects <- load(file, envir = envir)
      return(invisible(loaded_objects))
    }, error = function(e2) {
      stop("Failed to load unencrypted file: ", e2$message)
    })
  }
}