#' @title enableDebug
#' @description This is a helper function used by \code{debugMode()} to add tracing to specific 
#' functions. It wraps each function with entry/exit messages and timing information using R's 
#' built-in \code{trace()} functionality.
#' @param fn_names a character vector of function names to enable debugging for
#' @return NULL - modifies functions in place by adding trace calls
#' @family debugging
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @seealso \code{\link{debugMode}}
#' @keywords internal
#' @noRd
enableDebug <- function(fn_names, where = .GlobalEnv) {
  for (fn_name in fn_names) {
    # Try to find where the function actually lives
    fn_env <- tryCatch(
      environment(get(fn_name)),
      error = function(e) where
    )
    
    try({
      trace(fn_name,
            where = fn_env,
            tracer = bquote({
              .depth <- length(sys.calls()) - 1
              .indent <- paste(rep("  ", .depth), collapse = "")
              message(.indent, "\u2192 Entering ", .(fn_name))
              .t1 <- proc.time()
            }),
            exit = bquote({
              .elapsed <- proc.time() - .t1
              .depth <- length(sys.calls()) - 1
              .indent <- paste(rep("  ", .depth), collapse = "")
              message(.indent, "\u2190 Exiting ", .(fn_name), " (", 
                      round(.elapsed[3], 2), "s elapsed)")
            }),
            print = FALSE)
    }, silent = TRUE)
  }
}

#' @title debugMode
#' @description This function enables or disables debugging mode for all functions within a 
#' package's R directory. When enabled, every function call will print entry/exit messages along 
#' with elapsed time, making it easy to trace execution flow and identify performance bottlenecks. 
#' Unlike traditional debugging approaches, this requires no modifications to existing function 
#' code - simply toggle debugging on or off as needed. The function automatically discovers all 
#' functions defined in .R files within the specified directory.
#' @param enable Logical. If TRUE, enables debug tracing; if FALSE, disables it.
#' @param package Character. Name of an installed package to debug (e.g., "dplyr").
#' @param r_dir Character. Name of the R source directory. Default is "R".
#' @param package_path Character. Full path to a package directory (for development packages).
#'
#' @return Invisibly returns a character vector of function names that were traced.
#' @export
#'
#' @details
#' This function automatically discovers all functions defined in a package's R directory
#' and adds entry/exit tracing with timing information. It works with:
#' \itemize{
#'   \item Development packages (current directory or specified path)
#'   \item Installed packages (by name)
#' }
#'
#' Priority order: package_path > package > current directory
#'
#' @examples
#' \dontrun{
#' # Debug current package
#' debugMode(TRUE)
#' 
#' # Debug an installed package
#' debugMode(TRUE, package = "YourPackage")
#' 
#' # Debug a development package by path
#' debugMode(TRUE, package_path = "~/Projects/MyPackage")
#' 
#' # Turn off debugging
#' debugMode(FALSE)
#' }
debugMode <- function(enable = TRUE, package = NULL, r_dir = "R", package_path = NULL) {
  
  # Priority: package_path > package name > current directory
  if (!is.null(package_path)) {
    full_r_dir <- file.path(package_path, r_dir)
  } else if (!is.null(package)) {
    # Try to find the package source
    pkg_path <- find.package(package)
    full_r_dir <- file.path(pkg_path, r_dir)
  } else {
    full_r_dir <- r_dir
  }
  
  if (!dir.exists(full_r_dir)) {
    stop("Directory not found: ", full_r_dir)
  }
  
  # Get all .R files in the R directory
  r_files <- list.files(full_r_dir, pattern = "\\.R$", 
                        full.names = TRUE, ignore.case = TRUE)
  
  # Extract function names from each file
  all_functions <- c()
  for (file in r_files) {
    code <- tryCatch(parse(file), error = function(e) NULL)
    if (is.null(code)) next
    
    # Find assignments that define functions
    for (expr in code) {
      if (length(expr) > 1 && expr[[1]] == as.name("<-")) {
        fn_name <- as.character(expr[[2]])
        # Check if right side is a function definition
        if (length(expr[[3]]) > 0 && expr[[3]][[1]] == as.name("function")) {
          all_functions <- c(all_functions, fn_name)
        }
      }
    }
  }
  
  all_functions <- unique(all_functions)
  
  if (enable) {
    # Determine the environment
    if (!is.null(package)) {
      pkg_env <- asNamespace(package)
    } else {
      pkg_env <- .GlobalEnv
    }
    
    enableDebug(all_functions, where = pkg_env)
    message("Debug mode ON for ", length(all_functions), " functions in ", full_r_dir)
    message("Functions: ", paste(all_functions, collapse = ", "))
  }
  
  invisible(all_functions)
}
