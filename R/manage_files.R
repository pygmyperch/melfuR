#' Manage File Operations
#'
#' Perform file operations such as listing, copying,
#' and moving files based on file name prefixes, suffixes,
#' or any part of the file name. It handles files in the working directory and
#' can operate on them based on the given action parameter.
#' @param action A string specifying the operation to perform on the files:
#'               'list' to display file names, 'copy' to copy files, or 'move'
#'               to move files. Default is 'list'.
#' @param prefix A string indicating the prefix of the file names to match.
#'               Default is an empty string, which means no prefix filtering.
#' @param suffix A string indicating the suffix of the file names to match.
#'               Default is an empty string, which means no suffix filtering.
#' @param match_any A string that should appear anywhere in the file name.
#'                  Default is an empty string, which means no general text filtering.
#' @param target_directory A string indicating the directory where files should
#'                         be copied or moved. It must be a valid path or NULL if only listing files.
#'                         Default is NULL.
#'
#' @return Invisible TRUE if the operation was successful, FALSE otherwise.
#'         If 'list' action is selected, it prints the list of matched files.
#'
#' @examples
#' # List all files containing "report" in their names
#' manage_files(action = "list", match_any = "report")
#'
#' # Copy all files starting with "data_" and ending with "_2023.txt" to the "archive/" directory
#' manage_files(action = "copy", prefix = "data_", suffix = "_2023.txt", target_directory = "archive/")
#'
#' # Move all files ending with "_result.txt" to the parent directory
#' manage_files(action = "move", suffix = "_result.txt", target_directory = "../")
#'
#' @export
manage_files <- function(action = "list", prefix = "", suffix = "", match_any = "", target_directory = NULL) {
  # Check for valid target directory for copy or move actions
  if (!is.null(target_directory) && !grepl("/$", target_directory)) {
    target_directory <- paste0(target_directory, "/")
  }
  
  # Escape special characters in prefix, suffix, and match.any
  prefix_pattern <- gsub(".", "\\.", prefix, fixed = TRUE)
  suffix_pattern <- gsub(".", "\\.", suffix, fixed = TRUE)
  match_any_pattern <- gsub(".", "\\.", match_any, fixed = TRUE)
  
  # Construct the regex pattern based on the inputs
  pattern <- if (nzchar(match_any_pattern)) {
    paste0(".*", match_any_pattern, ".*")
  } else {
    paste0("^", prefix_pattern, ".*", suffix_pattern, "$")
  }
  
  # List all files matching the pattern in the current directory
  files_to_operate <- list.files(pattern = pattern)
  
  # Check if files exist to operate on
  if (length(files_to_operate) == 0) {
    cat("No files found.\n")
    return(invisible(FALSE))
  }
  
  # Handle different actions based on the user's choice
  if (action == "list") {
    print(files_to_operate)
    return(invisible(TRUE))
  } else if (is.null(target_directory)) {
    warning("Target directory must not be NULL for 'copy' or 'move' actions.")
    return(invisible(FALSE))
  } else if (action == "copy") {
    file_copied_successfully <- sapply(files_to_operate, function(file) {
      file.copy(file, paste0(target_directory, file))
    })
    if (all(file_copied_successfully)) {
      cat("Files successfully copied to", target_directory, "\n")
    } else {
      warning("Not all files were copied successfully.")
    }
  } else if (action == "move") {
    file_copied_successfully <- sapply(files_to_operate, function(file) {
      file.copy(file, paste0(target_directory, file))
    })
    if (all(file_copied_successfully)) {
      sapply(files_to_operate, file.remove)
      cat("Files successfully moved to", target_directory, "\n")
    } else {
      warning("Not all files were copied successfully. No files were removed.")
    }
  } else {
    warning("Invalid action specified. Please choose 'list', 'copy', or 'move'.")
    return(invisible(FALSE))
  }
  
  return(invisible(TRUE))
}
