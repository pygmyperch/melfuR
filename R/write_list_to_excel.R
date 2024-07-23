#' Write a List of Data Frames to an Excel File
#'
#' This function writes a list of data frames to an Excel file, with each element of the list being written to a separate sheet.
#'
#' @param data_list A named list of data frames to be written to the Excel file. Each list element will be written to a separate sheet.
#' @param file_name A character string specifying the name of the output Excel file.
#'
#' @return This function does not return a value. It writes the list of data frames to an Excel file.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @examples
#' \dontrun{
#' Example workflow:
#' 
#' # Create an empty list to store results from some analyses
#' results <- list()
#'
#' # results from each analysis step are added to the list
#' analysis1_result <- data.frame(A = 1:5, B = letters[1:5])
#' results$analysis1_result <- as.data.frame(analysis1_result)
#'
#' analysis2_result <- data.frame(X = rnorm(5), Y = runif(5))
#' results$analysis2_result <- as.data.frame(analysis2_result)
#'
#' # Write the list to an Excel file with each element on a seperate sheet
#' write_list_to_excel(results, "example.xlsx")
#' }
#' @export
write_list_to_excel <- function(data_list, file_name) {
  
  # Function to replace NA with NULL in a data frame
  replace_na_with_null <- function(df) {
    for (i in seq_along(df)) {
      df[[i]][is.na(df[[i]])] <- ""
    }
    return(df)
  }
  
  wb <- createWorkbook()
  for (sheet_name in names(data_list)) {
    addWorksheet(wb, sheet_name)
    data_to_write <- data_list[[sheet_name]]
    if (!is.data.frame(data_to_write)) {
      data_to_write <- as.data.frame(data_to_write)
    }
    
    # Replace NA values with NULL
    data_to_write <- replace_na_with_null(data_to_write)
    
    writeData(wb, sheet_name, data_to_write, rowNames = TRUE)
  }
  saveWorkbook(wb, file_name, overwrite = TRUE)
}
