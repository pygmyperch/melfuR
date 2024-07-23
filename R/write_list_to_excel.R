#' Write a List of Data Frames to an Excel File
#'
#' This function writes a list of data frames to an Excel file, with each element of the list being written to a separate sheet.
#' It preprocesses the data frames to replace NA values with a placeholder string, writes the data to the workbook,
#' and then clears the cells containing the placeholder string to ensure they are treated as truly empty cells in Excel.
#'
#' @param data_list A named list of data frames to be written to the Excel file. Each list element will be written to a separate sheet.
#' @param file_name A character string specifying the name of the output Excel file.
#' @param placeholder A character string used as a placeholder to replace NA values in the data frames. Default is "NA_PLACEHOLDER".
#'
#' @return This function does not return a value. It writes the list of data frames to an Excel file.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData deleteData saveWorkbook
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
#' # Write the list to an Excel file with each element on a separate sheet
#' write_list_to_excel(results, "example.xlsx")
#' }
#' @export
write_list_to_excel <- function(data_list, file_name, placeholder = "NA_PLACEHOLDER") {
  
  # Helper function to preprocess data frame to replace any NAs with a placeholder string
  # Args:
  #   df: The data frame to preprocess.
  #   placeholder: A string used as a placeholder to replace NA values.
  # Returns:
  #   A data frame with NA values replaced by the placeholder string.
  replace_na_with_placeholder <- function(df, placeholder = "NA_PLACEHOLDER") {
    df[] <- lapply(df, function(x) {
      if (is.factor(x)) {
        x <- as.character(x)
      }
      x[is.na(x)] <- placeholder
      return(x)
    })
    return(df)
  }
  
  wb <- createWorkbook()
  
  for (sheet_name in names(data_list)) {
    addWorksheet(wb, sheet_name)
    data_to_write <- data_list[[sheet_name]]
    
    if (!is.data.frame(data_to_write)) {
      data_to_write <- as.data.frame(data_to_write)
    }
    
    # Replace NA values with the placeholder
    data_to_write <- replace_na_with_placeholder(data_to_write, placeholder)
    
    # Write the data to the sheet
    writeData(wb, sheet_name, data_to_write, rowNames = FALSE, colNames = TRUE)
    
    # Identify and clear cells with the placeholder
    na_positions <- which(data_to_write == placeholder, arr.ind = TRUE)
    for (pos in seq_len(nrow(na_positions))) {
      row <- na_positions[pos, "row"]
      col <- na_positions[pos, "col"]
      # Adjust rows and cols to account for headers and 1-based indexing in openxlsx
      deleteData(wb, sheet = sheet_name, cols = col, rows = row + 1)
    }
  }
  
  # Save the workbook
  saveWorkbook(wb, file_name, overwrite = TRUE)
}
