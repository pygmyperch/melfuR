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
  # Create a new workbook
  wb <- createWorkbook()
  
  # Iterate over each element in the list
  for (sheet_name in names(data_list)) {
    # Add a new sheet to the workbook
    addWorksheet(wb, sheet_name)
    
    # Ensure the data is a data frame before writing to Excel
    data_to_write <- data_list[[sheet_name]]
    if (!is.data.frame(data_to_write)) {
      data_to_write <- as.data.frame(data_to_write)
    }
    
    # Write the data to the sheet
    writeData(wb, sheet_name, data_to_write, rowNames = TRUE, na.string = na.string)
  }
  
  # Save the workbook to file
  saveWorkbook(wb, file_name, overwrite = TRUE)
}
