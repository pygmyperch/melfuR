#' extract upper or lower triangle from a square matrix
#'
#' @param square.matrix a pairwise matrix object
#' @param result.file a character string name of your result file
#' @param upper a logical indicating that you want to keep the upper triangle (default=FALSE)
#' @param keep.diag a logical indicating that you want to keep the diagonal in your trimat (default=TRUE)
#' @return a trimatrix object and .csv file
#' @author Chris Brauer
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load example matrix
#'  example.mat <- matrix(c(0,3,5,6, 3,0,6,6, 5,6,0,3, 6,6,3,0), 
#'                 nrow = 4, ncol = 4, dimnames = list(c("A", "B", "C", "D"), 
#'                 c("A", "B", "C", "D")))
#'
#'  # extract lower trimarix
#'  Lmat <- extract_tri(example.mat, "lowermat")
#'  
#'  # extract upper trimarix without the diagonal
#'  Umat <- extract_tri(example.mat, "uppermat", upper = TRUE, keep.diag = FALSE)




extract_tri <- function (square.matrix, result.file, upper = FALSE, keep.diag = TRUE) {
  
  sqmat <- as.matrix(square.matrix)
  
  if (!keep.diag) {
    
    if (!upper) {
      #Hide upper triangle
      top <- sqmat
      top[upper.tri(sqmat, diag = TRUE)] <-""
      trimat <- as.data.frame(top)
      
    } else {
      #Hide lower triangle
      lower <- sqmat
      lower[lower.tri(sqmat, diag = TRUE)] <-""
      trimat <- as.data.frame(lower)
      
    }
    
    
    
    
  } else {

    if (!upper) {
      #Hide upper triangle
      top <- sqmat
      top[upper.tri(sqmat)] <-""
      trimat <- as.data.frame(top)
      
    } else {
      #Hide lower triangle
      lower <- sqmat
      lower[lower.tri(sqmat)] <-""
      trimat <- as.data.frame(lower)
      
    }
    
    
    
  }
  

  
  write.csv(trimat, file=paste0(result.file,".csv"))
  
  return(trimat)
  
}
