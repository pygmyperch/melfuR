#' extract upper or lower triangle from a square matrix
#'
#' @param square.matrix a pairwise matrix object
#' @param result.file a pairwise matrix object
#' @param upper a logical indicating that you want to keep the upper triangle (default=FALSE)
#' @return a trimatrix objecgt and .csv file
#' @author Chris Brauer
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load example matrix
#'  example.mat <- matrix(c(0,3,5,6, 3,0,6,6, 5,6,0,3, 6,6,3,0), nrow = 4, ncol = 4, 
#'               dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D")))
#'
#'  # extract lower trimarix
#'  Ltrimat <- extract_tri(sqmat, lowermat)
#'  
#'  # extract upper trimarix
#'  Utrimat <- extract_tri(example.mat, uppermat, upper = TRUE)




extract_tri <- function (square.matrix, result.file, upper = FALSE) {
  
  sqmat <- as.matrix(sqmat)
  
  if (!upper) {
    #Hide upper triangle
    top <- sqmat
    top[upper.tri(sqmat)] <-""
    trimat <- as.data.frame(top)
    
  } else {
    #Hide lower triangle
    lower <- sqmat
    lower[lower.tri(sqmat, diag=TRUE)] <-""
    trimat <- as.data.frame(lower)
    
  }
  

  
  write.csv(trimat, file=paste0(result.file,".csv"))
  
  return(trimat)
  
}
