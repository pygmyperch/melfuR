#' This function calculates the percent of missing genotypes for each population from a genind object
#'
#' @param input.file A genind object
#' @keywords missing data
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  ## load the example data
#'  data(rainbow.genind)
#'
#'  # calculate the percent polymorphic loci per population
#'  res <- missing_data(rainbow.genind)
#'



missing_data <- function(genind) {
  
  poly.lst <- seppop(genind)
  miss.geno <- lapply(poly.lst, function(x) {(sum(is.na(x@tab)))/(dim(x@tab)[1]*dim(x@tab)[2])*100})
  miss.geno <- data.frame(round(sapply(miss.geno,c), digits = 2))
  colnames(miss.geno) <- "%missing"
  return(miss.geno)
  
}

