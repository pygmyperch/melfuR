#' This function calculates the percent polymorphic loci per population from a genind object
#'
#' @param input.file A genind object
#' @keywords genetic diversity polymorphic loci
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  ## load the example data
#'  data(rainbow.genind)
#'
#'  # calculate the percent polymorphic loci per population
#'  res <- percent_poly(rainbow.genind)
#'



percent_poly <- function(genind) {
  
  poly.lst <- seppop(genind)
  percent.poly <- lapply(poly.lst, function(x) {length(which(nAll(x)==2))/(length(which(nAll(x)==1)) + length(which(nAll(x)==2)))*100})
  percent.poly <- data.frame(round(sapply(percent.poly,c), digits = 2))
  colnames(percent.poly) <- "%poly"
  return(percent.poly)
  
}



