#' Sort Alleles in genlight or genind Objects as Major/Minor
#'
#' This function sorts alleles in `genlight` or `genind` objects based on their major/minor status. Optionally, it allows specifying reference populations to analyze relative to a focal population.
#'
#' @param geno A `genlight` or `genind` object containing the genotype data.
#' @param pops A character vector specifying the populations to use as a reference for sorting alleles. If not supplied, alleles are sorted across all populations in the data. (optional)
#'
#' @details 
#' The function first checks if the input is a `genlight` object and converts it to a `genind` object if necessary. 
#' The alleles are then sorted based on their major/minor status (most common/least common). If the `pops` parameter is specified, the alleles are sorted 
#' relative to the specified focal populations. This can be useful for analyzing the genetic structure or allele frequency 
#' differences between specific populations. If no focal populations are provided, the alleles are sorted across all populations 
#' in the dataset.
#'
#' @return A `genind` or `genlight` object with alleles sorted as major/minor.
#'
#' @importFrom dartR.base gl2gi gi2gl
#'
#' @examples
#' \dontrun{
#'
#' # Example using a genlight object
#' data(nancycats)
#' genlight_obj <- glSim(nInd = 50, nLoc = 100)
#' sorted_genlight <- sort_alleles(genlight_obj)
#'
#' # Example using a genind object
#' genind_obj <- nancycats
#' sorted_genind <- sort_alleles(genind_obj)
#'
#' # Example specifying a focal population
#' sorted_genind_focal <- sort_alleles(genind_obj, pops = "P01")
#' 
#' #' # Example specifying a group of focal populations
#' sorted_genind_focals <- sort_alleles(genind_obj, pops = c("P01", "P02"))

#' }
#' @export
sort_alleles <- function(geno, pops=NULL) {
  
  # Check if genlight or genind object is supplied
  input.class <- class(geno)[1]
  
  # Convert genlight to genind if necessary
  wasGenlight <- FALSE
  if(input.class == "genlight") {
    geno <- gl2gi(geno)
    wasGenlight <- TRUE
  }
  
  # If focal population not supplied, calculate minor allele across all data
  if(is.null(pops)) {
    pops <- levels(geno@pop)
  }
  
  tab <- as.data.frame(geno@tab)
  tab$pop <- geno@pop
  tab <- tab[tab$pop %in% pops,]
  tab <- within(tab, rm(pop))
  
  res <- as.data.frame(colSums(tab, na.rm = TRUE))
  colnames(res) <- "allele_count"
  res$allele_name <- rownames(res)
  
  my.ord <- do.call(c, lapply(seq(2, nrow(res), by = 2), (function(i){
    c(i-1, i)[order(res$allele_count[(i-1):i], decreasing = TRUE)]})))
  
  res1 <- data.frame(res[my.ord,])
  
  geno@tab <- geno@tab[, res1$allele_name]
  
  # Convert back to genlight if the original input was genlight
  if(wasGenlight) {
    geno <- gi2gl(geno) # Assuming a hypothetical gi2gl function for conversion
  }
  
  return(geno)
}
