#' impute missing genotypes based on pop structure characterised using snmf
#'
#'
#' @param input.genotypes A genind or genlight object
#' @param K the number of predetermined clusters in your data (suggest using smnf, ADMIXTURE or similar to explore first)
#' @param no_cores the number of cores to use for the analysis
#'
#' @return a genind or genlight object with missing data imputed
#'
#'#' @details
#' The function imputes missing genotypes based on ancestral population allele frequencies estimated using LEA::snmf()
#'
#' @author Chris Brauer
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load example coordinate file
#'  data(rainbow.genind)
#'
#'  # run analysis for K=6
#'  imputed.gi <- impute_genotypes(rainbow.genind, K = 6)
#'  
#' @export
#' @importFrom LEA write.geno snmf cross.entropy impute read.lfmm
#' @importFrom adegenet locNames indNames


impute_genotypes <- function (input.genotypes, K = NULL, no_cores = 1) {

  # Store the original class of the input
  input_class <- class(input.genotypes)
  
  # Check if input is a genlight object and convert to genind if necessary
  if (inherits(input.genotypes, "genlight")) {
    input.genotypes <- dartR.base::gl2gi(input.genotypes)
  } else if (!inherits(input.genotypes, "genind")) {
    stop("Input file must be a genind or genlight object.")
  }
  
  alleles <- input.genotypes@tab
  snps <- alleles[, seq(1, ncol(alleles), 2)]
  colnames(snps) <- locNames(input.genotypes)
  md <- round((sum(is.na(snps)))/(dim(snps)[1] * dim(snps)[2]) * 
                100, digits = 2)
  
  cat(paste0("Total missing data = "), md, "%\n")
  snps[is.na(snps)] <- 9
  write.geno(snps, "dat.geno")
  
  snmf = snmf("dat.geno", K = K, entropy = TRUE, repetitions = 10, 
              seed = 42, CPU = no_cores, project = "new")
  best = which.min(cross.entropy(snmf, K = K))
  
  impute(snmf, "dat.geno", method = "mode", K = K, 
         run = best)
  imputed.snps <- read.lfmm("dat.lfmm_imputed.lfmm")
  #dim(imputed.snps)
  colnames(imputed.snps) <- locNames(input.genotypes)
  rownames(imputed.snps) <- indNames(input.genotypes)
  md2 <- (sum(is.na(imputed.snps)))/(dim(imputed.snps)[1] * 
                                       dim(imputed.snps)[2]) * 100
  cat(paste0("Total missing data was = "), md, "%\n")
  cat(paste0("Total missing data now = "), md2, "%\n")
  
  imputed.snps2 <- 2 - imputed.snps
  x <- order(c(1:ncol(imputed.snps), 1:ncol(imputed.snps2)))
  dat <- cbind(imputed.snps, imputed.snps2)[, x]
  colnames(dat) <- colnames(input.genotypes@tab)
  
  imputed.genotypes <- input.genotypes
  imputed.genotypes@tab <- dat
  mode(imputed.genotypes@tab) <- "integer"
  
  # Convert to genlight if input was genlight
  if ("genlight" %in% input_class) {
    imputed.genotypes <- dartR.base::gi2gl(imputed.genotypes)
  }
  
  # Return the processed object, either genind or genlight
  return(imputed.genotypes)

}




