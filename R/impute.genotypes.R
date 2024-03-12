#' impute missing genotypes based on pop structure characterised using snmf
#'
#'
#' @param input.file A genind object
#' @param K the number of predetermined clusters in your data (suggest using smnf, ADMIXTURE or similar to explore first)
#' @param no_cores the number of cores to use for the analysis
#'
#' @return a genind object with missing data imputed
#'
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
#'  imputed.gi <- impute.data(rainbow.genind, K = 6)
#'  
#' @export
#' @importFrom LEA write.geno snmf cross.entropy impute read.lfmm
#' @importFrom adegenet locNames indNames


impute.data <- function (input.file, K = NULL, no_cores = 1) 
{

  alleles <- input.file@tab
  snps <- alleles[, seq(1, ncol(alleles), 2)]
  colnames(snps) <- locNames(input.file)
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
  colnames(imputed.snps) <- locNames(input.file)
  rownames(imputed.snps) <- indNames(input.file)
  md2 <- (sum(is.na(imputed.snps)))/(dim(imputed.snps)[1] * 
                                       dim(imputed.snps)[2]) * 100
  cat(paste0("Total missing data was = "), md, "%\n")
  cat(paste0("Total missing data now = "), md2, "%\n")
  
  imputed.snps2 <- 2 - imputed.snps
  x <- order(c(1:ncol(imputed.snps), 1:ncol(imputed.snps2)))
  dat <- cbind(imputed.snps, imputed.snps2)[, x]
  colnames(dat) <- colnames(input.file@tab)
  
  imputed.genind <- input.file
  imputed.genind@tab <- dat
  mode(imputed.genind@tab) <- "integer"
  
  return(imputed.genind)
  
}




