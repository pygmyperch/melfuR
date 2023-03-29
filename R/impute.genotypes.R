#' impute missing genotypes based on pop structure characterised using snmf
#'
#'
#' @param input.file A genind object
#' @param K the number of predetermined clusters in your data (suggest using smnf, ADMIXTURE or similar to explore first)
#' @param no_cores the number of cores to use for the analysis
#'
#' @return imputed.genind               a genind object with missing data imputed
#'
#'
#' @author Chris Brauer
#' @keywords genotype imputation
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load example coordinate file
#'  data(rainbow.genind)
#'
#'  # run analysis
#'  imputed.gi <- impute.data(rainbow.genind)



impute.data <- function(input.file, K=1, no_cores=1) {


  #check required packages are installed and loaded
  packages <- "LEA"
  for(package in packages){
    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('require', list(package))
    else {
      stop("some required packages not loaded")
    }
  }


  alleles <- input.file@tab
  snps <- alleles[,seq(1,ncol(alleles),2)]
  colnames(snps) <- locNames(input.file)
  md <- round((sum(is.na(snps)))/(dim(snps)[1]*dim(snps)[2])*100, digits = 2)
  cat(paste0("Total missing data = "),md,"%\n")
  write.lfmm(snps, "dat.lfmm")
  lfmm.obj = read.lfmm("dat.lfmm")

  project.snmf = snmf("dat.lfmm", K = K,
                      entropy = TRUE, repetitions = 10, seed = 42, CPU = no_cores,
                      project = "new")
  # select the run with the lowest cross-entropy value
  best = which.min(cross.entropy(project.snmf, K = K))


  # Impute the missing genotypes
  impute(project.snmf, "dat.lfmm", method = 'mode', K = K, run = best)
  imputed.snps <- read.lfmm("dat.lfmm_imputed.lfmm")
  dim(imputed.snps)
  #imputed.snps[1:10,1:10]
  colnames(imputed.snps) <- locNames(input.file)
  rownames(imputed.snps) <- indNames(input.file)

  # check total % missing data
  md2 <- (sum(is.na(imputed.snps)))/(dim(imputed.snps)[1]*dim(imputed.snps)[2])*100
  cat(paste0("Total missing data was = "),md,"%\n")
  cat(paste0("Total missing data now = "),md2,"%\n")


  imputed.snps2 <- 2-imputed.snps
  #create indexes for the desired order
  x <- order(c(1:ncol(imputed.snps), 1:ncol(imputed.snps2)))
  #cbind d1 and d2, interleaving columns with x
  dat <- cbind(imputed.snps, imputed.snps2)[,x]
  colnames(dat) <- colnames(input.file@tab)

  imputed.genind <- input.file
  imputed.genind@tab <- dat
  mode(imputed.genind@tab) <- "integer"
  return(imputed.genind)
}
