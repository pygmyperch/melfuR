#' A wrapper function for the hw.test function from pegas to filter a genind object for per locus Hardy-Weinberg equilibrium by population (i.e. remove loci out of HWE in >x\% of populations at a specified FDR).
#'
#'
#'
#' @param GenInd.obj a genind object
#' @param perm the number of permutations for the monte carlo simulations
#' @param FDR_cut the false discovery rate threshold
#' @param pop_thresh the minimum number (\%) of populations each locus needs to be in HWE to be retained
#' @param no_cores the number of cores to use for the analysis
#'
#'
#' @return FDRadjP.csv           a table of FDR adjusted P values for each locus per population
#' @return OutOfHWE_SNPs.csv     a table of SNPs out of HWE at the specified FDR in >pop_thresh \% of populations
#' @return HWE_SNPs.csv          a table of SNPs in HWE at the specified FDR in >pop_thresh populations
#' @return HWE_list              a vector of SNPs in HWE suitable for input to the subset_snps function
#'
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load data
#'  data(rainbow.genind)
#'
#'  #subset rainbow.genind to 100 loci
#'  gen100 <- rainbow.genind[ ,1:200]
#'
#'  # run HWE_filter to exclude loci out of HWE in >50\% of populations at a FDR of 0.1
#'  run1 <- HWE_filter(gen100, perm=999, FDR_cut=0.1, pop_thresh=50, no_cores=6)
#'
#'
#'  ## use output to generate new structure file retaining the 97 loci in HWE
#'
#'  # load original structure file
#'    stfile <- system.file("extdata", "Mfsub500.stru", package="melfuR")
#'
#'  # run subset_snps using output of HWE_filter
#'    subset_snps(stfile, "new_structure_file", run1)
#'
#' @importFrom adegenet locNames popNames seppop
#' @importFrom pegas hw.test
#' @importFrom parallel clusterEvalQ stopCluster


HWE_filter <- function(GenInd.obj, perm, FDR_cut, pop_thresh, no_cores) {

  # #check required packages are installed and loaded
  # packages <- c("adegenet", "pegas", "plyr", "foreach", "doParallel")
  # 
  # for(package in packages){
  # 
  #   # if package is installed locally, load
  #   if(package %in% rownames(installed.packages()))
  #     do.call('require', list(package))
  # 
  #   else {
  #     stop("some required packages not loaded")
  #   }
  # }
  # 
  #define several variables
  loci <- locNames(GenInd.obj)
  pops <- popNames(GenInd.obj)
  N <- as.numeric(length(loci))
  indpops <- seppop(GenInd.obj)

  if (no_cores ==1) {

    HWE <- sapply(indpops, function(x) hw.test(x, B=perm))

  } else {

    cl <- makeCluster(no_cores)
    clusterEvalQ(cl, library("pegas"))
    HWE <- parSapply(cl, indpops, function(x, perm) hw.test(x, B=perm), perm)
    stopCluster(cl)

  }


  #subset matrix to only include exact p values (last N rows)
  HWE_P <- tail(HWE, N)
  row.names(HWE_P) <- loci
  colnames(HWE_P) <- pops
  HWE_P <- as.data.frame(HWE_P)


  FDRadjP <- cbind(colwise(p.adjust, method="fdr")(HWE_P))
  write.csv(FDRadjP, "FDRadjP.csv")

  #create column of population counts for each SNP (how many pops have HWE estimation)
  NumEstimates <- as.data.frame(apply(FDRadjP, MARGIN = 1, FUN = function(x) length(x[!is.na(x)])))
  colnames(NumEstimates) <- "NumEstimates"

  #create column of number of P values <FDR_cut for each SNP
  NumSig <- as.data.frame(apply(FDRadjP, MARGIN = 1, FUN = function(x) sum(x[!is.na(x)] < FDR_cut)))
  colnames(NumSig) <- "NumSig"
  SigSnps <- cbind(NumEstimates,NumSig)

  #calculate % of pops out of HWE at "FDR_cut" FDR for each SNP
  propOut <- as.data.frame(SigSnps$NumSig/SigSnps$NumEstimates*100)
  colnames(propOut) <- "propOut"
  SigSnps <- cbind(NumEstimates, NumSig, propOut)
  row.names(SigSnps) <- loci

  #subset list of SNPs in HWE at "FDR_cut" FDR in at least pop_thresh % of populations
  HWE_SNPs <- subset(SigSnps, SigSnps$propOut < pop_thresh)
  HWE_list <- as.data.frame(rownames(HWE_SNPs))
  write.csv(HWE_SNPs, "HWE_SNPs.csv")

  #subset list of SNPs out of HWE at FDR10% in at least pop_thresh % of populations
  OutOfHWE_SNPs <- subset(SigSnps, SigSnps$propOut >= pop_thresh)
  write.csv(OutOfHWE_SNPs, "OutOfHWE_SNPs.csv")
  num_out <- length(OutOfHWE_SNPs[ ,1])
  retained <- length(HWE_SNPs[ ,1])

  print(noquote(paste0(num_out, " loci are out of HWE in more than ", pop_thresh, "% of populations")))
  print(noquote(paste0(retained, " remaining loci are considered to be in HWE")))
  print(noquote("Have a nice day."))

  return(HWE_list)
}
