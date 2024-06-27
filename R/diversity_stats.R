#' This function calculates basic genetic diversity stats from a genind or genlight object
#'
#' @param input.file A genind or genlight object
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  ## load the example data
#'  data(rainbow.genind)
#'
#'  # calculate Na, %poly, Ho, He, and Fis (with 95%CIs) per population
#'  res <- diversity_stats(rainbow.genind)
#'
#' @export
#' @importFrom hierfstat genind2hierfstat allelic.richness basic.stats boot.ppfis
#' @importFrom adegenet seppop isPoly nAll
#' @importFrom dartR.base gl2gi

diversity_stats <- function(input.file) {
  # Check if input is a genlight object and convert to genind if necessary
  if (inherits(input.file, "genlight")) {
    genind <- dartR.base::gl2gi(input.file)
  } else if (inherits(input.file, "genind")) {
    genind <- input.file
  } else {
    stop("Input file must be a genind or genlight object.")
  }
  
  f.stat <- genind2hierfstat(genind)
  # get number of inds per pop
  pop.numbers <- as.data.frame(table(f.stat$pop))
  pop.numbers <- as.data.frame(pop.numbers$Freq)
  
  # estimate allelic richness
  Na <- allelic.richness(f.stat)
  popNa <- as.data.frame(colMeans(Na$Ar, na.rm = TRUE))
  
  # estimate Ho, He, Fis
  stats <- basic.stats(f.stat, digits = 4)
  Ho <- as.data.frame(stats$Ho)
  popHo <- as.data.frame(colMeans(Ho, na.rm = TRUE))
  He <- as.data.frame(stats$Hs)
  popHe <- as.data.frame(colMeans(He, na.rm = TRUE))
  popFis <- as.data.frame(1 - colSums(stats$Ho, na.rm = TRUE) / colSums(stats$Hs, na.rm = TRUE))
  
  # estimate 95%CIs around the Fis values
  geno.fis <- boot.ppfis(f.stat)
  FisCI <- as.data.frame(geno.fis$fis.ci)
  
  # split input into list of data.frames by pop factor and calc %poly loci
  poly.lst <- seppop(genind)
  percent.poly <- lapply(poly.lst, function(x) {
    (sum(isPoly(x))) / (dim(x@tab)[2] / 2 - (length(which(nAll(x) == 0)))) * 100
  })
  percent.poly <- data.frame(sapply(percent.poly, c))
  
  geno.stats <- cbind(pop.numbers, popNa, percent.poly, popHo, popHe[, 1], popFis[, 1], FisCI)
  colnames(geno.stats) <- c("N", "Na", "%poly", "Ho", "He", "Fis", "LCI", "UCI")
  write.csv(geno.stats, "stats.csv")
  
  return(geno.stats)
}
