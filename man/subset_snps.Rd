% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_snps.R
\name{subset_snps}
\alias{subset_snps}
\title{This function subsets a structure file based on a list of loci to retain}
\usage{
subset_snps(input.file, result.file, loci.to.keep, popnumbers = FALSE)
}
\arguments{
\item{input.file}{A character string giving the path to the file to subset}

\item{result.file}{A character string name of your result file}

\item{loci.to.keep}{A vector of character strings naming the loci to retain (based on the locus names in the original file)}

\item{popnumbers}{an optional logical stating whether to convert the population column from character strings to numbers (default = FALSE)}
}
\description{
This function subsets a structure file based on a list of loci to retain
}
\examples{
 ## set directory for results to be written
 setwd("path/to/working/directory")

 # load example structure file
 stfile <- system.file("extdata", "Mfsub500.stru", package="melfuR")
 # generate list of loci to keep
 snplist <- as.data.frame(c("SNP_1", "SNP_45", "SNP_96"))

 # subset file to the three loci, keeping original population codes
 subset_snps(stfile, "new_file", snplist)

 # subset file to three loci, replacing original population codes with numbers
 subset_snps(stfile, "new_file", snplist, popnumbers = TRUE)
}
