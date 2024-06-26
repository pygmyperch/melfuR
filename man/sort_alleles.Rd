% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sort_alleles.R
\name{sort_alleles}
\alias{sort_alleles}
\title{Sort Alleles in genlight or genind Objects as Major/Minor}
\usage{
sort_alleles(geno, pops = NULL)
}
\arguments{
\item{geno}{A `genlight` or `genind` object containing the genotype data.}

\item{pops}{A character vector specifying the populations to use as a reference for sorting alleles. If not supplied, alleles are sorted across all populations in the data. (optional)}
}
\value{
A `genind` or `genlight` object with alleles sorted as major/minor.
}
\description{
This function sorts alleles in `genlight` or `genind` objects based on their major/minor status. Optionally, it allows specifying reference populations to analyze relative to a focal population.
}
\details{
The function first checks if the input is a `genlight` object and converts it to a `genind` object if necessary. 
The alleles are then sorted based on their major/minor status (most common/least common). If the `pops` parameter is specified, the alleles are sorted 
relative to the specified focal populations. This can be useful for analyzing the genetic structure or allele frequency 
differences between specific populations. If no focal populations are provided, the alleles are sorted across all populations 
in the dataset.
}
\examples{
\dontrun{

# Example using a genlight object
data(nancycats)
genlight_obj <- glSim(nInd = 50, nLoc = 100)
sorted_genlight <- sort_alleles(genlight_obj)

# Example using a genind object
genind_obj <- nancycats
sorted_genind <- sort_alleles(genind_obj)

# Example specifying a focal population
sorted_genind_focal <- sort_alleles(genind_obj, pops = "P01")

#' # Example specifying a group of focal populations
sorted_genind_focals <- sort_alleles(genind_obj, pops = c("P01", "P02"))
}
}
