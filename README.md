# melfuR
A collection of R scripts for exploratory analysis of ddRAD data and other (hopefully) useful utilities.

# Installation:
devtools::install_github("pygmyperch/melfuR")


# test_dapc.R
A function to run a simple DAPC on the example SNP data using the ADEGENET package.

Usage:

data(rainbow.genind)
test_dapc(rainbow.genind)
