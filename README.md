# melfuR
A collection of R scripts for exploratory analysis of ddRAD data and other (hopefully) useful utilities.

# Installation:
devtools::install_github("pygmyperch/melfuR")


# Functions

A function to run a simple DAPC on the example SNP data using the ADEGENET package.
test_dapc
Usage:
data(rainbow.genind)
test_dapc(rainbow.genind)
