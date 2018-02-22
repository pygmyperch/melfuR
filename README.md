# melfuR
A collection of R scripts for exploratory analysis of ddRAD data and other (hopefully) useful utilities.

# How to Install

The preferred way to install this package is using devtools:

First install devtools

```r
install.packages("devtools")
library("devtools")
```

Then install melfuR

```r
devtools::install_github("pygmyperch/melfuR")
```

A quick overview of some of the key functions:

* `rainbow.genind`: An example genind object consisting of 500 loci and 249 individuals from 14 populations from Melanotaenia fluviatilis in the Murray-Darling Basin. 

* `test_dapc`: A function to run a simple DAPC on the example SNP data using the ADEGENET package.
Usage:

```r
data(rainbow.genind)
test_dapc(rainbow.genind)
```




A function to run a simple DAPC on the example SNP data using the ADEGENET package.
test_dapc
Usage:
data(rainbow.genind)
test_dapc(rainbow.genind)
