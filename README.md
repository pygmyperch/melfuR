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

* `rainbow.genind`: An example genind object consisting of 500 loci and 249 individuals from 14 populations of *Melanotaenia fluviatilis* in the Murray-Darling Basin. 

* `subset_snps`: A function to subset a structure file based on a list of loci

* `rainbow.env`: Environmental data for 14 populations of *Melanotaenia fluviatilis* in the Murray-Darling Basin. 

Usage:

```r
data(rainbow.genind)
test_dapc(rainbow.genind)
```

