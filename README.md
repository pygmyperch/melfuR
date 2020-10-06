# melfuR
A collection of R scripts for exploratory analysis of ddRAD data, spatial analyses and other (hopefully) useful utilities.

# How to Install

The preferred way to install this package is using devtools:

First install devtools

```r
install.packages("devtools")
```

Then install melfuR

```r
devtools::install_github("pygmyperch/melfuR")
```

A quick overview of some of the key functions:

* `rainbow.genind`: An example genind object consisting of 500 loci and 249 individuals from 14 populations of *Melanotaenia fluviatilis* in the Murray-Darling Basin. 

* `rainbow.env`: Environmental data for 14 populations of *Melanotaenia fluviatilis* in the Murray-Darling Basin. 

* `run.gINLAnd`: A wrapper function to run gINLAnd GEA analysis on a genind object

* `HWE_filter`: filter loci for HWE by population using a genind object

* `percent_poly`: calculate the % polymorphic loci per population from a genind object

* `missing_data`: calculate the % of missing genotypes per population from a genind object

* `subset_snps`: subset a structure file based on a list of loci

* `xy2kml`: convert a set of XY coordinates to a .kml file for use with Google Earth

* `viamaris`: calculate pairwise oceanic distances from a set of XY coordinates

* `extract_tri`: extract upper or lower triangle from a square matrix


