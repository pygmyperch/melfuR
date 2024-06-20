___
[![Alt text](../master/images/melfu_logo.png)](http://www.molecularecology.flinders.edu.au/)



## http://www.molecularecology.flinders.edu.au/&nbsp; &nbsp; [![Alt text](../master/images/fb3.png)](https://www.facebook.com/molecularecologylab/)&nbsp; &nbsp; &nbsp; &nbsp; <span style="font-size:larger;">Chris Brauer</span>&nbsp; [![Alt text](../master/images/mail3.png)](mailto:chris.brauer@flinders.edu.au)&nbsp; &nbsp; [![Alt text](../master/images/twitter2.png)](https://twitter.com/pygmyperch)
___
<br/>

# melfuR

<br/>


A collection of ugly R scripts for exploratory analysis of ddRAD data, spatial analyses and other (hopefully) useful utilities.

These are mostly just wrapper scripts to simplify running basic pop-gen analyses using a single format (adegenet: genind)
\
The code is characteristically ugly :dizzy_face: but feel free to use/modify whatever you find useful

<br/>

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

* `impute.data`: impute missing genotypes in a genind object

* `percent_poly`: calculate the % polymorphic loci per population from a genind object

* `diversity_stats`: calculate basic genetic diversity stats from a genind object

* `missing_data`: calculate the % of missing genotypes per population from a genind object

* `subset_snps`: subset a structure file based on a list of loci

* `xy2kml`: **deprecated function, use sf2KML instead**

* `viamaris`: calculate pairwise oceanic distances from a set of XY coordinates

* `extract_tri`: extract upper or lower triangle from a square matrix

* `sf2KML`: convert a sf point object to a .kml file

* `clean_vcf_INFO`: Parse and clean-up INFO Fields in a VCF File to remove any fields not consistently present across all records


