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

A quick overview of the functions:

* `rainbow.genind`: an example genind object consisting of 500 loci and 249 individuals from 14 populations of *Melanotaenia fluviatilis* in the Murray-Darling Basin. 

* `rainbow.env`: environmental data for 14 populations of *Melanotaenia fluviatilis* in the Murray-Darling Basin. 

* `DelphinusXY`: XY coordinates of common dolphin *Delphinus delphis* sampling locations in Australia and New Zealand. 

* `MDB_cols`: custom color palette based on the Murray-Darling Basin landscape.

* `view_colors`: view the MDB color palette.

* `expand_pop2ind`: expands population-level metadata to an individual-level data frame.

* `HWE_filter`: filter loci for HWE by population using a genind object

* `impute_genotypes`: impute missing genotypes in a genind or genlight object using population allele frequencies estimated with Sparse Non-Negative Matrix Factorization (LEA::snmf).

* `sort_alleles`: sort alleles in a genind or genlight object based major/minor status, optionally sort relative to a specified reference population.

* `percent_poly`: calculate the % polymorphic loci per population from a genind object

* `diversity_stats`: calculate basic genetic diversity stats from a genind object

* `missing_data`: calculate the % of missing genotypes per population from a genind object

* `subset_snps`: subset a structure file based on a list of loci

* `viamaris`: calculate pairwise oceanic distances from a set of XY coordinates.

* `extract_tri`: extract upper or lower triangle from a square matrix.

* `sf2KML`: convert sf points, lines, and polygon objects to .kml files.

* `clean_vcf_INFO`: clean-up INFO fields in a VCF file to remove any fields not present across all records.

* `polarise_ancestral_vcf`: polarise a VCF file setting the (supplied) ancestral allele as REF.

* `manage_files`: perform file operations such as listing, copying, and moving files.

* `write_list_to_excel`: write a list of data frames to an Excel file (.xlsx).


