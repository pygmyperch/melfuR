#' Polarise a VCF File setting the (supplied) Ancestral Allele as REF
#'
#' This function reads a VCF file, reorders the REF and ALT alleles based on the ancestral allele 
#' specified in the INFO field (ID=AA) (the AA INFO field must be present in the vcf), adjusts the REF, ALT and genotype (GT) fields accordingly, and writes 
#' the modified VCF to a new file.
#'
#' @param input_vcf Path to the input VCF file.
#' @param output_vcf Path to the output VCF file. If not provided, defaults to "output.vcf.gz".
#' @param compress Optional boolean indicating whether to compress the output file. If not provided, it will automatically detect based on the input file.
#'
#' @return The function writes the modified VCF file to the specified output path and returns 
#' a modified vcfR object to the environment
#'
#' @examples
#' polarise_ancestral_vcf("input.vcf")
#'
#' @importFrom vcfR read.vcfR extract.gt extract.info write.vcf
#' @importFrom stringr str_replace_all
#' @importFrom R.utils gunzip

polarise_ancestral_vcf <- function(input_vcf, output_vcf = "output.vcf.gz", compress = NULL) {

  # helper function to check if input file is gzipped
  is_gzipped <- function(file) {
    con <- file(file, "rb")
    magic_number <- readBin(con, "raw", n = 2)
    close(con)
    return(identical(magic_number, as.raw(c(0x1f, 0x8b))))
  }
  
  # set compress based on whether the input file is compressed if compress=NULL
  if (is.null(compress)) {
    compress <- is_gzipped(input_vcf)
  }
  
  # read vcf file
  vcf <- read.vcfR(input_vcf)
  
  # check if AA INFO field containing ancestral allele information exists
  if (!any(grepl("AA=", vcf@fix[,"INFO"]))) {
    stop("The AA INFO field does not exist. Cannot polarise vcf file.")
  }

  # extract GT and AA elements
  vcf_GT <- extract.gt(vcf, element = "GT")
  vcf_info <- extract.info(vcf, element = "AA")
  
  # determine which FORMAT elements are present
  format_elements <- strsplit(vcf@gt[1, "FORMAT"], ":")[[1]]
  format_data <- lapply(format_elements, function(element) {
    extract.gt(vcf, element = element)
  })
  names(format_data) <- format_elements
  
  # helper function to swap REF and ALT alleles and adjust GT values
  swap_alleles <- function(ref, alt, gt) {
    new_ref <- alt
    new_alt <- ref
    
    # handle missing genotypes
    if (is.na(gt)) {
      return(list(new_ref, new_alt, gt))
    }
    
    # swap alleles in GT field
    new_gt <- switch(gt,
                     "0/1" = "1/0",
                     "1/0" = "0/1",
                     "0/0" = "1/1",
                     "1/1" = "0/0",
                     "0|1" = "1|0",
                     "1|0" = "0|1",
                     "0|0" = "1|1",
                     "1|1" = "0|0",
                     gt)
    
    return(list(new_ref, new_alt, new_gt))
  }
  
  # iterate over each variant and adjust REF, ALT, and GT
  for (i in 1:nrow(vcf@fix)) {
    aa <- vcf_info[i]
    
    if (!is.na(aa) && aa != vcf@fix[i, "REF"]) {
      result <- swap_alleles(vcf@fix[i, "REF"], vcf@fix[i, "ALT"], vcf_GT[i, 1])
      vcf@fix[i, "REF"] <- result[[1]]
      vcf@fix[i, "ALT"] <- result[[2]]
      vcf_GT[i, 1] <- result[[3]]
    }
  }
  
  # update vcf object with new genotypes
  for (j in 1:ncol(vcf_GT)) {
    for (i in 1:nrow(vcf_GT)) {
      if (!is.na(vcf_info[i]) && vcf_info[i] != vcf@fix[i, "REF"]) {
        gt_field <- vcf_GT[i, j]
        result <- swap_alleles(vcf@fix[i, "REF"], vcf@fix[i, "ALT"], gt_field)
        vcf_GT[i, j] <- result[[3]]
      }
    }
  }
  
  # reassemble FORMAT fields
  new_format_data <- matrix(nrow = nrow(vcf_GT), ncol = ncol(vcf_GT))
  for (i in 1:nrow(vcf_GT)) {
    for (j in 1:ncol(vcf_GT)) {
      gt <- ifelse(!is.na(vcf_GT[i, j]), vcf_GT[i, j], "./.")
      other_fields <- sapply(format_elements[-1], function(element) {
        ifelse(!is.na(format_data[[element]][i, j]), format_data[[element]][i, j], ".")
      })
      new_format_data[i, j] <- paste(c(gt, other_fields), collapse = ":")
    }
  }
  
  # get sample names
  sample_names <- colnames(vcf@gt)[-1] # exclude FORMAT column
  
  # generate new genotype matrix
  new_vcf_gt <- cbind(FORMAT = vcf@gt[, "FORMAT"], new_format_data)
  colnames(new_vcf_gt) <- c("FORMAT", sample_names)
  
  # update vcf@gt
  vcf@gt <- new_vcf_gt
  
  # update vcf@meta to add note that melfuR::polarise_ancestral_vcf was used
  vcf@meta <- c(vcf@meta,
                "##melfuR::polarise_ancestral_vcf, reorder REF, ALT and GT fields based on REF=ancestral allele as recorded in INFO=ID=AA")
  format_meta <- sapply(format_elements, function(element) {
    switch(element,
           "GT" = "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Polarised Genotype\">",
           "DP" = "##FORMAT=<ID=DP,Number=1,Type=Integer,Description=\"Read Depth\">",
           "GL" = "##FORMAT=<ID=GL,Number=G,Type=Float,Description=\"Genotype Likelihoods\">",
           "PL" = "##FORMAT=<ID=PL,Number=G,Type=Integer,Description=\"Phred-scaled Likelihoods\">",
           "GP" = "##FORMAT=<ID=GP,Number=G,Type=Float,Description=\"Genotype Posterior Probabilities\">",
           paste("##FORMAT=<ID=", element, ",Number=.,Type=.,Description=\".\">", sep = "")
    )
  })
  vcf@meta <- c(vcf@meta, format_meta)
  
  # ensure output file has .gz extension if compress is TRUE even if supplied output file name does not end with .gz
  temp_output_vcf <- output_vcf
  if (compress && !grepl("\\.gz$", output_vcf)) {
    temp_output_vcf <- paste0(output_vcf, ".gz")
  } else if (!compress && grepl("\\.gz$", output_vcf)) {
    temp_output_vcf <- output_vcf
  } else if (!compress && !grepl("\\.gz$", output_vcf)) {
    temp_output_vcf <- paste0(output_vcf, ".gz")
  }
  
  # write modified vcf to a new file
  write.vcf(vcf, file = temp_output_vcf)
  
  # unzip the file if compression not wanted
  if (!compress) {
    decompressed_file <- sub("\\.gz$", "", temp_output_vcf)
    R.utils::gunzip(temp_output_vcf, destname = decompressed_file, overwrite = TRUE)
    output_vcf <- decompressed_file
  }
  
  # return modified vcfR object to the environment, object name = output filename without suffix
  output_name <- paste0(sub("\\.vcf(\\.gz)?$", "", basename(output_vcf)), "_vcf")
  assign(output_name, vcf, envir = .GlobalEnv)
}

