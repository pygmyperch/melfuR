#' Parse and clean-up INFO Fields in a VCF File
#'
#' This function reads a VCF file, processes the INFO fields to remove any fields that are not consistently present 
#' across all records, and outputs a cleaned VCF file with "_clean" appended to the input file name before the suffix.
#'
#' @param vcffile A character string specifying the path to the input VCF file.
#'
#' @return Writes a cleaned VCF file to the specified or default path.
#'
#' @examples
#' # Assume you have a VCF file "my_input.vcf" in your working directory
#' clean_vcf_INFO("my_input.vcf")
#'
#' @export
#' @importFrom vcfR read.vcfR write.vcf getINFO
#' @importFrom stringr str_split
clean_vcf_INFO <- function(vcffile) {
  # Check for required packages
  required_packages <- c("vcfR", "stringr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package ", pkg, " needed for this function to work. Please install it.\n")
    }
  }
  
  # Generate the output file name by appending "_clean" before the suffix
  outputfile <- sub("(.*)\\.(vcf|VCF)$", "\\1_clean.\\2", vcffile)
  
  # Read the VCF file
  vcf <- vcfR::read.vcfR(file = vcffile)
  
  # Extract INFO fields
  info_tmp_2 <- vcfR::getINFO(vcf)
  
  # Parse the INFO fields into key-value pairs
  split_info <- lapply(info_tmp_2, function(x) {
    pairs <- unlist(stringr::str_split(x, ";"))
    key_value <- stringr::str_split(pairs, "=")
    keys <- sapply(key_value, `[`, 1)
    values <- sapply(key_value, `[`, 2)
    setNames(values, keys)
  })
  
  # Determine all possible keys
  all_keys <- unique(unlist(lapply(split_info, names)))
  
  # Identify keys missing in any record
  missing_keys <- all_keys[sapply(all_keys, function(k) {
    any(sapply(split_info, function(x) !(k %in% names(x))))
  })]
  
  # Remove these keys from each record
  cleaned_info <- lapply(split_info, function(x) {
    x[setdiff(names(x), missing_keys)]
  })
  
  # Reconstruct the INFO fields
  info_tmp_2_cleaned <- sapply(cleaned_info, function(x) {
    paste(names(x), x, sep = "=", collapse = ";")
  })
  
  # Replace the original INFO column with the cleaned one
  vcf@fix[, 8] <- info_tmp_2_cleaned
  
  # Write the cleaned VCF to the output file
  vcfR::write.vcf(vcf, file = outputfile)
  
  message("Cleaned VCF file written to: ", outputfile)
}
