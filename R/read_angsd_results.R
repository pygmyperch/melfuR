#' Read ANGSD D-statistic results
#'
#' This function imports a tab-delimited ANGSD D-statistics result file.
#' 
#' Rscript jackKnife.R file= result.abbababa indNames= inds.txt outfile= example_ANGSD_Dstat_res.txt
#' For some reason the result file often contains a column at the end that contains only NA values.
#'
#' @param filepath Path to the ANGSD result file (e.g., "ind_eachamD.res.txt").
#'
#' @return A data frame with parsed results and any all-NA columns removed.
#'
#' @examples
#' # Load example file from package extdata
#' example_file <- system.file("extdata", "example_ANGSD_Dstat_res.txt", package = "melfuR")
#' res <- read_angsd_results(example_file)
#' head(res)
#'
#' @export
read_angsd_results <- function(filepath) {
  res <- read.delim(filepath, header = TRUE, sep = "\t", fill = TRUE, strip.white = TRUE)
  all_na_cols <- sapply(res, function(col) all(is.na(col)))
  if (any(all_na_cols)) {
    message("Dropping column(s) with all NA values: ", paste(names(res)[all_na_cols], collapse = ", "))
    res <- res[, !all_na_cols, drop = FALSE]
  }
  return(res)
}
