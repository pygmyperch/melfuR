#' Expand Population-Level Metadata to Individual-Level Data Frame
#'
#' This function expands population-level metadata to an individual-level data frame by merging metadata with individual data from a `genlight` or `genind` object.
#'
#' @param geno A `genlight` or `genind` object containing the genotype data.
#' @param meta.data A data frame containing population-level metadata. This data frame must include a column named `site` that corresponds to the population names in the `genlight` or `genind` object.
#'
#' @details 
#' The function extracts individual sample names and their population assignments from the `genlight` or `genind` object. Population-level metadata such as XY coordinates or environmental data are expanded to individual-level metadata using a left join on the `site` column.
#' The resulting individual-level data frame is saved as a CSV file named "individual_env_data.csv".
#'
#' @return A data frame containing individual-level data with expanded metadata.
#'
#' @importFrom adegenet indNames pop
#' @importFrom dplyr left_join
#' @importFrom dartR.base gi2gl
#'
#' @examples
#' \dontrun{
#'
#' # Example genlight object
#' library(adegenet)
#' data(nancycats)
#' genlight_obj <- gi2gl(nancycats[pop(nancycats) %in% levels(pop(nancycats))[1:3]])
#'
#' # Example metadata
#' meta_data <- data.frame(
#'   site = c("P01", "P02", "P03"),
#'   env_var1 = c(10.5, 20.3, 15.2),
#'   env_var2 = c(100, 200, 150),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Expand population-level metadata to individual-level
#' individual_data <- expand_pop2ind(genlight_obj, meta_data)
#'
#' # Example genind object
#' genind_obj <- nancycats[pop(nancycats) %in% levels(pop(nancycats))[1:3]]
#' individual_data_genind <- expand_pop2ind(genind_obj, meta_data)
#' }
#' @export
expand_pop2ind <- function(geno, meta.data) {
  # Check if genlight or genind object is supplied
  input.class <- class(geno)[1]
  
  # Extract individual names and population assignments
  if (input.class == "genlight" || input.class == "genind") {
    ind_names <- indNames(geno)
    pops <- pop(geno)
  } else {
    stop("The input object must be of class 'genlight' or 'genind'.")
  }
  
  # Create a data frame with individual names and their population
  ind_df <- data.frame(ind.names = ind_names, site = as.character(pops), stringsAsFactors = FALSE)
  meta.data$site <- as.character(meta.data$site)
  
  # Use left_join to merge
  expanded_df <- ind_df %>%
    left_join(meta.data, by = "site")
  
  write.csv(expanded_df, "individual_env_data.csv", quote = FALSE, row.names = FALSE)
  return(expanded_df)
}
