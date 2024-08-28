#' Calculate basic genetic diversity statistics from genind or genlight objects
#'
#' This function calculates basic genetic diversity statistics from a genind or genlight object,
#' or from a list of such objects.
#'
#' @param input.file A genind object, a genlight object, or a list of such objects
#'
#' @return For a single input object, returns a data frame of diversity statistics.
#'         For a list of input objects, returns a list of data frames, one for each input object.
#'         In both cases, the function also writes the results to CSV files.
#'
#' @details The function calculates the following statistics for each population:
#'          N (sample size), Na (allelic richness), %poly (percentage of polymorphic loci),
#'          Ho (observed heterozygosity), He (expected heterozygosity),
#'          Fis (inbreeding coefficient), and 95% confidence intervals for Fis (LCI and UCI).
#'
#'          When processing a list of objects, the function uses the names of the list elements
#'          (or generates names if they're missing) to name the output files and list elements.
#'          Any "." in the input object names are replaced with underscores in the output file names.
#'
#' @examples
#' ## Set directory for results to be written
#' setwd("path/to/working/directory")
#'
#' ## Load the example data
#' data(rainbow.genind)
#'
#' # Calculate diversity stats for a single genind object
#' res_single <- diversity_stats(rainbow.genind)
#'
#' # Calculate diversity stats for a list of genind objects
#' genind_list <- list(pop1 = rainbow.genind, pop2 = rainbow.genind)
#' res_list <- diversity_stats(genind_list)
#'
#' @export
#' @importFrom hierfstat genind2hierfstat allelic.richness basic.stats boot.ppfis
#' @importFrom adegenet seppop isPoly nAll
#' @importFrom dartR.base gl2gi
diversity_stats <- function(input.file) {
  
  process_single_input <- function(single_input) {
    # Check if input is a genlight object and convert to genind if necessary
    if (inherits(single_input, "genlight")) {
      genind <- dartR.base::gl2gi(single_input)
    } else if (inherits(single_input, "genind")) {
      genind <- single_input
    } else {
      stop("Input file must be a genind or genlight object.")
    }
    
    f.stat <- genind2hierfstat(genind)
    # get number of inds per pop
    pop.numbers <- as.data.frame(table(f.stat$pop))
    pop.numbers <- as.data.frame(pop.numbers$Freq)
    
    # estimate allelic richness
    Na <- allelic.richness(f.stat)
    popNa <- as.data.frame(colMeans(Na$Ar, na.rm = TRUE))
    
    # estimate Ho, He, Fis
    stats <- basic.stats(f.stat, digits = 4)
    Ho <- as.data.frame(stats$Ho)
    popHo <- as.data.frame(colMeans(Ho, na.rm = TRUE))
    He <- as.data.frame(stats$Hs)
    popHe <- as.data.frame(colMeans(He, na.rm = TRUE))
    popFis <- as.data.frame(1 - colSums(stats$Ho, na.rm = TRUE) / colSums(stats$Hs, na.rm = TRUE))
    
    # estimate 95%CIs around the Fis values
    geno.fis <- boot.ppfis(f.stat)
    FisCI <- as.data.frame(geno.fis$fis.ci)
    
    # split input into list of data.frames by pop factor and calc %poly loci
    poly.lst <- seppop(genind)
    percent.poly <- lapply(poly.lst, function(x) {
      (sum(isPoly(x))) / (dim(x@tab)[2] / 2 - (length(which(nAll(x) == 0)))) * 100
    })
    percent.poly <- data.frame(sapply(percent.poly, c))
    
    geno.stats <- cbind(pop.numbers, popNa, percent.poly, popHo, popHe[, 1], popFis[, 1], FisCI)
    colnames(geno.stats) <- c("N", "Na", "%poly", "Ho", "He", "Fis", "LCI", "UCI")
    
    return(geno.stats)
  }
  

    # Function to safely get names and replace dots with underscores
    safe_names <- function(x) {
      if (is.null(names(x))) {
        return(paste0("unnamed_", seq_along(x)))
      } else {
        return(gsub("\\.", "_", names(x)))
      }
    }
    
    # Check if the input is a list
    if (is.list(input.file) && !inherits(input.file, c("genind", "genlight"))) {
      # Get the names of the list elements
      input_names <- safe_names(input.file)
      
      # Process each element in the list
      results <- lapply(seq_along(input.file), function(i) {
        current_input <- input.file[[i]]
        current_name <- input_names[i]
        
        if (inherits(current_input, c("genind", "genlight"))) {
          result <- process_single_input(current_input)
          output_file <- paste0(current_name, "_stats.csv")
          write.csv(result, output_file)
          return(result)
        } else {
          stop(paste("Element", i, "in the input list is not a genind or genlight object."))
        }
      })
      names(results) <- input_names
      return(results)
    } 
    # Check if the input is a genind or genlight object
    else if (inherits(input.file, c("genind", "genlight"))) {
      # Capture the name of the input object and replace dots with underscores
      input_name <- gsub("\\.", "_", deparse(substitute(input.file)))
      result <- process_single_input(input.file)
      # Write the output to a CSV file
      output_file <- paste0(input_name, "_stats.csv")
      write.csv(result, output_file)
      return(result)
    } 
    # If the input is neither a list nor a genind/genlight object, return an error
    else {
      stop("Input must be a list of genind/genlight objects, a genind object, or a genlight object.")
    }
}
