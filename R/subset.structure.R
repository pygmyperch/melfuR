#' This function subsets a structure file based on a list of loci to retain
#'
#' @param input.file A character string giving the path to the file to subset
#' @param result.file A character string name of your result file
#' @param loci.to.keep A vector of character strings naming the loci to retain (based on the locus names in the original file)
#' @param popnumbers an optional logical stating whether to convert the population column from character strings to numbers (default = FALSE)
#' @keywords structure
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'
#'  generate list of loci to keep
#'  snplist <- as.data.frame(c("SNP_1", "SNP_45", "SNP_96"))
#'
#'  # subset file to the three loci, keeping original population codes
#'  subset.structure("Mfsub500.stru", "new_file", snplist)
#'
#'  # subset file to three loci, replacing original population codes with numbers
#'  subset.structure("Mfsub500.stru", "new_file", snplist, popnumbers = TRUE)



subset.structure <- function(input.file, result.file, loci.to.keep, popnumbers=FALSE){

  struc_file <- read.table(input.file, skip = 1)
  locus_names <- read.table(input.file, nrows=1)

  if (!popnumbers) {
    ind_pop <- struc_file[,1:2]

  } else {
    if (is.numeric(struc_file[,2])) {
      ind_pop <- struc_file[,1:2]
    } else {
      ind <- as.data.frame(struc_file[,1])
      popvec <- struc_file[,2]
      popvec <- as.data.frame(as.character(1:length(unique(popvec)))[ match(popvec, unique(popvec)) ])
      ind_pop <- cbind(ind, popvec)
    }

  }

  # grab individual and pop information and remove from data
  struc_file <- struc_file[,-(1:2),drop=TRUE]
  colnames(struc_file) <- t(locus_names[1,])

  # subset data based on list of loci
  struc_mat <- as.matrix(t(struc_file))
  include <- loci.to.keep[,1]
  new_gen <- struc_mat[rownames(struc_mat) %in% include,]

  new_gen <- t(new_gen)
  new_struc <- cbind(ind_pop, new_gen)
  colnames(new_struc) <- c("","", t(loci.to.keep)[1,])
  write.table(new_struc, file=paste0(result.file,".stru"), quote = FALSE, row.names = FALSE, sep = "\t")

}





subset.structure("/Users/chrisbrauer/GoogleDrive/Rpackage_files/local_files/Mfsub500.stru", "new_file", snplist, popnumbers = TRUE)





