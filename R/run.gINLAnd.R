#' A wrapper function to run gINLAnd genotype-environment association (GEA) analysis using a genind file
#'
#' This function calls gINLAnd to test for GEAs using SNP data in ADEGENET's genind format
#' @param input.file A genind object
#' @param result.file name of your result file
#' @param cutoff logBF threshold for candidate loci (i.e. loci with logBF>cutoff will be considered candidates)
#' @param coord dataframe with x and y spatial coordinates for each population
#' @param env.var value of some environmental variable for each population
#' @references Guillot G, Vitalis R, le Rouzic A, Gautier M (2014) Detecting correlation between allele frequencies
#'                  and environmental variables as a signature of selection. A fast computational approach for
#'                  genome-wide studies. Spatial Statistics 8, 145–155.
#'
#' @keywords gINLAnd
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  ## load the example data
#'  data(rainbow.genind)
#'  data(rainbow.env)
#'
#'  # subset rainbow.genind to 200 loci
#'  gen100 <- rainbow.genind[,1:200]
#'
#'  # get xy coordinates
#'  coords <- rainbow.env[,2:3]
#'
#'  # get environmental variable
#'  tempPC1 <- as.data.frame(rainbow.env[,4])
#'
#'  ## run gINLAnd
#'  run.gINLAnd(gen100, "res.tempPC1", 10, coords, tempPC1)

run.gINLAnd <- function(input.file, result.file, cutoff, coord, env.var){

  # prepare inputs for gINLAnd

  env.var <- env.var
  coord <- coord
  gen_pop <- genind2genpop(input.file)
  allele.tab <- as.data.frame(gen_pop$tab)
  allele1 <- allele.tab[ ,seq(1, ncol(allele.tab), 2)]
  allele2 <- allele.tab[ ,seq(2, ncol(allele.tab), 2)]
  pop_count <- allele1 + allele2
  locus.names <- as.data.frame(input.file@all.names)
  locus.names <- as.character(colnames(locus.names))
  colnames(allele1) <- locus.names
  colnames(pop_count) <- locus.names


  # run gINLAnd
  gINLAnd.main <- function(){

    # look for previously estimated tau and kappa and load, otherwise estimate from data
    ## estimating from data will take a long time.

    if(!exists("tau")) {
      if(!file.exists("tau")) {
        subs = sample(x=1:ncol(allele1),size=100,replace=FALSE)
        res.infcov <- gINLAnd::gINLAnd.inference(s=coord,sphere=FALSE, z=allele1,codominant=TRUE,
                                                 pop.size=pop_count, inference.cov=TRUE, subset.loci.inf.cov = subs)
        tau <- res.infcov$tau
        save(tau, file="tau")
        kappa <- res.infcov$kappa
        save(kappa, file="kappa")

      } else {
        load("tau")
        load("kappa")
      }
    }


    ## run the analysis using all SNPs with parameters estimated above ##
    res.bylocus <- gINLAnd::gINLAnd.inference(s=coord,sphere=FALSE,
                                              z=allele1,codominant=TRUE,
                                              pop.size=pop_count,y=env.var[,1],
                                              tau=tau,kappa=kappa,
                                              mlik=TRUE,
                                              models.mlik=list("z~1"=FALSE,"z~1+x"=TRUE,"z~1+y"=FALSE,"z~1+x+y"=TRUE),
                                              inference.cov=FALSE)



    ## plot log bayes factors for each loci highlighting those with logBF > your cutoff ##
    fig <- plot.default(res.bylocus$logBF, col=ifelse(res.bylocus$logBF > cutoff, "red", "black"), main=result.file)

    ## print to file
    pdf(file=paste0(result.file,".pdf"), width=6, height=6)
    plot.default(res.bylocus$logBF, col=ifelse(res.bylocus$logBF > cutoff, "red", "black"));
    dev.off()


    # list details of run, logBF for each locus, loci above your threshold (putitively under selection),
    # loci in order of highest to lowest support and marginal likelihoods for models
    details <- cbind(coord, env.var)
    logBF <- cbind(as.data.frame(seq(1, as.numeric(length(t(as.data.frame(res.bylocus$logBF)))))),
                   as.data.frame(res.bylocus$logBF), as.data.frame(t(res.bylocus$evidence)[ ,c(2,4)]))
    order <- logBF[with(logBF, order(-res.bylocus$logBF)), ]
    colnames(order)[1:2] <- c("locus","logBF")
    sel <- order[1:as.numeric(length(t(as.data.frame(which(res.bylocus$logBF >cutoff))))), ]
    sel.len <- as.numeric(length(t(as.data.frame(which(res.bylocus$logBF >cutoff)))))

    # write results to .xlsx file
    xlsx::write.xlsx(details, file=paste0(result.file,".xlsx"), sheetName="Details")
    xlsx::write.xlsx(sel, file=paste0(result.file,".xlsx"), sheetName="Candidate loci", append=TRUE)
    xlsx::write.xlsx(order, file=paste0(result.file,".xlsx"), sheetName="Results", append=TRUE)

    if (sel.len ==1) {
      print(paste0("You have 1 candidate locus with a logBF > ", cutoff))
    } else {
      print(paste0("You have ",sel.len, " candidate loci with a logBF > ", cutoff))
    }

    return(fig)
  }

  gINLAnd.main()

  print("Your analysis has finished, have a nice day.")

}
