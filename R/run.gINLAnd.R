#' Run gINLAnd genotype-environment association (GEA) analysis
#'
#' This function tests for GEAs using SNP data in a Genepop format
#' @param x A Genepop file
#' @keywords gINLAnd
#' @export
#' @examples
#'  data(CATCOLDQRAIN)
#'  data(Mf_mdsXY)
#'  WD <- system.file("extdata", "rainbow.gen", package = "melfuR")
#'  run.gINLAnd("WD", "res.CATCOLDQRAIN", 10, Mf_mdsXY, CATCOLDQRAIN)

run.gINLAnd <- function(input.file, res.file, cutoff, coord, env.var){

  perl <- Sys.which("perl")

  if (perl == "") {
    stop(
      "Cannot find 'perl'. run.gINLAnd requires perl to be installed and on the PATH.",
      call. = FALSE
    )
  }

  # prepare inputs for gINLAnd

  env.var <- env.var
  coord <- coord
  #genepop <- paste(input.dir, list.files(pattern="*.gen"))
  genepop <- input.file

  cmd <- paste("perl", system.file("perl", "gINLAnd_input.pl", package = "melfuR"), genepop)
  system(cmd)
  allele.counts <- read.table(list.files(pattern="*Allele_Counts", full.names=TRUE), header=TRUE)
  pop.size <- read.table(list.files(pattern="*Population_Counts", full.names=TRUE), header=TRUE)


  # run gINLAnd
   gINLAnd.main <- function(){

    # look for previously estimated tau and kappa and load, otherwise estimate from data
    ## estimating from data will take a long time.

    if(!exists("tau")) {
      if(!file.exists("tau")) {
        subs = sample(x=1:ncol(allele.counts),size=10,replace=FALSE)
        res.infcov <- gINLAnd::gINLAnd.inference(s=coord,sphere=FALSE, z=allele.counts,codominant=TRUE,
                                        pop.size=pop.size, inference.cov=TRUE, subset.loci.inf.cov = subs)
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
                                     z=allele.counts,codominant=TRUE,
                                     pop.size=pop.size,y=env.var[,1],
                                     tau=tau,kappa=kappa,
                                     mlik=TRUE,
                                     models.mlik=list("z~1"=FALSE,"z~1+x"=TRUE,"z~1+y"=FALSE,"z~1+x+y"=TRUE),
                                     inference.cov=FALSE)



    ## plot log bayes factors for each loci highlighting those with logBF > your cutoff ##
    fig <- plot.default(res.bylocus$logBF, col=ifelse(res.bylocus$logBF > cutoff, "red", "black"), main=res.file)

    ## print to file
    pdf(file=paste0(res.file,".pdf"), width=6, height=6)
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
    xlsx::write.xlsx(details, file=paste0(res.file,".xlsx"), sheetName="Details")
    xlsx::write.xlsx(sel, file=paste0(res.file,".xlsx"), sheetName="Candidate loci", append=TRUE)
    xlsx::write.xlsx(order, file=paste0(res.file,".xlsx"), sheetName="Results", append=TRUE)

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



