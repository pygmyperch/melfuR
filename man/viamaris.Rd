% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viamaris.R
\name{viamaris}
\alias{viamaris}
\title{calculate pairwise oceanic distances among sampling locations}
\usage{
viamaris(sampleXY, extent.buffer = NULL)
}
\arguments{
\item{sampleXY}{-  a data frame with three columns: ID, X and Y (in decimal degrees)}

\item{extent.buffer}{(optional) number of degrees to extend raster around your samples, default = 0.5
(you may need to increase this to allow navigation around land masses that extend beyond the extent of your samples)}
}
\value{
distmat               a list object containing the function call, distance matrix, and spatial objects you can use for plotting (original coordinates, adjusted coordinates (if used) and study area raster)

distmat.csv           a pairwise matrix of distances (Km) between sampling sites

XY.kml                a .kml file of the original coordinates that you can view in google earth

adjustedXY.kml        a .kml file of the adjusted coordinates that you can view in google earth (if necessary)
}
\description{
calculate pairwise oceanic distances among sampling locations
}
\examples{
 ## set directory for results to be written
 setwd("path/to/working/directory")

 # load example coordinate file
 data(DelphinusXY)

 # run analysis
 dist.mat <- viamaris(DelphinusXY, extent.buffer = 5)
}
\author{
Chris Brauer
}
