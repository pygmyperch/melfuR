#' generate a kml file from dataframe of coordinates
#'
#' @param sampleXY -  a data frame with three columns: ID, X and Y (in decimal degrees)
#' @return a .kml file of that you can view in google earth
#' @author Chris Brauer
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load example coordinate file
#'  data(Tursiops.XY)
#'
#'  # run analysis
#'  xy2kml(Tursiops.XY)



xy2kml <- function (sampleXY) {

  # get input object name to use as name of output file
  nm <- deparse(substitute(sampleXY))

  cs <- "+init=epsg:4326"

  # define SpatialPointsDataFrame for raw sampleXYs
  spXY <- as.data.frame(sampleXY)
  coordinates(spXY)<- c("X", "Y")
  proj4string(spXY)<- cs
  spXY <- spTransform(spXY, CRS= cs)


  kmlPoints(sampleXY, kmlfile = paste0(as.character(nm),"_XY.kml"), name = sampleXY$ID,
            icon = "http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png")


}

