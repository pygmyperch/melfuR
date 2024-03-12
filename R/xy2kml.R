#' generate a kml file from dataframe of coordinates
#'
#' @param sampleXY -  a data frame with three columns: ID, X and Y (in decimal degrees)
#' @return a .kml file of that you can view in google earth
#' @author Chris Brauer
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load example coordinate file
#'  data(DelphinusXY)
#'
#'  # run analysis
#'  xy2kml(DelphinusXY)
#'
#' @export
#' @importFrom sf st_coordinates



xy2kml <- function (sampleXY) {

  nm <- deparse(substitute(sampleXY))
  xy <- as.data.frame(sampleXY)
  xy <- st_as_sf(xy, coords = c("X", "Y"), crs = 4326)
  

  sf2KML(spXY, colour = "red", outputPath = paste0(as.character(nm), "_XY.kml"))
  


}

