#' Generate KML Files from simple SF Geometry type: POINT objects with Custom Icon colours
#'
#' This function creates KML files from `sf` objects. Users can specify colours for the placemarks, 
#' which are then mapped to predefined URLs for icons in those colours. If no colour is specified, 
#' a default icon is used.
#'
#' @param sf_object An `sf` object containing the spatial data to be plotted. 
#'                  Must have an `ID` column for placemark labels and geometry for coordinates.
#'                  Must be of Geometry type: POINT.
#' @param colour Optional; a character string specifying the colour of the placemark pins. 
#'              Valid options are "green", "blue", "red", "purple", "lightblue", "pink", and "white". 
#'              Defaults to NULL, which uses the default yellow colour.
#' @param outputPath Optional; the file path where the KML file will be saved. 
#'                   If not specified, the file is saved in the current working directory with a 
#'                   name based on the `sf` object's name.
#'
#' @return Writes a KML file to the specified or default path.
#'
#' @examples
#' # Start with a simple data.frame with three columns: "ID", "X", and "Y"
#' Flinders_University <- data.frame(
#'   ID = c("MELFU", "Tavern", "Animal House", "Aquaculture compound", "Lake", "$1,360 car park"),
#'   X = c(138.570071, 138.571627, 138.569855, 138.569586, 138.572218, 138.569437), 
#'   Y = c(-35.026967, -35.026029, -35.028127, -35.027034, -35.026907, -35.029019)
#' )
#'
#' # Convert the data frame to an sf object
#' library(sf)
#' Flinders_sf <- st_as_sf(Flinders_University, coords = c("X", "Y"), crs = 4326)
#'
#' # Generate the KML file
#' sf2KML(Flinders_sf, colour = "red", outputPath = "Flinders_University.kml")
#'
#' @export
#' @importFrom xml2 read_xml write_xml xml_add_child
#' @importFrom sf st_coordinates st_as_sf
#' @importFrom geosphere distm



sf2KML <- function(sf_object, colour = NULL, outputPath = NULL) {
  sf_object_name <- deparse(substitute(sf_object))
  if (is.null(outputPath)) {
    outputPath <- paste0(sf_object_name, ".kml")
  } else {
    if (!grepl("/", outputPath)) {
      outputPath <- paste0(getwd(), "/", outputPath)
    }
  }
  
  baseUrl <- "http://maps.google.com/mapfiles/kml/pushpin/"
  validColours <- list(
    green = "grn", blue = "blue", red = "red", purple = "purple",
    lightblue = "ltblu", pink = "pink", white = "wht"
  )
  
  iconUrl <- paste0(baseUrl, ifelse(colour %in% names(validColours), validColours[[colour]], "ylw"), "-pushpin.png")
  
  kml <- read_xml('<kml xmlns="http://www.opengis.net/kml/2.2"></kml>')
  doc <- xml_add_child(kml, "Document")
  
  # Calculate the centroid and the optimal viewing range
  coords <- st_coordinates(st_centroid(st_union(sf_object)))
  centroid_x <- coords[1, "X"]
  centroid_y <- coords[1, "Y"]
  
  # set zoom level for opening kml using geodesic distance for the furthest point pair
  max_distance <- max(distm(st_coordinates(sf_object)))
  #view_range <- max_distance #/ 2  # adjust to half of max distance
  
  lookAt <- xml_add_child(doc, "LookAt")
  xml_add_child(lookAt, "longitude", as.character(centroid_x))
  xml_add_child(lookAt, "latitude", as.character(centroid_y))
  xml_add_child(lookAt, "altitude", "0")
  xml_add_child(lookAt, "range", as.character(max_distance))
  xml_add_child(lookAt, "tilt", "0")
  xml_add_child(lookAt, "heading", "0")
  
  # Custom icon style
  if (!is.null(iconUrl)) {
    styleNode <- xml_add_child(doc, 'Style', id = 'customIconStyle')
    iconStyleNode <- xml_add_child(styleNode, 'IconStyle')
    iconNode <- xml_add_child(iconStyleNode, 'Icon')
    xml_add_child(iconNode, 'href', iconUrl)
  }
  
  # Add placemarks for each point
  n <- nrow(sf_object)
  for (i in seq_len(n)) {
    coords <- st_coordinates(sf_object[i, ])
    ID <- as.character(sf_object$ID[i])
    
    placemark <- xml_add_child(doc, 'Placemark')
    xml_add_child(placemark, 'name', ID)
    if (!is.null(iconUrl)) {
      xml_add_child(placemark, 'styleUrl', '#customIconStyle')
    }
    
    point <- xml_add_child(placemark, 'Point')
    xml_add_child(point, 'coordinates', sprintf('%f,%f,0', coords[1, 'X'], coords[1, 'Y']))
  }
  
  # Save the KML file
  write_xml(kml, outputPath)
}

