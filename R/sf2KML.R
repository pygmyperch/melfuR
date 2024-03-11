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
#' @importFrom sf st_coordinates



sf2KML <- function(sf_object, colour = NULL, outputPath = NULL) {
  
  sf_object_name <- deparse(substitute(sf_object))
  if (is.null(outputPath)) {
    outputPath <- paste0(sf_object_name, ".kml")
  } else {
    # Ensure outputPath incorporates working directory if only a filename is provided
    if (!grepl("/", outputPath)) {
      outputPath <- paste0(getwd(), "/", outputPath)
    }
  }
  # Base URL pattern for the icons
  baseUrl <- "http://maps.google.com/mapfiles/kml/pushpin/"
  
  # List of valid colours
  validColours <- list(
    green = "grn",
    blue = "blue",
    red = "red",
    purple = "purple",
    lightblue = "ltblu",
    pink = "pink",
    white = "wht"
  )
  
  # Determine the icon URL based on the specified colour
  if (!is.null(colour) && colour %in% names(validColours)) {
    colourAbbreviation <- validColours[[colour]]
    iconUrl <- paste0(baseUrl, colourAbbreviation, "-pushpin.png")
  } else {
    # Default icon URL if no valid colour is specified
    iconUrl <- paste0(baseUrl, "ylw-pushpin.png") # Using yellow (ylw) as the default colour
  }
  
  # Initialize the root KML structure with the namespace
  kml <- read_xml('<kml xmlns="http://www.opengis.net/kml/2.2"></kml>')
  
  # Directly add and work with the Document node
  doc <- xml_add_child(kml, 'Document')
  
  # Add a custom icon style if an icon URL is provided
  if (!is.null(iconUrl)) {
    styleNode <- xml_add_child(doc, 'Style', id = 'customIconStyle')
    iconStyleNode <- xml_add_child(styleNode, 'IconStyle')
    iconNode <- xml_add_child(iconStyleNode, 'Icon')
    xml_add_child(iconNode, 'href', iconUrl)
  }
  
  # Iterate over the sf object to add Placemark elements
  n <- nrow(sf_object)
  for (i in seq_len(n)) {
    coords <- st_coordinates(sf_object[i, ])
    ID <- as.character(sf_object$ID[i]) # Convert factor to character if needed
    
    placemark <- xml_add_child(doc, 'Placemark')
    xml_add_child(placemark, 'name', ID)
    
    if (!is.null(iconUrl)) {
      xml_add_child(placemark, 'styleUrl', '#customIconStyle')
    }
    
    point <- xml_add_child(placemark, 'Point')
    xml_add_child(point, 'coordinates', sprintf('%f,%f,0', coords[1, 'X'], coords[1, 'Y']))
  }
  
  # Write the KML to the specified output file
  write_xml(kml, outputPath)
}


