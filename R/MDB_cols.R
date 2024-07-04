#' Custom color palette based on the Murray-Darling Basin landscape
#'
#' Custom color palette based on the natural elements and features
#' of the Murray-Darling Basin landscape.
#'
#' @format A named vector of RGB colors.
#' \describe{
#'   \item{Mudflat}{RGB(190/255, 175/255, 152/255)}
#'   \item{Silt}{RGB(144/255, 128/255, 101/255)}
#'   \item{Murray}{RGB(168/255, 160/255, 100/255)}
#'   \item{Mallee}{RGB(102/255, 101/255, 50/255)}
#'   \item{Eucalyptus}{RGB(94/255, 112/255, 60/255)}
#'   \item{Grassland}{RGB(200/255, 179/255, 59/255)}
#'   \item{Ochre}{RGB(200/255, 112/255, 0/255)}
#'   \item{Sunset}{RGB(200/255, 80/255, 37/255)}
#'   \item{Ironbark}{RGB(167/255, 47/255, 26/255)}
#'   \item{PinkGum}{RGB(222/255, 116/255, 116/255)}
#'   \item{MorningSky}{RGB(199/255, 225/255, 255/255)}
#'   \item{EveningSky}{RGB(149/255, 217/255, 255/255)}
#'   \item{SummerSky}{RGB(76/255, 149/255, 255/255)}
#' }
#' @source Custom defined for the melfuR package
#' @export
MDB_cols <- c(
  "Mudflat" = rgb(190/255, 175/255, 152/255),
  "Silt" = rgb(144/255, 128/255, 101/255),
  "Murray" = rgb(168/255, 160/255, 100/255),
  "Mallee" = rgb(102/255, 101/255, 50/255),
  "Eucalyptus" = rgb(94/255, 112/255, 60/255),
  "Grassland" = rgb(200/255, 179/255, 59/255),
  "Ochre" = rgb(200/255, 112/255, 0/255),
  "Sunset" = rgb(200/255, 80/255, 37/255),
  "Ironbark" = rgb(167/255, 47/255, 26/255),
  "PinkGum" = rgb(222/255, 116/255, 116/255),
  "MorningSky" = rgb(199/255, 225/255, 255/255),
  "EveningSky" = rgb(149/255, 217/255, 255/255),
  "SummerSky" = rgb(76/255, 149/255, 255/255)
)

# Save the data to the package (run once during package development)
# usethis::use_data(MDB_cols, overwrite = TRUE)
