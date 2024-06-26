#' calculate pairwise oceanic distances among sampling locations
#'
#'
#' @param sampleXY -  a data frame with three columns: ID, X and Y (in decimal degrees)
#' @param extent.buffer (optional) number of degrees to extend raster around your samples, default = 0.5
#'                      (you may need to increase this to allow navigation around land masses that extend beyond the extent of your samples)
#'
#' @return distmat               a list object containing the function call, distance matrix, and spatial objects you can use for plotting (original coordinates, adjusted coordinates (if used) and study area raster)
#' @return distmat.csv           a pairwise matrix of distances (Km) between sampling sites
#' @return XY.kml                a .kml file of the original coordinates that you can view in google earth
#' @return adjustedXY.kml        a .kml file of the adjusted coordinates that you can view in google earth (if necessary)
#'
#'
#' @author Chris Brauer
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load example coordinate file
#'  data(DelphinusXY)
#'
#'  # run analysis
#'  dist.mat <- viamaris(DelphinusXY, extent.buffer = 5)
#' @export
#' @importFrom raster plot extent raster cellFromXY
#' @importFrom terra extract xyFromCell rasterize
#' @importFrom sf st_as_sf st_wrap_dateline st_coordinates st_transform st_bbox
#' @importFrom gdistance geoCorrection transition costDistance


viamaris <- function (sampleXY, extent.buffer = NULL)
{
  inds <- as.data.frame(sampleXY$ID)
  XY <- as.matrix(cbind(sampleXY$X, sampleXY$Y))
  if (is.null(extent.buffer)) {
    extent.buffer <- 0.5}

  resolution <- 1500

  nm <- deparse(substitute(sampleXY))

  mapfile <- melfuR:::wldmap
  mapfile.sf <- as(mapfile, "sf")
  mapfile.sf <- st_transform(mapfile.sf, 4326)
  #wkt <- comment(raster::crs(mapfile.sf))

  # rawXY <- as.data.frame(sampleXY)
  # coordinates(rawXY) <- c("X", "Y")
  # proj4string(rawXY) <- wkt
  rawXY <- st_as_sf(sampleXY, coords = c("X", "Y"), crs = 4326)
  #rawXY <- as(rawXY, "Spatial")
  nearestWater <- function(points, raster, max_distance) {
    nearest <- function(lis, raster) {
      neighbours <- matrix(lis[[1]], ncol = 2)
      point <- lis[[2]]
      water <- !is.na(neighbours[, 2])
      
      # Check if the point itself is on water first
      original_cell <- cellFromXY(raster, matrix(point, nrow = 1))
      if (!is.na(raster[original_cell])) {
        return(point)  # Return the original point if it's on water
      }
      
      if (!any(water)) {
        return(c(NA, NA))
      } else {
        coords <- xyFromCell(raster, neighbours[water, 1])
        if (nrow(coords) == 1) {
          return(coords[1, ])
        }
        dists <- sqrt((coords[, 1] - point[1])^2 + (coords[, 2] - point[2])^2)
        return(coords[which.min(dists), ])
      }
    }
    neighbour_list <- extract(raster, points, buffer = max_distance, cellnumbers = TRUE)
    neighbour_list <- lapply(1:nrow(points), function(i) {
      list(neighbours = neighbour_list[[i]], point = as.numeric(points[i, ]))
    })
    return(t(sapply(neighbour_list, nearest, raster)))
  }
  
  if (any(sampleXY$X < 0) && any(sampleXY$X > 0)) {
    XY360 <- as.matrix(cbind(sampleXY$X, sampleXY$Y))
    X360 <- as.matrix(ifelse(sampleXY$X < 0, (360 + sampleXY$X[sampleXY$X <
                                                                 0]), sampleXY$X))
    XY360 <- as.data.frame(cbind(inds, X360[, 1], XY360[,
                                                        2]))
    colnames(XY360) <- c("ID", "X", "Y")
    # coordinates(XY360) <- c("X", "Y")
    # proj4string(XY360) <- wkt
    XY360 <- st_as_sf(XY360, coords = c("X", "Y"), crs = 4326)
    #XY360 <- as(XY360, "Spatial")

    sp.inds <- XY360
    cat("\npreparing raster\n")
    minX <- st_bbox(XY360)[1] - extent.buffer
    maxX <- st_bbox(XY360)[3] + extent.buffer
    minY <- st_bbox(XY360)[2] - extent.buffer
    maxY <- st_bbox(XY360)[4] + extent.buffer
    ras.extent <- extent(as(extent(minX, maxX, minY, maxY),
                            "SpatialPolygons"))
    init.ras <- raster(nrow = resolution, ncol = resolution,
                       ext = ras.extent, crs=4326)
    newmapfile <- st_wrap_dateline(mapfile.sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)
    main.ras <- rasterize(newmapfile, init.ras)


  } else {
    # coordinates(sampleXY) <- c("X", "Y")
    # proj4string(sampleXY) <- wkt
    sp.inds <- rawXY
    cat("\npreparing raster\n")
    minX <- st_bbox(rawXY)[1] - extent.buffer
    maxX <- st_bbox(rawXY)[3] + extent.buffer
    minY <- st_bbox(rawXY)[2] - extent.buffer
    maxY <- st_bbox(rawXY)[4] + extent.buffer
    ras.extent <- extent(as(extent(minX, maxX, minY, maxY),
                            "SpatialPolygons"))
    init.ras <- raster(nrow = resolution, ncol = resolution,
                       ext = ras.extent, crs=4326)
    main.ras <- rasterize(mapfile, init.ras)
  }
  main.ras <- is.na(main.ras)
  main.ras[main.ras == 0] <- NA
  plot(main.ras, breaks = c(0, 1), col = "blue", legend = FALSE)
  plot(sp.inds, col = "red", cex = 1, add = T)
  cat("\nanalysing raster\n")
  tr <- geoCorrection(transition(main.ras, function(x) 1/mean(x),
                                 8), scl = FALSE)
  if (any(is.na(as.data.frame(extract(main.ras, as(sp.inds, "Spatial")))))) {
    cat("\nsome samples are on dry land!\n")
    cat("\ndon't worry, this may just be an artefact of the raster resolution\n")
    cat("...adjusting coordinates to nearest water\n\n")
    search.radius <- 0
    repeat {
      search.radius <- search.radius + 1000
      cat(paste0("optimising search radius: ", search.radius,
                 " metres\n"))
      wetXY <- nearestWater((as.matrix(st_coordinates(sp.inds))),
                            main.ras, search.radius)
      if (search.radius > 10000) {
        cat("\n\ncannot find water nearby, please check your coordinates\n\n")
        break
      }
      if (!any(is.na(wetXY))) {
        cat("\nfound it!\n")
        cat("\n...calculating distance matrix\n")
        break
      }
    }
    IND.wetXY.coord <- cbind(inds, wetXY)
    colnames(IND.wetXY.coord) <- c("ID", "X", "Y")
    # coordinates(IND.wetXY.coord) <- c("x", "y")
    # proj4string(IND.wetXY.coord) <- wkt
    IND.wetXY.coord <- st_as_sf(IND.wetXY.coord, coords = c("X", "Y"), crs = 4326)
    #IND.wetXY.coord <- as(IND.wetXY.coord, "Spatial")

    wet.sp.inds <- IND.wetXY.coord
    adj.XY <- as.matrix(st_coordinates(wet.sp.inds))
    plot(wet.sp.inds, col = "limegreen", cex = 1, pch = 16,
                 add = T)
    cost <- as.matrix(costDistance(tr, as(wet.sp.inds, "Spatial"))/1000)
    if (any(adj.XY[, 1] > 180)) {
      adj.wet.XYkml <- adj.XY
      adj.wet.Xkml <- as.matrix(ifelse(adj.XY[, 1] > 180,
                                       (-360 + adj.XY[, 1][adj.XY[, 1] > 180]), adj.XY[,
                                                                                       1]))
      adj.wet.XYkml <- as.data.frame(cbind(inds, adj.wet.Xkml[,
                                                              1], adj.wet.XYkml[, 2]))
      colnames(adj.wet.XYkml) <- c("ID", "X", "Y")
      adj.wet.XYkml <- st_as_sf(adj.wet.XYkml, coords = c("X", "Y"), crs = 4326)
      #coordinates(adj.wet.XYkml) <- c("X", "Y")
    }
  }  else {
    cat("\n...calculating distance matrix\n")
    wet.sp.inds <- NULL
    adj.XY <- NULL
    XY <- as.matrix(st_coordinates(sp.inds))
    plot(sp.inds, col = "limegreen", cex = 1, pch = 16, add = T)
    cost <- as.matrix(costDistance(tr, as(sp.inds, "Spatial"))/1000)
  }
  colnames(cost) <- t(inds)
  rownames(cost) <- inds[, 1]
  cat("\nYour results are ready, have a nice day!\n")
  result <- list()
  result$call <- match.call()
  result$dist.matrix <- cost
  result$raster <- main.ras
  result$XY.sf <- sp.inds
  result$XY <- XY
  result$adjXY.sf <- wet.sp.inds
  #result$adj.XY <- adj.XY
  if (exists("adj.wet.XYkml")) {
    result$adj.XY360 <- adj.XY
    result$adj.XY180 <- as.matrix(st_coordinates(adj.wet.XYkml))
  } else {
    if (exists("adj.XY") && !is.null(adj.XY)) {
      result$adjXY <- adj.XY
      }
  }
  res.file <- list()
  res.file$call <- match.call()
  res.file$raster <- main.ras
  res.file$XY.sf <- summary(sp.inds)
  res.file$XY <- XY
  res.file$adjXY.sf <- summary(wet.sp.inds)
  if (exists("adj.wet.XYkml")) {
    # if +ve and -ve latitude in original data
    res.file$adjXY <- as.matrix(st_coordinates(adj.wet.XYkml))
  }  else {
    if (exists("adj.XY") && !is.null(adj.XY)) {
      res.file$adjXY <- adj.XY
    }
  }
  
  write.csv(result$dist.matrix, paste0(as.character(nm), "_distmat.csv"))

  kmlXY <- st_as_sf(sampleXY, coords = c("X", "Y"), crs = 4326)
  sf2KML(kmlXY, colour = "red", outputPath = paste0(as.character(nm), "_XY.kml"))
  # kmlPoints(kmlXY, kmlfile = paste0(as.character(nm), "_XY.kml"),
  #           name = sp.inds$ID, icon = "http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png")
  if (exists("wet.sp.inds") && !is.null(wet.sp.inds)) {
    if (exists("adj.wet.XYkml")) {

      sf2KML(adj.wet.XYkml, colour = "green", outputPath = paste0(as.character(nm), "_adjustedXY.kml"))
      # kmlPoints(adj.wet.XYkml, kmlfile = paste0(as.character(nm),
      #                                           "_adjustedXY.kml"), name = sp.inds$ID, icon = "http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png")
    }  else {
      #names(wet.sp.inds)[names(wet.sp.inds) == "sampleXY$ID"] <- "ID"
      sf2KML(wet.sp.inds, colour = "green", outputPath = paste0(as.character(nm), "_adjustedXY.kml"))
      # kmlPoints(wet.sp.inds, kmlfile = paste0(as.character(nm),
      #                                         "_adjustedXY.kml"), name = sp.inds$ID, icon = "http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png")
    }
  }
  sink(paste0(as.character(nm), "_results.txt"))
  print(res.file)
  sink()
  return(result)
}

