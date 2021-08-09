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
#' @return XY.kml                a .kml file of the adjusted coordinates that you can view in google earth
#'
#'
#' @author Chris Brauer
#' @keywords pairwise distance viamaris
#' @export
#' @examples
#'  ## set directory for results to be written
#'  setwd("path/to/working/directory")
#'
#'  # load example coordinate file
#'  data(Tursiops.XY)
#'
#'  # run analysis
#'  dist.mat <- viamaris(Tursiops.XY, extent.buffer = 5)



viamaris <- function (sampleXY, extent.buffer = NULL)
{
  inds <- as.data.frame(sampleXY$ID)
  XY <- as.matrix(cbind(sampleXY$X, sampleXY$Y))
  if (is.null(extent.buffer))
    extent.buffer <- 0.5

  resolution <- 1500

  nm <- deparse(substitute(sampleXY))

  mapfile <- melfuR:::wldmap
  mapfile.sf <- as(mapfile, "sf")
  wkt <- comment(raster::crs(mapfile.sf))

  rawXY <- as.data.frame(sampleXY)
  coordinates(rawXY) <- c("X", "Y")
  proj4string(rawXY) <- wkt
  nearestWater <- function(points, raster, max_distance) {
    nearest <- function(lis, raster) {
      neighbours <- matrix(lis[[1]], ncol = 2)
      point <- lis[[2]]
      land <- !is.na(neighbours[, 2])
      if (!any(land)) {
        return(c(NA, NA))
      }
      else {
        coords <- xyFromCell(raster, neighbours[land,
                                                1])
        if (nrow(coords) == 1) {
          return(coords[1, ])
        }
        dists <- sqrt((coords[, 1] - point[1])^2 + (coords[,
                                                           2] - point[2])^2)
        return(coords[which.min(dists), ])
      }
    }
    neighbour_list <- extract(raster, points, buffer = max_distance,
                              cellnumbers = TRUE)
    neighbour_list <- lapply(1:nrow(points), function(i) {
      list(neighbours = neighbour_list[[i]], point = as.numeric(points[i,
      ]))
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
    coordinates(XY360) <- c("X", "Y")
    proj4string(XY360) <- wkt
    sp.inds <- XY360
    cat("\npreparing raster\n")
    minX <- XY360@bbox[1, 1] - extent.buffer
    maxX <- XY360@bbox[1, 2] + extent.buffer
    minY <- XY360@bbox[2, 1] - extent.buffer
    maxY <- XY360@bbox[2, 2] + extent.buffer
    ras.extent <- extent(as(extent(minX, maxX, minY, maxY),
                            "SpatialPolygons"))
    init.ras <- raster(nrow = resolution, ncol = resolution,
                       ext = ras.extent, crs=wkt)
    newmapfile <- st_wrap_dateline(mapfile.sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)
    main.ras <- rasterize(newmapfile, init.ras)


  }
  else {
    coordinates(sampleXY) <- c("X", "Y")
    proj4string(sampleXY) <- wkt
    sp.inds <- sampleXY
    cat("\npreparing raster\n")
    minX = sampleXY@bbox[1, 1] - extent.buffer
    maxX = sampleXY@bbox[1, 2] + extent.buffer
    minY = sampleXY@bbox[2, 1] - extent.buffer
    maxY = sampleXY@bbox[2, 2] + extent.buffer
    ras.extent <- extent(as(extent(minX, maxX, minY, maxY),
                            "SpatialPolygons"))
    init.ras <- raster(nrow = resolution, ncol = resolution,
                       ext = ras.extent, crs=wkt)
    main.ras <- rasterize(mapfile, init.ras)
  }
  main.ras <- is.na(main.ras)
  main.ras[main.ras == 0] <- NA
  plot(main.ras, breaks = c(0, 1), col = "blue", legend = FALSE)
  plot(sp.inds, col = "red", cex = 1, add = T)
  cat("\nanalysing raster\n")
  tr <- geoCorrection(transition(main.ras, function(x) 1/mean(x),
                                 8), scl = FALSE)
  if (any(is.na(as.data.frame(extract(main.ras, sp.inds))))) {
    cat("\nsome samples are on dry land!\n")
    cat("\ndon't worry, this may just be an artefact of the raster resolution\n")
    cat("...adjusting coordinates to nearest water\n\n")
    search.radius <- 0
    repeat {
      search.radius <- search.radius + 1000
      cat(paste0("optimising search radius: ", search.radius,
                 " metres\n"))
      wetXY <- nearestWater((as.matrix(coordinates(sp.inds))),
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
    coordinates(IND.wetXY.coord) <- c("x", "y")
    proj4string(IND.wetXY.coord) <- wkt
    wet.sp.inds <- IND.wetXY.coord
    adj.XY <- as.matrix(coordinates(wet.sp.inds))
    plot(wet.sp.inds, col = "limegreen", cex = 1, pch = 16,
         add = T)
    cost <- as.matrix(costDistance(tr, wet.sp.inds)/1000)
    if (any(adj.XY[, 1] > 180)) {
      adj.wet.XYkml <- adj.XY
      adj.wet.Xkml <- as.matrix(ifelse(adj.XY[, 1] > 180,
                                       (-360 + adj.XY[, 1][adj.XY[, 1] > 180]), adj.XY[,
                                                                                       1]))
      adj.wet.XYkml <- as.data.frame(cbind(inds, adj.wet.Xkml[,
                                                              1], adj.wet.XYkml[, 2]))
      colnames(adj.wet.XYkml) <- c("ID", "X", "Y")
      coordinates(adj.wet.XYkml) <- c("X", "Y")
    }
  }
  else {
    cat("\n...calculating distance matrix\n")
    wet.sp.inds <- NULL
    adj.XY <- NULL
    XY <- as.matrix(coordinates(sp.inds))
    plot(sp.inds, col = "limegreen", cex = 1, pch = 16, add = T)
    cost <- as.matrix(costDistance(tr, sp.inds)/1000)
  }
  colnames(cost) <- t(inds)
  rownames(cost) <- inds[, 1]
  cat("\nYour results are ready, have a nice day!\n")
  result <- list()
  result$call <- match.call()
  result$dist.matrix <- cost
  result$raster <- main.ras
  result$XY.SpatialPointsDataFrame <- sp.inds
  result$XY <- XY
  result$adjXY.SpatialPointsDataFrame <- wet.sp.inds
  result$adj.XY <- adj.XY
  if (exists("adj.wet.XYkml")) {
    result$adj.XY360 <- adj.XY
    result$adj.XY180 <- as.matrix(coordinates(adj.wet.XYkml))
  }
  else {
    result$adjXY <- adj.XY
  }
  res.file <- list()
  res.file$call <- match.call()
  res.file$raster <- main.ras
  res.file$XY.SpatialPointsDataFrame <- summary(sp.inds)
  res.file$XY <- XY
  res.file$adjXY.SpatialPointsDataFrame <- summary(wet.sp.inds)
  if (exists("adj.wet.XYkml")) {
    res.file$adjXY <- as.matrix(coordinates(adj.wet.XYkml))
  }
  else {
    res.file$adjXY <- adj.XY
  }
  write.csv(result$dist.matrix, paste0(as.character(nm), "_distmat.csv"))
  kmlPoints(rawXY, kmlfile = paste0(as.character(nm), "_XY.kml"),
            name = sp.inds$ID, icon = "http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png")
  if (exists("wet.sp.inds")) {
    if (exists("adj.wet.XYkml")) {
      kmlPoints(adj.wet.XYkml, kmlfile = paste0(as.character(nm),
                                                "_adjustedXY.kml"), name = sp.inds$ID, icon = "http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png")
    }
    else {
      kmlPoints(wet.sp.inds, kmlfile = paste0(as.character(nm),
                                              "_adjustedXY.kml"), name = sp.inds$ID, icon = "http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png")
    }
  }
  sink(paste0(as.character(nm), "_results.txt"))
  print(res.file)
  sink()
  return(result)
}

