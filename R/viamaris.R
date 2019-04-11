#' calculate pairwise oceanic distances among sampling locations
#'
#'
#' @param sampleXY -  a data frame with three columns: ID, X and Y (in decimal degrees)
#' @param extent.buffer (optional) number of degrees to extend raster around your samples, default = 0.5
#'                      (you may need to increase this to allow navigation around land masses that extend beyond the extent of your samples)
#' @param resolution (optional) the size of the raster, default = 1500 (1500 X 1500) (caution, increasing this too much will consume lots of RAM, without much benefit in accuracy)
#' @param EPSG (optional) the coordinate reference system of your coordinates, default = 4326 (WGS84)
#'
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
#'  # load example structure file
#'  data(Tursiops.XY)
#'
#'  # run analysis
#'  dist.mat <- viamaris(Tursiops.XY, extent.buffer = 5)



viamaris <- function (sampleXY, extent.buffer = NULL, resolution = NULL, EPSG = NULL) {


  # define some variables
  inds <- as.data.frame(sampleXY$ID)
  XY <- as.matrix(cbind(sampleXY$X, sampleXY$Y))

  # set default buffer of 0.5 degrees N,S,E and W of sample coordinates
  if(is.null(extent.buffer))
    extent.buffer <- 0.5

  # set default raster dimensions of 1500x1500 pixels
  if(is.null(resolution))
    resolution <- 1500

  # set default coordinate reference system WGS84
  if(is.null(EPSG))
    EPSG <- 4326

  cs <- CRS(paste0("+init=epsg:",EPSG))

  # get input object name to use as name of output file
  nm <- deparse(substitute(sampleXY))

  # Read in Shapefile
  mapfile <- melfuR:::wldmap
  print(mapfile)


  # define SpatialPointsDataFrame for raw sampleXYs
  rawXY <- as.data.frame(sampleXY)
  coordinates(rawXY)<- c("X", "Y")
  proj4string(rawXY)<- cs
  rawXY <- spTransform(rawXY, CRS= cs)

  # define NA to nearest !NA func. (nearestWater (was nearestLand)) function borrowed from https://github.com/SEEG-Oxford/seegSDM)
  nearestWater <- function (points, raster, max_distance) {
    # get nearest non_na cells (within a maximum distance) to a set of points
    # points can be anything extract accepts as the y argument
    # max_distance is in the map units if raster is projected
    # or metres otherwise

    # function to find nearest of a set of neighbours or return NA
    nearest <- function (lis, raster) {
      neighbours <- matrix(lis[[1]], ncol = 2)
      point <- lis[[2]]
      # neighbours is a two column matrix giving cell numbers and values
      land <- !is.na(neighbours[, 2])
      if (!any(land)) {
        # if there is no land, give up and return NA
        return (c(NA, NA))
      } else{
        # otherwise get the land cell coordinates
        coords <- xyFromCell(raster, neighbours[land, 1])

        if (nrow(coords) == 1) {
          # if there's only one, return it
          return (coords[1, ])
        }

        # otherwise calculate distances
        dists <- sqrt((coords[, 1] - point[1]) ^ 2 +
                        (coords[, 2] - point[2]) ^ 2)

        # and return the coordinates of the closest
        return (coords[which.min(dists), ])
      }
    }

    # extract cell values within max_distance of the points
    neighbour_list <- extract(raster, points,
                              buffer = max_distance,
                              cellnumbers = TRUE)

    # add the original point in there too
    neighbour_list <- lapply(1:nrow(points),
                             function(i) {
                               list(neighbours = neighbour_list[[i]],
                                    point = as.numeric(points[i, ]))
                             })

    return (t(sapply(neighbour_list, nearest, raster)))
  }

  # check for samples either side of the meridian, if TRUE, convert sample longitude from -180/180 to 0/360
  if(any(sampleXY$X < 0) && any(sampleXY$X > 0)) {

    # convert coordinates
    XY360 <- as.matrix(cbind(sampleXY$X, sampleXY$Y))
    X360 <- as.matrix(ifelse(sampleXY$X < 0, (360+sampleXY$X[sampleXY$X < 0]), sampleXY$X))
    XY360 <- as.data.frame(cbind(inds, X360[,1], XY360[,2]))
    colnames(XY360) <- c("ID", "X", "Y")
    coordinates(XY360) <- c("X", "Y")
    sp.inds <- XY360
    print(sp.inds)

    # build and convert raster
    cat("\npreparing raster\n")
    minX <- XY360@bbox[1,1] - extent.buffer
    maxX <- XY360@bbox[1,2] + extent.buffer
    minY <- XY360@bbox[2,1] - extent.buffer
    maxY <- XY360@bbox[2,2] + extent.buffer
    print(minX)
    print(maxX)
    print(minY)
    print(maxY)

    ras.extent <- extent(as(extent(minX, maxX, minY, maxY), 'SpatialPolygons'))
    print(ras.extent)
    init.ras <- raster(nrow=resolution, ncol=resolution, ext = ras.extent)
    print(init.ras)
    cut.shp <- mapfile
    print(cut.shp)
    cut.shp@bbox <- as.matrix(extent(init.ras))
    print(cut.shp@bbox)
    main.ras <- rasterize(cut.shp, init.ras)
    print(main.ras)



  } else {

    # define SpatialPointsDataFrame for sampleXYs
    coordinates(sampleXY)<- c("X", "Y")
    proj4string(sampleXY)<- cs
    sp.inds <- spTransform(sampleXY, CRS= cs)

    # define main raster
    cat("\npreparing raster\n")
    minX=sampleXY@bbox[1,1] - extent.buffer
    maxX=sampleXY@bbox[1,2] + extent.buffer
    minY=sampleXY@bbox[2,1] - extent.buffer
    maxY=sampleXY@bbox[2,2] + extent.buffer

    ras.extent <- extent(as(extent(minX, maxX, minY, maxY), 'SpatialPolygons'))
    init.ras <- raster(nrow=resolution, ncol=resolution, ext = ras.extent)
    cut.shp <- mapfile
    cut.shp@bbox <- as.matrix(extent(init.ras))
    main.ras <- rasterize(cut.shp, init.ras)

  }

  main.ras <- is.na(main.ras)

  # set land to NA
  main.ras[main.ras==0] <- NA

  plot(main.ras,breaks=c(0,1),col="blue", legend=FALSE)
  plot(sp.inds, col="red", cex = 1, add=T)



  # create a transition object, correcting for unequal length raster cell x,y
  cat("\nanalysing raster\n")
  tr <- geoCorrection(transition(main.ras, function(x) 1/mean(x), 8), scl=FALSE)

  if (any(is.na(as.data.frame(extract(main.ras, sp.inds))))) {
    cat("\nsome samples are on dry land!\n")
    cat("\ndon't worry, this may just be an artefact of the raster resolution\n")
    cat("...adjusting coordinates to nearest water\n\n")

    search.radius <- 0
    repeat {
      search.radius <- search.radius+1000
      cat(paste0("optimising search radius: ",search.radius," metres\n"))
      wetXY <- nearestWater((as.matrix(coordinates(sp.inds))), main.ras, search.radius)

      if (search.radius > 10000){cat("\n\ncannot find water nearby, please check your coordinates\n\n")
        break}
      if (!any(is.na(wetXY))){
        cat("\nfound it!\n")
        cat("\n...calculating distance matrix\n")
        break
      }
    }

    IND.wetXY.coord <- cbind(inds ,wetXY)
    coordinates(IND.wetXY.coord)<- c("x", "y")
    proj4string(IND.wetXY.coord)<- cs
    wet.sp.inds <- spTransform(IND.wetXY.coord, CRS= cs)
    adj.XY <- as.matrix(coordinates(wet.sp.inds))
    plot(wet.sp.inds, col="limegreen", cex = 1, pch = 16, add=T)
    #text(sp.inds, labels = inds[,1], pos = 4)
    cost <- as.matrix(costDistance(tr, wet.sp.inds)/1000)

    # check for samples either side of the meridian, if TRUE, convert sample longitude back to -180/180 for generating KMLs
    if(any(adj.XY[,1] > 180)) {

      adj.wet.XYkml <- adj.XY
      adj.wet.Xkml <- as.matrix(ifelse(adj.XY[,1] > 180, (-360+adj.XY[,1][adj.XY[,1] > 180]), adj.XY[,1]))
      adj.wet.XYkml <- as.data.frame(cbind(inds, adj.wet.Xkml[,1], adj.wet.XYkml[,2]))
      colnames(adj.wet.XYkml) <- c("ID", "X", "Y")
      coordinates(adj.wet.XYkml) <- c("X", "Y")

    }

  } else {
    # calculate cost distance matrix in km
    cat("\n...calculating distance matrix\n")
    wet.sp.inds <- NULL
    adj.XY <- NULL
    XY <- as.matrix(coordinates(sp.inds))
    plot(sp.inds, col="limegreen", cex = 1, pch = 16, add=T)
    #text(sp.inds, labels = inds[,1], pos = 4)
    cost <- as.matrix(costDistance(tr, sp.inds)/1000)

  }

  colnames(cost) <- t(inds)
  rownames(cost) <- inds[,1]
  cat("\nYour distance matrix is ready, have a nice day!\n")


  # build result object
  result <- list()
  result$call <- match.call()
  result$dist.matrix <- cost
  result$raster <- main.ras
  result$XY.SpatialPointsDataFrame <- sp.inds
  result$XY <- XY
  result$adjXY.SpatialPointsDataFrame <- wet.sp.inds
  result$adj.XY <- adj.XY
  if(exists("adj.wet.XYkml")) {
    result$adj.XY360 <- adj.XY
    result$adj.XY180 <- as.matrix(coordinates(adj.wet.XYkml))

  } else {
    result$adjXY <- adj.XY
  }



  # build result file
  res.file <- list()
  res.file$call <- match.call()
  res.file$raster <- main.ras
  res.file$XY.SpatialPointsDataFrame <- summary(sp.inds)
  res.file$XY <- XY
  res.file$adjXY.SpatialPointsDataFrame <- summary(wet.sp.inds)

  if(exists("adj.wet.XYkml")) {
    res.file$adjXY <- as.matrix(coordinates(adj.wet.XYkml))

  } else {
    res.file$adjXY <- adj.XY
  }


  # write results
  write.csv(result$dist.matrix, paste0(as.character(nm),"_distmat.csv"))


  kmlPoints(rawXY, kmlfile = paste0(as.character(nm),"_XY.kml"), name = sp.inds$ID,
            icon = "inst/extdata/red-pushpin.png")


  if (exists("wet.sp.inds")) {

    if(exists("adj.wet.XYkml")) {
      kmlPoints(adj.wet.XYkml, kmlfile = paste0(as.character(nm),"_adjustedXY.kml"), name = sp.inds$ID,
                icon = "inst/extdata/grn-pushpin.png")

    } else {
      kmlPoints(wet.sp.inds, kmlfile = paste0(as.character(nm),"_adjustedXY.kml"), name = sp.inds$ID,
                icon = "inst/extdata/grn-pushpin.png")
    }

  }

  sink(paste0(as.character(nm),"_results.txt"))
  print(res.file)
  sink()


  return(result)

}

