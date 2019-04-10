#' test shp data
#'
#'
#' @export




mapshp <- function () {

   newshp <- readOGR("extdata/mapfiles", "ne_10m_land")
   ras.extent <- as(extent(118.0496, 138.0496, -38.88000, -28.88000), 'SpatialPolygons')
   cut.shp <- newshp
   cut.shp@bbox <- as.matrix(extent(ras.extent))
   init.ras <- raster(nrow=500, ncol=500)
   crs(init.ras) <- crs(cut.shp)
   extent(init.ras) <- extent(cut.shp)
   main.ras <- rasterize(cut.shp, init.ras)
   plot(main.ras)

}

