#'
#'@title Clip a set of polygons using a bounding box
#'
#'@description Function to clip a set of polygons using a bounding box.
#'
#'@param shp - set of polygons (from call to readOGR)
#'@param bb  - bounding box (see details)
#'@param verbosity - integer flag (>0) to print intermediate info
#'
#'@return - spatial dataframe of clipped polygons
#'
#'@details The bounding box can be one of the following (see extent{raster}):\cr
#' 1. vector of coordinates given as xmin, xmax, ymin, ymax\cr
#' 2. vector of coordinates given as (with names): "left","bottom","top","right"\cr
#' 3. matrix with columns corresponding to "x","y" and rows to "lower left", "upper right"\cr
#' 4. dataframe with columns corresponding to "x","y" and rows to "lower left", "upper right"\cr
#' \cr
#' Also, the bounding box must be in the same coordinate system as the set of polygons.\cr
#' Uses \code{raster::extent}, \code{sp::proj4string}, and \code{rgeos::gIntersection}.
#'
#' @export
#'
gClip <- function(shp, bb,verbosity=0){
    if (verbosity>0) print(bb);
    bp<-bb;
    if (class(bp)=="data.frame"){
        if (verbosity>1) cat("bp is a data.frame\n")
        b_poly <- as(raster::extent(as.vector((as.matrix(bp)))), "SpatialPolygons")
    } else if (class(bp) == "matrix") {
        if (verbosity>1) cat("bp is a matrix\n")
        b_poly <- as(raster::extent(as.vector(t(bp))), "SpatialPolygons")
    } else {
        #bp expected in order xmin,xmax,ymin,ymax
        if ('left'==tolower(names(bp)[1])){
            if (verbosity>1) cat("bp is a vector w/ order: left, bottom, right, top\n")
            #order is left, bottom, right, top (xmin, ymin, xmax, ymax)
            bp<-c(bb[1],bb[3],bb[2],bb[4]);
            names(bp)<-c('xmin','xmax','ymin','ymax');
            if (verbosity>1) print(bp);
        }
        b_poly <- as(raster::extent(bp), "SpatialPolygons")
    }
    if (verbosity>0) print(b_poly);

    sp::proj4string(b_poly) <- sp::proj4string(shp)
    rgeos::gIntersection(shp, b_poly, byid = T)
}
