#'
#'@title Crop features (a \code{sf::sf} or \code{sf::sfc} object) to a bounding box
#'
#'@description Function to crop features (a \code{sf::sf} or \code{sf::sfc} object) to a bounding box.
#'
#'@param obj - object of class "sf" or "sfc"
#'@param bbx  - object that can be converted to a sf:bbox (see details)
#'
#'@return - object of same type as \code{obj}, but cropped to the bbox limits
#'
#'@details \code{bbx} can be any object that can be converted to a \code{sf::bbox}
#'using \code{getBBox}.
#'
#' @note The bounding box must be in the same coordinate system as \code{obj}. If the
#' coordinate reference system for \code{bbx} is undefined, it is assumed to be the same as
#' that for \code{obj}.
#'
#' @importFrom sf st_crs
#' @importFrom sf st_crop
#'
#' @export
#'
cropFeaturesToBBox<-function(obj,bbx){
    bbxp<-bbx;
    if (!inherits(bbxp,"bbox")) bbxp<-getBBox(bbxp);#--convert to sf::bbox
    crs_obj<-sf::st_crs(obj);
    crs_bbx<-sf::st_crs(bbxp);
    if (is.na(crs_bbx)) crs_bbx<-crs_obj; #--assume unset crs is same as obj
    if (crs_bbx!=crs_obj)
        stop("Coordinate reference systems must be the same.")
    bbx<-getBBox(bbx,crs=crs_obj);
    return(sf::st_crop(obj,bbx));
}
