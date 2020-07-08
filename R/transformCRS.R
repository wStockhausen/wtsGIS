#'
#' @title Transform a spatial dataset to a different coordinate reference system
#'
#' @description Function to transform a spatial dataset to a different coordinate reference system.
#'
#' @param layer - spatial data layer to transform
#' @param toCRS - representation of final coordinate reference system (see @details)
#'
#' @return spatial layer of same type as input (sf or sp)
#'
#' @details If \code{layer} is an object of class sf, sfc, or sfg, then \code{toCRS}
#' should be convertible to an sf::crs object via \code{get_crs(toCRS)}.
#'
#' If \code{layer} is an object of class Spatial, then \code{toCRS}
#' should be convertible to an sp::CRS object via \code{get_spCRS(toCRS)} .
#'
#' @importFrom sf st_transform
#' @importFrom sp spTransform
#'
#' @export
#'
transformCRS<-function(layer,toCRS){
  tlayer<-NULL;
  if (inherits(layer,c("sf","sfc","sfg"))){
    crs<-get_crs(toCRS);#--convert toCRS to crs object
    tlayer<-sf::st_transform(layer,crs);
  } else if (inherits(layer,c("Spatial"))){
    CRS<-get_spCRS(toCRS);
    tlayer <- sp::spTransform(layer,CRS);
  }
  return(tlayer);
}
