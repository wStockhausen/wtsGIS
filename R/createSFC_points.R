#'
#' @title Create an \pkg{sf} (simple features) column with point geometries from x,y coordinate vectors
#'
#' @description Function to create an \pkg{sf} (simple features) column with point geometries (i.e., an sfc_POINT object) from x,y coordinate vectors.
#'
#' @param x - vector of x coordinates
#' @param y - vector of y coordinates
#' @param crs - coordinate reference system (convertible to an sf crs by \code{\link{get_crs}})
#' @param wrapDateline - flag (T/F) to use 0 to 360 rather than -180 to 180 range for longitudes
#'
#' @return an object of class sfc_POINT
#'
#' @details Uses package \pkg{sf}.
#'
#' @importFrom sf st_point
#' @importFrom sf st_sfc
#'
#' @export
#'
createSFC_points<-function(x,y,crs=sf::NA_crs_,wrapDateline=FALSE){
  #--create point geometries
  nr<-length(x);
  if (wrapDateline) ifelse(x<0,360+x,x);
  geoms<-vector(length=nr,mode="list");
  for (rw in 1:nr){
    geoms[[rw]] <- sf::st_point(x=c(x[rw],y[rw]),
                                dim="XY");
  }
  if (!inherits(crs,"crs")) crs = get_crs(crs);#--convert to sf::crs, if possible
  sfc_geoms<-sf::st_sfc(geoms,crs=crs);
  return(sfc_geoms);
}
