#'
#' @title Create an sf (simple features) column with point geometries from x,y coordinate vectors
#'
#' @description Function to create an sf column (simple features) with point geometries (i.e., an sfc_POINT object) from x,y coordinate vectors.
#'
#' @param x - vector of x coordinates
#' @param y - vector of y coordinates
#' @param crs - coordinate reference system: EPSG code or character with proj4string
#'
#' @return an object of class sfc_POINT
#'
#' @details Uses package \code{sf}.
#'
#' @export
#'
createSFC_points<-function(x,y,crs=sf::NA_crs_){
  #--create point geometries
  nr<-length(x);
  geoms<-vector(length=nr,mode="list");
  for (rw in 1:nr){
    geoms[[rw]] <- sf::st_point(x=c(x[rw],y[rw]),
                                dim="XY");
  }
  sfc_geoms<-sf::st_sfc(geoms,crs=crs);
  return(sfc_geoms);
}
