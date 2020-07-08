#'
#' @title Get a sf::bbox object based on another object
#'
#' @description Function to get a sf::bbox object based on another object.
#'
#' @param obj - an object that can be converted to a bbox, or from which one can be extracted
#' @param crs - an object that can be converted to a sf::crs object using \code{get_crs(crs)}
#'
#' @return a sf::bbox object (numeric vector of class "bbox" with xmin, ymin, xmax, ymax and a sf::crs as an attribute)
#'
#' @details obj=NULL returns a bbox for the Earth in degrees, with WGS84 as crs. Otherwise, obj can be:
#' \itemize{
#'   \item{a 4-element vector with values xmin, ymin, xmax, ymax}
#'   \item{a 2x2 matrix with xmin, ymin on first row, xmax, ymax on second row}
#'   \item{a list with sub-list elements \code{bottomleft=c(xmin,ymin)} and \code{topright=c(xmax,ymax)}}
#'   \item{an object on which \code{sf::st_bbox(obj)} can be called}
#' }
#'
#' \code{crs} is ignored unless obj is a vector, matrix or a list; it is taken directly from \code{obj}.
#'
#' @importFrom sf st_bbox
#'
#' @export
#'
getBBox<-function(obj,crs=NULL){
  bbx<-c(xmin=-180,ymin=-90,xmax=180,ymax=90);
  if (is.null(obj)) {
    bbx<-sf::st_bbox(bbx,crs=get_crs("WGS84"));
  } else if (class(obj)[1]=="numeric") {
    bbx['xmin']<-obj[1];
    bbx['ymin']<-obj[2];
    bbx['xmax']<-obj[3];
    bbx['ymax']<-obj[4];
    bbx<-(if(is.null(crs)) sf::st_bbox(bbx) else sf::st_bbox(bbx,crs=get_crs(crs)));
  }else if (class(obj)[1]=="matrix") {
    bbx['xmin']<-obj[1,1];
    bbx['ymin']<-obj[2,1];
    bbx['xmax']<-obj[1,2];
    bbx['ymax']<-obj[2,2];
    bbx<-(if(is.null(crs)) sf::st_bbox(bbx) else sf::st_bbox(bbx,crs=get_crs(crs)));
  } else if (class(obj)[1]=="list") {
    bbx['xmin']<-obj$bottomleft[[1]];
    bbx['ymin']<-obj$bottomleft[[2]];
    bbx['xmax']<-obj$topright[[1]];
    bbx['ymax']<-obj$topright[[2]];
    bbx<-(if(is.null(crs)) sf::st_bbox(bbx) else sf::st_bbox(bbx,crs=get_crs(crs)));
  } else {
    bbx<-sf::st_bbox(obj);
  }
  return(bbx);
}
