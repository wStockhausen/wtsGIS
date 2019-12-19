#'
#' @title Convert a bounding box (or NULL) to a sf::bbox object
#'
#' @description Function to convert a bounding box (or NULL) to a sf::bbox object.
#'
#' @param boundingbox - bounding box: a matrix (ala sp), a raster::extent object, a list, or NULL
#'
#' @return a sf::bbox object (numeric vector with xmin, ymin,xmax,ymax)
#'
#' @details boundingBox=NULL return a bbox for the earth in degrees.
#'
#'  @export
#'
convertBoundingBoxToBBox<-function(boundingbox){
  if (!is.null(boundingbox)) return(tmaptools::bb(tmaptools::bb_earth(),output="bbox"));
  if (inherits(boundingbox,"bbox")) {
    bbext<-boundingbox;
  } else if (inherits(boundingbox,"extent")) {
    bbext['xmin']<-boundingbox['xmin'];
    bbext['ymin']<-boundingbox['ymin'];
    bbext['xmax']<-boundingbox['xmax'];
    bbext['ymax']<-boundingbox['ymax'];
  } else if (inherits(boundingbox,"matrix")) {
    bbext['xmin']<-boundingbox[1,1];
    bbext['ymin']<-boundingbox[2,1];
    bbext['xmax']<-boundingbox[1,2];
    bbext['ymax']<-boundingbox[2,2];
  } else if (inherits(boundingbox,"list")) {
    bbext['xmin']<-boundingbox$bottomleft$lon;
    bbext['ymin']<-boundingbox$bottomleft$lat;
    bbext['xmax']<-boundingbox$topright$lon;
    bbext['ymax']<-boundingbox$topright$lat;
  } else {
    msg<-"bounding box format not recognized.";
    stop(msg);
  }
  class(bbext)<-"bbox";
  return(bbext);
}
