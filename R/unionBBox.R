#'
#' @title Union two bounding boxes
#'
#' @description Function to union two bounding boxes.
#'
#' @param bbx1 - original bounding box
#' @param bbx2 - updating bounding box
#'
#' @return resulting bounding box as an sf::bbox object
#'
#' @details Returns a \code{sf::bbox} covering the union of the individual bounding boxes.
#' If not \code{sf::bbox}'s, the bounding boxes are converted to \code{sf::bbox} objects
#' using \code{getBBox}, then unioned.
#'
#' @export
#'
unionBBox<-function(bbx1=NULL,bbx2=NULL){
  #--convert to sf::bbox format
  if (!is.null(bbx1)) bbx1<-getBBox(bbx1);
  if (!is.null(bbx2)) bbx2<-getBBox(bbx2);
  #--handle NULL objects
  if (is.null(bbx1)) return(bbx2);
  if (is.null(bbx2)) return(bbx1);
  #--process non-null bboxes
  bbx<-bbx1;
  bbx["xmin"]<-min(bbx1["xmin"],bbx2["xmin"],na.rm=TRUE);
  bbx["xmax"]<-max(bbx1["xmax"],bbx2["xmax"],na.rm=TRUE);
  bbx["ymin"]<-min(bbx1["ymin"],bbx2["ymin"],na.rm=TRUE);
  bbx["ymax"]<-max(bbx1["ymax"],bbx2["ymax"],na.rm=TRUE);
  return(bbx);
}
