#'
#' @title Create a \code{sf::bbox} for a standard map area
#'
#' @description This function \code{sf::bbox} for a standard map area.
#'
#' @details The \code{sf::bbox} is returned in vector format, with values defining
#' xmin, ymin, xmax, ymax limits for the area.
#'
#' @param area - area to return the bounding box for ("EBS","CGOA")
#'
#' @return - bounding box in list format
#'
#' @importFrom sf st_bbox
#'
#' @export
#'
getStandardBBox<-function(area=c("EBS","CGOA")){
    if (toupper(area[1])=="EBS"){
        v<-c(xmin=-179,ymin=54,xmax=-157,ymax=62.5);
        bbx<-sf::st_bbox(v,crs=get_crs("WGS84"))
        return(bbx);
    }
    if (toupper(area[1])=="CGOA"){
        v<-c(xmin=-162,ymin=50,xmax=-136,ymax=60);
        bbx<-sf::st_bbox(v,crs=get_crs("WGS84"))
        return(bbx);
    }
    stop(paste0("'",area[1],"' is not a standard area (i.e., 'EBS' or 'CGOA')"));
    return(NULL);
}
