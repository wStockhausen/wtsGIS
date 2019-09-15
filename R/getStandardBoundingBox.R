#'
#' @title Create a bounding box to display a standard map area
#'
#' @description This function returns a bounding box to display a standard map area.
#'
#' @details The bounding box is returned in list format, with coordinates defining the
#' bottomleft and topright boundaries for the area.
#'
#' @param area - area to return the bounding box for ("EBS","CGOA")
#'
#' @return - bounding box in list format
#'
#' @export
#'
getStandardBoundingBox<-function(area=c("EBS","CGOA")){
  if (toupper(area[1])=="EBS")
    return(list(bottomleft=list(lon=-179,lat=54),
                topright  =list(lon=-157,lat=62.5)));
  if (toupper(area[1])=="CGOA")
    return(list(bottomleft=list(lon=-162,lat=50),
                topright  =list(lon=-136,lat=60)));
}
