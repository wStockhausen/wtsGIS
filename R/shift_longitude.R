#' 
#' @title Shift longitude coordinates of \pkg{sf} object to 0-360
#' 
#' @description Function to shift longitude coordinates of \pkg{sf} object to 0-360.
#' 
#' @param obj - object to shift coordinates
#' 
#' @return object of same class as obj, but with longitudes shifted
#' 
#' @details This function expands the \code{\link[sf]{st_shift_longitude}} function
#' to work with \code{sf::bbox} objects as well as other \pkg{sf} objects.
#' 
#' If the coordinate reference system of the object is not lat/lon, no shift is made.
#' 
#' @import sf
#' 
#' @export
#' 
shift_longitude<-function(obj){
  if (sf::st_is_longlat(obj)){
    if (inherits(obj,"bbox")){
      obj["xmin"] = ifelse(obj["xmin"]<0,360+obj["xmin"],obj["xmin"]);
      obj["xmax"] = ifelse(obj["xmax"]<0,360+obj["xmax"],obj["xmax"]);
    } else {
      obj %<>% sf::st_shift_longitude();
    }
  }
  return(obj);
}