#'
#' @title Set a bounding box on a map
#'
#' @description Function to set a bounding box on a map
#'
#' @param map - tmap object
#' @param bbox - bbox object
#' @param crs_bbox - coordinate reference system for bbox
#'
#' @return tmap object with bounding box set to projected bbox
#'
#' @details Requires package \code{tmap} and \code{tmaptools}.
#'
#' @export
#'
setBoundingBoxOnMap<-function(map,
                              bbox,
                              crs_bbox=tmaptools::get_proj4("longlat",output="character")){
  crs_map<-map[[1]]$projection;
  if (is.null(crs_bbox)) crs_bbox <- get_proj4(bbox);
  bbox2<-tmaptools::bb(bbox,
                       current.projection=crs_bbox,
                       projection=crs_map);
  map[[1]]$bbox<-bbox2;
  return(map);
}
