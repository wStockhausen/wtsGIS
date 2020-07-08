#'
#' @title Crop a map to a bounding box
#'
#' @description Function to crop a map to a bounding box
#'
#' @param map - tmap object
#' @param bbx - object convertible to a \code{sf::bbox}
#'
#' @return tmap object with bounding box set to projected bbox
#'
#' @details bbx replaces the bbox on the map object. Prior to replacement,
#' bbx is projected to the same coordinate reference system as the map.
#'
#' @export
#'
cropMapToBBox<-function(map,
                        bbx){
  bbxp<-getBBox(bbx);#--make sure bbx is a sf::bbox object
  crs_map<-map[[1]]$projection;
  crs_bbxp<-get_crs(bbxp);
  if (is.na(crs_bbxp)||(crs_map!=crs_bbxp)){
    # if (is.na(crs_bbxp)) crs_bbxp=crs_map;       #assume bbox has same crs as map, if necessary
    # sfc_bbxp<-sf::st_as_sfc(bbxp,crs=crs_bbxp);  #convert bbox to sfc for transformation
    # sfc_bbxp<-sf::st_transform(sfc_bbxp,crs_map);#transform to map coordinates
    # bbxp<-getBBox(sfc_bbxp);                     #convert back to sf::bbox
    bbxp<-transformBBox(bbxp,crs_map)
  }
  map[[1]]$bbox<-bbxp;
  return(map);
}
