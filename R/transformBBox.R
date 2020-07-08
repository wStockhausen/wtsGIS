#'
#' @title Transform (or assign) a bounding box's coordinate reference system
#'
#' @description Function to transform (or assign) a bounding box's coordinate reference system.
#'
#' @param bbx - object convertible to a \code{sf::bbox}
#' @param crs - the output coordinate reference system (see @details)
#'
#' @return the transformed bounding box in the specifed crs
#'
#' @details \code{crs} should be an object convertible to a \code{sf::crs} by
#' \code{get_crs}. If the crs of the bbox is NA, then \code{crs} is assigned to
#' the bbox, but no coordinate transformation takes place.
#'
#' @importFrom sf st_as_sfc
#' @importFrom sf st_transform
#'
#' @export
#'
transformBBox<-function(bbx,
                        crs){
    bbxp<-getBBox(bbx);#--make sure bbx is a sf::bbox object
    crsp<-get_crs(crs);#--make sure crs is a sf::crs object
    crs_bbxp<-get_crs(bbxp);      #--get the crs for the bbox
    sfc_bbxp<-sf::st_as_sfc(bbxp);#convert bbox to sfc for transformation
    #--try to make sure sfc_bxpp has a non-NA coordinate reference system
    sf::st_crs(sfc_bbxp)<-(if (!is.na(crs_bbxp)) crs_bbxp else crsp);
    #--transform to new coordinates, if required
    if (crsp!=crs_bbxp) sfc_bbxp<-sf::st_transform(sfc_bbxp,crsp);
    #--convert back to sf::bbox
    bbxp<-getBBox(sfc_bbxp);
    return(bbxp);
}
