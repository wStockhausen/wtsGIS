#'
#' @title Create a basemap layer for maps based on the tmap package
#'
#' @description This function creates a basemap layer for maps based on the tmap package.
#'
#' @details The basemap contains a land layer (polygons) and a bathymetry layer (lines).
#' Uses \code{getPackagedLayer} or \code{wtsUtilities::readShapefile} to create the
#' land and bathymetric layers. If \code{bbox} is NULL, then the
#' bounding box for the land layer is used as the bounding box for the basemap.
#'
#' @param layer.land - a \code{sf::sf} object representing land
#' @param layer.bathym - a \code{sf::sf} object representing bathymetry
#' @param crs.finl - representation of final (display) coordinate reference system (see details)
#' @param bbox - a bounding box (see details)
#' @param colors.bg - background color
#' @param colors.land - color for land
#' @param colors.bathym - color for the bathymetry
#' @param alpha.bathym - transparency for the bathymetry
#' @param points.size - size of points, in map units
#'
#' @return - basemap layer based on the tmap package
#'
#' @details The final coordinate reference system (\code{crs.finl}) can be any object that
#' can be converted to a \code{sf::crs} object using \code{get_crs}.
#'
#' The bounding box (\code{bbox}) can be any object that can be converted
#' to a \code{sf::bbox} using \code{getBBox}.
#'
#' @import magrittr
#'
#' @export
#'
createBaseTMap<-function( layer.land=getPackagedLayer("Alaska"),
                          layer.bathym=getPackagedLayer("ShelfBathymetry"),
                          crs.finl=get_crs("WGS84"),
                          bbox=getStandardBBox("EBS"),
                          colors.bg="white",
                          colors.land="grey85",
                          colors.bathym="darkblue",
                          alpha.bathym=1.0,
                          points.size=0.01
                          ){
  #make sure crs.finl is a sf::crs object
  crs.finl<-get_crs(crs.finl);

  land<-layer.land;
  if (!is.null(crs.finl)) land <- transformCRS(land,crs.finl);

  bathym<-layer.bathym;
  if (!is.null(crs.finl)) bathym <- transformCRS(bathym,crs.finl);

  #define bounding box for map extent
  if (is.null(bbox)) {
    bbext<-getBBox(land);#--no bbox, so use extent of land
  } else {
    bbext<-getBBox(bbox);#--use bbox
    #--make sure bbox is is same crs as land
    crs_land<-get_crs(land);
    if (crs_land!=get_crs(bbext)) bbext<-transformBBox(bbext,crs_land);
  }

  #basemap using projection to crs.finl
  basemap<-tmap::tm_shape(land,bbox=bbext,is.master=TRUE,projection=crs.finl)+
             tmap::tm_fill(col=colors.land)+tmap::tm_layout(bg.color=colors.bg);
  if (!is.null(bathym))
      basemap <- basemap + tmap::tm_shape(bathym) + tmap::tm_lines(col=colors.bathym,
                                                                   alpha=alpha.bathym);

  return(basemap);
}
