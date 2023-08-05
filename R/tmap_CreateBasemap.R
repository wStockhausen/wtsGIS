#'
#' @title Create a basemap layer for maps based on the tmap package
#'
#' @description This function creates a basemap layer for maps based on the tmap package.
#'
#' @details The basemap contains a land layer (polygons) and a bathymetry layer (lines).
#' Uses [getPackagedLayer()] or [readShapefile()] to create the
#' land and bathymetric layers. If \code{bbox} is NULL, then the
#' bounding box for the land layer is used as the bounding box for the basemap.
#'
#' @param layer.land - a \code{sf::sf} object representing land
#' @param layer.bathym - a \code{sf::sf} object representing bathymetry
#' @param final.crs - representation of final (display) coordinate reference system (see details)
#' @param bbox - a bounding box (see details)
#' @param colors.bg - background color
#' @param colors.land - color for land
#' @param colors.bathym - color for the bathymetry
#' @param alpha.bathym - transparency for the bathymetry
#' @param points.size - size of points, in map units
#'
#' @return - basemap layer based on the tmap package
#'
#' @details The final coordinate reference system (\code{final.crs}) can be any object that
#' can be converted to a [sf::st_crs()] object using [get_crs()].
#'
#' The bounding box (\code{bbox}) can be any object that can be converted
#' to a \code{sf::bbox} using [getBBox()].
#'
#' @import magrittr
#' @import tmap
#'
#' @md
#'
#' @export
#'
tmap_CreateBasemap<-function(layer.land=getPackagedLayer("Alaska"),
                             layer.bathym=getPackagedLayer("ShelfBathymetry"),
                             final.crs=get_crs("WGS84"),
                             bbox=getStandardBBox("EBS"),
                             colors.bg="white",
                             colors.land="grey85",
                             colors.bathym="darkblue",
                             alpha.bathym=1.0,
                             points.size=0.01
                            ){
  #make sure final.crs is a sf::crs object
  final.crs<-get_crs(final.crs);

  land<-layer.land;
  if (!is.null(final.crs)) land <- transformCRS(land,final.crs);

  bathym<-layer.bathym;
  if (!is.null(final.crs)) bathym <- transformCRS(bathym,final.crs);

  #define bounding box for map extent
  if (is.null(bbox)) {
    bbext<-getBBox(land);#--no bbox, so use extent of land
  } else {
    bbext<-getBBox(bbox);#--use bbox
    #--make sure bbox is is same crs as land
    crs_land<-get_crs(land);
    if (crs_land!=get_crs(bbext)) bbext<-transformBBox(bbext,crs_land);
  }

  #basemap using projection to final.crs
  basemap<-tmap::tm_shape(land,bbox=bbext,is.master=TRUE,projection=final.crs)+
             tmap::tm_fill(col=colors.land)+tmap::tm_layout(bg.color=colors.bg);
  if (!is.null(bathym))
      basemap <- basemap + tmap::tm_shape(bathym) + tmap::tm_lines(col=colors.bathym,
                                                                   alpha=alpha.bathym);

  return(basemap);
}
