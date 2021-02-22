#'
#' @title Create basemap layers for maps based on the \pkg{ggplot2} package
#'
#' @description This function creates a basemap layer for maps based on the \pkg{ggplot2} package.
#'
#' @details The basemap contains a land layer (polygons) and a bathymetry layer (lines).
#' Uses \code{\link{getPackagedLayer}} or \code{wtsUtilities::readShapefile} to create the
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
#'
#' @return - basemap layer based on the \pkg{ggplot2} package
#'
#' @details The final coordinate reference system (\code{final.crs}) can be any object that
#' can be converted to a \code{sf::crs} object using \code{\link{get_crs}}.
#'
#' The bounding box (\code{bbox}) can be any object that can be converted
#' to a \code{sf::bbox} using \code{\link{getBBox}}.
#'
#' @import magrittr
#' @import tmap
#'
#' @export
#'
gg_CreateBasemapLayers = function(layer.land=getPackagedLayer("Alaska"),
                                 layer.bathym=getPackagedLayer("ShelfBathymetry"),
                                 final.crs=get_crs("WGS84"),
                                 bbox=getStandardBBox("EBS"),
                                 colors.bg="white",
                                 colors.land="grey85",
                                 colors.bathym="darkblue",
                                 alpha.bathym=1.0){

  #make sure final.crs is a sf::crs object
  final.crs = get_crs(final.crs);

  land = layer.land;
  if (!is.null(final.crs)) land  =  transformCRS(land,final.crs);

  #define bounding box for map extent
  if (is.null(bbox)) {
    bbox = getBBox(land);#--no bbox, so use extent of land
  } else {
    bbox = getBBox(bbox);#--use bbox
    #--make sure bbox is is same crs as land
    crs_land = get_crs(land);
    if (crs_land!=get_crs(bbox)) bbox = transformBBox(bbox,crs_land);
  }

  lyr_land = ggplot2::geom_sf(data=land,fill=colors.land,colour=NA,inherit.aes=FALSE);

  #--create bathymetry layer
  lyr_bathym=NULL;
  if (!is.null(layer.bathym)){
    bathym = layer.bathym;
    if (!is.null(final.crs)) bathym  =  transformCRS(bathym,final.crs);
    lyr_bathym  = ggplot2::geom_sf(data=bathym,colour=colors.bathym,alpha=alpha.bathym,inherit.aes=FALSE);
  }

  #--define coordinate scale for default basemap
  map_scale = ggplot2::coord_sf(xlim=c(bbox["xmin"],bbox["xmax"]),
                                ylim=c(bbox["ymin"],bbox["ymax"]),
                                crs=wtsGIS::get_crs(bbox),
                                expand=FALSE,
                                clip="on",
                                default=TRUE);

  #--define theme
  #----define aspect ratio for panels
  asp = NULL; #--let ggplot2 work it out
  if (!st_is_longlat(bbox)) asp = (bbox["ymax"]-bbox["ymin"])/(bbox["xmax"]-bbox["xmin"]);
  #----remove axis titles (necessary when connectivity zone labels are included in map)
  theme = ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         plot.background = ggplot2::element_rect(fill=colors.bg),
                         panel.spacing = grid::unit(0.05,"cm"),
                         aspect.ratio=asp);
  return(list(bathym=lyr_bathym,land=lyr_land,map_scale=map_scale,theme=theme));
}

