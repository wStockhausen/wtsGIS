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
#' @param final.crs - object representing the final (display) coordinate reference system (see details)
#' @param bbox - a bounding box (see details; default is for the EBS)
#' @param colors.bg - background color (default is "white")
#' @param colors.land - color for land (default is "grey85")
#' @param colors.bathym - color for the bathymetry (default is "darkblue")
#' @param alpha.bathym - transparency for the bathymetry (default is 1)
#'
#' @return - basemap layer based on the \pkg{ggplot2} package
#'
#' @details The final coordinate reference system (\code{final.crs}) can be any object that
#' can be converted to a \code{sf::crs} object using \code{\link{get_crs}}.
#'
#' The bounding box (\code{bbox}) can be any object that can be converted
#' to a \code{sf::bbox} using \code{\link{getBBox}}.
#'
#' layer.land and layer.bathym will first be transformed to the bounding box coordinates
#' and cropped to the bounding box. All three will then be converted to the final.crs
#' coordinate system.
#'
#' If the bounding box is NULL, it will be created from the bounding box for
#' layer.land (if not NULL) or layer.bathym (if layer.land is NULL.
#'
#' If final.crs is NULL, it will be set to the CRS for the bounding box.
#'
#' @import magrittr
#' @import ggplot2
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

  #make sure final.crs is a sf::crs object, if it is not NULL
  if (!is.null(final.crs)) final.crs = wtsGIS::get_crs(final.crs);

  #--if bbox is not NULL...
  if (!is.null(bbox)) {
      crs = wtsGIS::get_crs(bbox);
      if (!is.na(crs)){
          #--bbox crs is defined, so if final.crs is NULL, set final.crs to crs
          if (is.null(final.crs)) final.crs=crs;
      } else {
          #--bbox crs is not defined, but if presumed same as final.crs if it is not NULL
          if (!is.null(final.crs)) bbox %<>% sf::st_set_crs(final.crs);
      }
  }

  lyr_land = NULL;
  if (!is.null(layer.land)){
    if (is.null(bbox))      bbox      = wtsGIS::getBBox(layer.land);
    if (is.null(final.crs)) final.crs = wtsGIS::get_crs(bbox);
    land = layer.land %>%
              sf::st_transform(wtsGIS::get_crs(bbox)) %>%  #--transform into bbox coordinates
              wtsGIS::cropFeaturesToBBox(bbox) %>%         #--crop to features to bbox
              sf::st_transform(final.crs);                 #--transform to final crs
    lyr_land = ggplot2::geom_sf(data=land,fill=colors.land,colour=NA,inherit.aes=FALSE);
  }

  #--create bathymetry layer
  lyr_bathym=NULL;
  if (!is.null(layer.bathym)){
    if (is.null(bbox))       bbox      = wtsGIS::getBBox(layer.bathym);
    if (is.null(final.crs))  final.crs = wtsGIS::get_crs(bbox);
    bathym = layer.bathym %>%
              sf::st_transform(wtsGIS::get_crs(bbox)) %>%  #--transform into bbox coordinates
              wtsGIS::cropFeaturesToBBox(bbox) %>%         #--crop to features to bbox
              sf::st_transform(final.crs);                 #--transform to final crs
    lyr_bathym  = ggplot2::geom_sf(data=bathym,colour=colors.bathym,alpha=alpha.bathym,inherit.aes=FALSE);
  }

  bbox %<>% wtsGIS::transformBBox(final.crs);#--now transform bbox to final.crs

  #--define coordinate scale for default basemap
  map_scale = ggplot2::coord_sf(xlim=c(bbox["xmin"],bbox["xmax"]),
                                ylim=c(bbox["ymin"],bbox["ymax"]),
                                crs=final.crs,
                                expand=FALSE,
                                clip="on",
                                default=TRUE);

  #--define theme
  #----define aspect ratio for panels
  asp = NULL; #--let ggplot2 work it out
  if (!sf::st_is_longlat(bbox)) asp = (bbox["ymax"]-bbox["ymin"])/(bbox["xmax"]-bbox["xmin"]);
  #----remove axis titles (necessary when connectivity zone labels are included in map)
  theme = ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         plot.background = ggplot2::element_rect(colour=NA,fill=colors.bg),
                         panel.background=element_blank(),
                         panel.border=element_rect(colour="black",fill=NA),
                         panel.spacing = grid::unit(0.05,"cm"),
                         aspect.ratio=asp);
  return(list(bathym=lyr_bathym,land=lyr_land,map_scale=map_scale,theme=theme));
}

