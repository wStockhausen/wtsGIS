#'
#' @title Create a basemap layer for maps based on the tmap package
#'
#' @description This function creates a basemap layer for maps based on the tmap package.
#'
#' @details The basemap contains a land layer (polygons) and a bathymetry layer (lines).
#' Uses \code{getPackagedLayer} or \code{wtsUtilities::createLayerFromShapefile} to create the
#' land and bathymetric layers. If boundingbox is NULL, then the
#' bounding box for the land layer is used as the bounding box for the basemap.
#'
#' @param layer.land - a tmap layer representing land
#' @param layer.bathym - a tmap layer representing bathymetry
#' @param gisPath - path to top level folder for shapefiles
#' @param shapeFile.land - land shapefile (if layer.land is not provided)
#' @param shapeFile.bathymetry - bathymetry shapefile (if layer.bathym is not provided)
#' @param strCRS.orig - string representation of original CRS (default = WGS84) used for ALL shapefiles
#' @param strCRS.finl - string representation of final CRS (default = WGS84) used for the basemap
#' @param boundingbox - a bounding box (see details)
#' @param colors.bg - background color
#' @param colors.land - color for land
#' @param colors.bathym - color for the bathymetry
#' @param alpha.bathym - transparency for the bathymetry
#' @param points.size - size of points, in map units
#'
#' @return - basemap layer based on the tmap package
#'
#' @details Bounding box options:
#'  1. list with elements bottomleft and topright, each a list with elements lon and lat
#'  2. an sf::bbox object (numeric vector with named elements xmin, ymin, xmax, ymax)
#'  3. a matrix with rows corresponding to x and y, columns to min and max (used by the sp package)
#'  4. a raster::extent object (numeric vector with named elements xmin, xmax, ymin, ymax)
#'
#' @import magrittr
#'
#' @export
#'
createBaseTMap<-function( layer.land=NULL,
                          layer.bathym=NULL,
                          gisPath=NULL,
                          shapeFile.land      =NULL,
                          shapeFile.bathymetry=NULL,
                          strCRS.orig=NULL,
                          strCRS.finl=getCRS("WGS84"),
                          boundingbox=list(bottomleft=list(lon=-179,lat=54),
                                           topright  =list(lon=-157,lat=62.5)),
                          colors.bg="white",
                          colors.land="grey85",
                          colors.bathym="darkblue",
                          alpha.bathym=1.0,
                          points.size=0.01
                          ){

  land<-layer.land;
  if (is.null(land)){
    if (!is.null(shapeFile.land)){
      f<-shapeFile.land;
      if (!is.null(gisPath)) f<-file.path(gisPath,shapeFile.land);
      message(paste0("reading land shapefile '",f,"'"));
      land<-createLayerFromShapefile(f,strCRS=strCRS.orig);
    } else {
      #--use default
      land<-getPackagedLayer("Alaska");
    }
  }
  if (!is.null(strCRS.finl)) land <- wtsGIS::transformCRS(land,strCRS.finl);

  bathym<-layer.bathym;
  if (is.null(bathym)){
    if (!is.null(shapeFile.bathymetry)){
      f<-shapeFile.bathymetry;
      if (!is.null(gisPath)) f<-file.path(gisPath,shapeFile.bathymetry);
      message(paste0("reading bathymetry shapefile '",f,"'"));
      bathym<-createLayerFromShapefile(f,strCRS=strCRS.orig);
    } else {
      bathym<-getPackagedLayer("ShelfBathymetry");
    }
  }
  if (!is.null(strCRS.finl)) bathym <- wtsGIS::transformCRS(bathym,strCRS.finl);


  #define bounding box for map extent
  bbext<-tmaptools::bb(land);#just to get a bounding box as an sf::bbox object
  if (!is.null(boundingbox)){
    if (inherits(boundingbox,"bbox")) {
      bbext<-boundingbox;
    } else if (inherits(boundingbox,"extent")) {
      bbext['xmin']<-boundingbox['xmin'];
      bbext['ymin']<-boundingbox['ymin'];
      bbext['xmax']<-boundingbox['xmax'];
      bbext['ymax']<-boundingbox['ymax'];
    } else if (inherits(boundingbox,"matrix")) {
      bbext['xmin']<-boundingbox[1,1];
      bbext['ymin']<-boundingbox[2,1];
      bbext['xmax']<-boundingbox[1,2];
      bbext['ymax']<-boundingbox[2,2];
    } else if (inherits(boundingbox,"list")) {
      bbext['xmin']<-boundingbox$bottomleft$lon;
      bbext['ymin']<-boundingbox$bottomleft$lat;
      bbext['xmax']<-boundingbox$topright$lon;
      bbext['ymax']<-boundingbox$topright$lat;
    } else {
      msg<-"bounding box format not recognized.";
      stop(msg);
    }
  }

  #basemap using CRS from strCRS
  basemap<-tmap::tm_shape(land,bbox=bbext,is.master=TRUE,projection=strCRS.finl)+
             tmap::tm_fill(col=colors.land)+tmap::tm_layout(bg.color=colors.bg);
  if (!is.null(bathym))
      basemap <- basemap + tmap::tm_shape(bathym) + tmap::tm_lines(col=colors.bathym,
                                                                   alpha=alpha.bathym);

  return(basemap);
}
