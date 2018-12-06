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
#' @param strCRS.finl- string representation of final CRS (default = WGS84) used for the basemap
#' @param boundingbox - a tmap-style bounding box
#' @param colors.bathym - color for the bathymetry
#' @param points.size - size of points, in map units
#'
#' @return - basemap layer based on the tmap package
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
                              strCRS.finl=tmaptools::get_proj4("longlat",output="character"),
                              boundingbox=list(bottomleft=list(lon=-179,lat=54),
                                               topright  =list(lon=-157,lat=62.5)),
                              colors.bathym="darkblue",
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
  if (!is.null(strCRS.finl))
    land <- land %>% tmaptools::set_projection(projection=strCRS.finl);

  bathym<-layer.bathym;
  if (is.null(bathym)){
    if (!is.null(shapeFile.bathymetry)){
      f<-shapeFile.bathymetry;
      if (!is.null(gisPath)) f<-file.path(gisPath,shapeFile.bathymetry);
      if (verbose) cat("reading bathymetry shapefile '",f,"'\n",sep="")
      bathym<-createLayerFromShapefile(f,strCRS=strCRS.orig);
    } else {
      bathym<-getPackagedLayer("ShelfBathymetry");
    }
  }
  if (!is.null(strCRS.finl))
    bathym <- bathym %>% tmaptools::set_projection(projection=strCRS.finl);


  #define bounding box for map extent
  bbext<-tmaptools::bb(land);#just to get a bounding box
  if (!is.null(boundingbox)){
    bbext['xmin']<-boundingbox$bottomleft$lon;
    bbext['ymin']<-boundingbox$bottomleft$lat;
    bbext['xmax']<-boundingbox$topright$lon;
    bbext['ymax']<-boundingbox$topright$lat;
  }

  #basemap using CRS from strCRS
  basemap<-tmap::tm_shape(land,bbox=bbext,is.master=TRUE,projection=strCRS.finl)+
             tmap::tm_fill();
  if (!is.null(bathym))
      basemap <- basemap + tmap::tm_shape(bathym) + tmap::tm_lines(col=colors.bathym);

  return(basemap);
}
