#'
#' @title Create a basemap layer for maps based on the tmap package
#'
#' @description This function creates a basemap layer for maps based on the tmap package.
#'
#' @details The basemap contains a land layer (polygons) and a bathymetry layer (lines).
#' Uses \code{wtsUtilities::tmap.CreateLayerFromShapefile}.
#'
#' @param layer.land - a tmap layer representing land
#' @param layer.bathym - a tmap layer representing bathymetry
#' @param gisDir - path to top level folder for shapefiles
#' @param shapeFile.land - land shapefile (if layer.land is not provided)
#' @param shapeFile.bathymetry - bathymetry shapefile (if layer.bathym is not provided)
#' @param strCRS - string representation of CRS (default = WGS84) used for ALL shapefiles
#' @param boundingbox - a tmap-style bounding box
#' @param colors.bathym - color for the bathymetry
#' @param points.size - size of points, in map units
#' @param verbose - flag to print debugging info
#'
#' @return - basemap layer based on the tmap package
#'
#' @export
#'
tmap.CreateBaseMap<-function( layer.land=NULL,
                              layer.bathym=NULL,
                              gisDir=NULL,
                              shapeFile.land      =system.file("extdata/Shapefiles/Land/Alaska.shp",package="wtsGIS"),
                              shapeFile.bathymetry=system.file("extdata/Shapefiles/Bathymetry/ShelfBathymetry.shp",package="wtsGIS"),
                              strCRS=tmaptools::get_proj4("longlat",output="character"),
                              boundingbox=list(bottomleft=list(lon=-179,lat=54),
                                               topright  =list(lon=-157,lat=62.5)),
                              colors.bathym="darkblue",
                              points.size=0.01,
                              verbose=TRUE
                              ){

  land<-layer.land;
  if (is.null(land)){
    if (!is.null(shapeFile.land)){
      f<-shapeFile.land;
      if (!is.null(gisDir)) f<-file.path(gisDir,shapeFile.land);
      if (verbose) cat("reading land shapefile '",f,"'\n",sep="")
      land<-tmap.CreateLayerFromShapefile(f,strCRS=strCRS);
    }
  }

  bathym<-layer.bathym;
  if (is.null(bathym)){
    if (!is.null(shapeFile.bathymetry)){
      f<-shapeFile.bathymetry;
      if (!is.null(gisDir)) f<-file.path(gisDir,shapeFile.bathymetry);
      if (verbose) cat("reading bathymetry shapefile '",f,"'\n",sep="")
      bathym<-tmap.CreateLayerFromShapefile(f,strCRS=strCRS);
    }
  }


  #define bounding box for map extent
  bbext<-tmaptools::bb(land);#just to get a bounding box
  bbext['xmin']<-boundingbox$bottomleft$lon;
  bbext['ymin']<-boundingbox$bottomleft$lat;
  bbext['xmax']<-boundingbox$topright$lon;
  bbext['ymax']<-boundingbox$topright$lat;

  #basemap using CRS from strCRS
  basemap<-tmap::tm_shape(land,bbox=bbext,is.master=TRUE)+tmap::tm_fill();
  if (!is.null(bathym))
      basemap <- basemap + tmap::tm_shape(bathym)+tmap::tm_lines(col=colors.bathym);

  return(basemap);
}
