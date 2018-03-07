#'
#' @title Create a raster from a dataframe and a bounding box
#'
#' @description This function creates a raster from a dataframe and a bounding box
#'
#' @details Uses akima::interp(), sp::SpatialPixelsDataFrame(), and raster::raster().
#'
#' @param dfr - spatial dataframe with data to rasterize
#' @param col - name of column in dataframe to rasterize
#' @param bbox - tmap-style bounding box
#' @param nx - number of cells in y direction
#' @param ny - number of cells in y direction
#'
#' @return - raster layer
#'
#' @export
#'
tmap.CreateRasterLayer<-function(dfr,
                            col,
                            bbox,
                            nx=500,
                            ny=500,
                            crs=sp::CRS(tmaptools::get_proj4("longlat")),
                            linear=TRUE,
                            extrap=FALSE,
                            duplicate="error"){
  xgs<-seq(bbox["x","min"],bbext["x","max"],length=500);
  ygs<-seq(bbox["y","min"],bbext["y","max"],length=500);
  intp<-akima::interp(x=dfr,z=col,
                      xo=xgs,yo=ygs,nx=nx,ny=ny,
                      linear=linear,extrap=FALSE,duplicate="error");
  if (is.list(intp)){
    intp2<-as.data.frame(akima::interp2xyz(intp,data.frame=TRUE));
    coords<-as.matrix(intp2[,1:2]);
    intp<-sp::SpatialPixelsDataFrame(coords,intp2[,"z",drop=FALSE],
                                     proj4string=crs)
  }
  rstr<-raster::raster(intp,values=TRUE);
  return(rstr);
}