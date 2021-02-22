#'
#' @title Create a \pkg{raster} spatial raster layer from a dataframe and a bounding box
#'
#' @description This function creates a \pkg{raster} spatial raster layer from a dataframe and a bounding box.
#'
#' @details Uses \code{akima::interp()}, \code{sp::SpatialPixelsDataFrame()}, and \code{raster::raster()}.
#'
#' @note TODO: this function should be revised to work with \code{sf} methods, not \code{sp} methods.
#'
#' @param dfr - spatial dataframe with data to rasterize
#' @param col - name of column in dataframe to rasterize
#' @param bbox - \code{sf}-style bounding box
#' @param nx - number of cells in y direction
#' @param ny - number of cells in y direction
#' @param strCRS - string representation of CRS in PROJ4 format (default yields long/lat in WGS84)
#' @param linear - flag passed to \code{akima::interp}
#' @param extrap - flag passed to \code{akima::interp}
#' @param duplicate - string passed to \code{akima::interp}
#'
#' @return - \pkg{raster} raster layer
#'
#' @importFrom akima interp
#' @importFrom akima interp2xyz
#' @importFrom raster raster
#' @importFrom sp CRS
#' @importFrom sp SpatialPixelsDataFrame
#'
#' @export
#'
createRasterLayer<-function(dfr,
                            col,
                            bbox,
                            nx=500,
                            ny=500,
                            strCRS=get_proj4string("WGS84"),
                            linear=TRUE,
                            extrap=FALSE,
                            duplicate="error"){
  xgs<-seq(bbox["xmin"],bbox["xmax"],length=500);
  ygs<-seq(bbox["ymin"],bbox["ymax"],length=500);
  intp<-akima::interp(x=dfr,z=col,
                      xo=xgs,yo=ygs,nx=nx,ny=ny,
                      linear=linear,extrap=extrap,duplicate=duplicate);
  if (is.list(intp)){
    intp2<-as.data.frame(akima::interp2xyz(intp,data.frame=TRUE));
    coords<-as.matrix(intp2[,1:2]);
    intp<-sp::SpatialPixelsDataFrame(coords,intp2[,"z",drop=FALSE],
                                     proj4string=sp::CRS(strCRS))
  }
  rstr<-raster::raster(intp,values=TRUE);
  return(rstr);
}
