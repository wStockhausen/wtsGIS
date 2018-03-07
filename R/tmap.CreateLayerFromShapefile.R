#'
#' @title Create a tmap layer from a shapefile
#' 
#' @description This function reads a shapefile and creates a tmap layer.
#' 
#' @param file - shapefile to read
#' @param strCRS - string describing the final coordinate reference system (CRS)
#' 
#' @return a map layer consistent with the tmap package
#' 
#' @details None.
#' 
#' @export
#' 
tmap.CreateLayerFromShapefile<-function(file,
                                        strCRS=tmaptools::get_proj4("longlat")){
    layer<-tmaptools::read_shape(file=file,stringsAsFactors=FALSE)
    layer<-sp::spTransform(layer,strCRS);#convert CRS to strCRS
    return(layer);
}