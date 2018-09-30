#'
#' @title Create a tmap layer from a shapefile
#'
#' @description This function reads a shapefile and creates a tmap layer, either as a
#' simple features dataset (using package \code{sf}) or as an sp dataset (using package \code{sp}).
#'
#' @param file - shapefile to read
#' @param strCRS - string describing the final coordinate reference system (CRS)
#' @param as.sf - flag to create layer as simple features default is TRUE as of 2018.09.28)
#'
#' @return a spatial dataset consistent with the tmap package, either a
#' simlpe features dataset or an sp dataset depending on whether as.sf is TRUE or FALSE.
#'
#' @details None.
#'
#' @export
#'
tmap.CreateLayerFromShapefile<-function(file,
                                        strCRS=tmaptools::get_proj4("longlat",output="character"),
                                        as.sf=TRUE){
    layer<-tmaptools::read_shape(file=file,stringsAsFactors=FALSE,as.sf=as.sf);
    if (as.sf){
      layer<-layer %>% sf::st_transform(strCRS);
    } else {
      layer<-sp::spTransform(layer,strCRS);#convert CRS to strCRS
    }
    return(layer);
}
