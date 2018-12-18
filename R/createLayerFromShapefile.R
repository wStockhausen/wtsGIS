#'
#' @title Create a spatial layer (an sf or sp object) from a shapefile
#'
#' @description This function reads a shapefile and creates a spatial layer, either as a
#' simple features dataset (using package \code{sf}) or as an sp dataset (using package \code{sp}).
#'
#' @param file - shapefile to read
#' @param strCRS - string describing the output coordinate reference system (CRS) [or NULL to use the original]
#' @param as.sf - flag to create layer as simple features default is TRUE as of 2018.09.28)
#'
#' @return a spatial dataset consistent with the tmap package, either a
#' simple features dataset or an sp dataset depending on whether as.sf is TRUE or FALSE.
#'
#' @details Uses \code{tmaptools::read_shape} to read the shapefile.
#' If requested, uses \code{wtsGIS::transformCRS} to
#' convert the output layer CRS to that represented by strCRS.
#'
#' @export
#'
createLayerFromShapefile<-function(file,
                                   strCRS=getCRS("WGS84"),
                                   as.sf=TRUE){
    if (!file.exists(file)) {
        warning(paste0("Shapefile '",file,"' could not be found. Returning NULL."),immediate.=TRUE);
        return(NULL);
    }
    layer<-tmaptools::read_shape(file=file,stringsAsFactors=FALSE,as.sf=as.sf);
    if (is.null(layer)) {
        warning(paste0("Shapefile '",file,"' could not be read. Returning NULL."),immediate.=TRUE);
        return(NULL);
    }
    if (!is.null(strCRS)) layer<-wtsGIS::transformCRS(layer,strCRS);
    return(layer);
}
