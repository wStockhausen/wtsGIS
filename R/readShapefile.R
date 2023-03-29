#'
#' @title Create a sf dataframe object from an ESRI shapefile
#'
#' @description This function reads a shapefile and creates a \pkg{sf} spatial dataframe object.
#'
#' @param file - shapefile to read
#' @param crs - an \code{sf::crs} object, EPSG code, or string describing the output coordinate reference system (or NULL to keep the original)
#'
#' @return a \code{sf::sf} data.frame.
#'
#' @details The function uses [sf::st_read()] to read the shapefile.
#' If \code{crs} is NULL, the coordinate reference system for the shapefile is unchanged.
#'
#' If the shapefile did not have a coordinate reference system originally associated with it, then
#' \code{crs} is used to **set** the \code{sf} object's coordinate reference system.
#'
#' If the shapefile has a coordinate reference system associated with it,
#' [wtsGIS::transformCRS()] is used to **transform/project** the \code{sf} object
#' to that represented by \code{crs}
#'
#' @importFrom sf st_read
#'
#' @export
#'
readShapefile<-function(file,
                        crs=NULL){
    if (!file.exists(file)) {
        warning(paste0("Shapefile '",file,"' could not be found. Returning NULL."),immediate.=TRUE);
        return(NULL);
    }
    layer<-sf::st_read(dsn=file,stringsAsFactors=FALSE,quiet=TRUE);

    if (is.null(layer)) {
        warning(paste0("Shapefile '",file,"' could not be read. Returning NULL."),immediate.=TRUE);
        return(NULL);
    }
    if (!is.null(crs)){
        if (is.null(sf::st_crs(layer))) {
            sf::st_crs(layer)<-get_crs(crs);
        } else {
            layer<-transformCRS(layer,crs);
        }
    }
    return(layer);
}
