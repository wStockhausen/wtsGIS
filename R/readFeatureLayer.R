#'
#' @title Create a sf dataframe from a (vector) feature layer
#'
#' @description This function reads a (vector) feature layer from a dsn and creates a
#' simple features dataframe (using package \code{sf}).
#'
#' @param dsn -data source name (interpretation varies by driver)
#' @param layer - feature layer to read (interpretation varies by driver)
#' @param drivers - RGDAL drivers to use to read feature class (limited set of short names of drivers to try; default is to try all)
#' @param crs - a \code{sf::crs} object, EPSG code, or string describing the output coordinate reference system (or NULL to keep the original)
#'
#' @return a \code{sf::sf} data.frame.
#'
#' @details The function uses \code{sf::st_read} to read the feature layer from the dsn.
#' If \code{crs} is NULL, the coordinate reference system for the layer is unchanged.
#' to it.
#'
#' If the layer did not have a coordinate reference system originally associated with it, then
#' \code{crs} is used to **set** the \code{sf} object's coordinate reference system.
#'
#' If the layer has a coordinate reference system associated with it,
#' \code{wtsGIS::transformCRS} is used to **transform/project** the \code{sf} object
#' to that represented by \code{crs}
#'
#' @importFrom sf st_crs
#' @importFrom sf st_read
#'
#' @export
#'
readFeatureLayer<-function(dsn,
                           layer,
                           drivers=character(0),
                           crs=NULL){
    if (!(dir.exists(dsn)|file.exists(dsn))) {
        warning(paste0("DSN '",file,"' could not be found. Returning NULL."),immediate.=TRUE);
        return(NULL);
    }

    sfdfr<-sf::st_read(dsn=dsn,
                       layer=layer,
                       drivers=drivers,
                       stringsAsFactors=FALSE);

    if (is.null(sfdfr)) {
        warning(paste0("DSN and layer '",file.path(dsn,layer),"' could not be read. Returning NULL."),immediate.=TRUE);
        return(NULL);
    }
    if (!is.null(crs)){
        if (is.null(sf::st_crs(sfdfr))) {
            sf::st_crs(sfdfr)<-get_crs(crs);
        } else {
            sfdfr<-transformCRS(sfdfr,crs);
        }
    }
    return(sfdfr);
}
