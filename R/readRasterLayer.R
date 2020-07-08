#'
#' @title Create a stars raster from a raster layer
#'
#' @description This function reads a raster layer from a dsn and creates a
#' stars object (using package \code{stars}).
#'
#' @param dsn - data source name (interpretation varies by driver)
#' @param driver - RGDAL drivers to use to read feature class (limited set of short names of drivers to try; default is to try all)
#'
#' @return a \code{stars::stars} object.
#'
#' @details The function uses \code{stars::read_stars} to read the feature layer from the dsn.
#' If \code{crs} is NULL, the coordinate reference system for the layer is unchanged.
#' to it.
#'
#' If the layer did not have a coordinate reference system originally associated with it, then
#' \code{crs} is used to **set** the \code{sf} object's coordinate reference system.
#'
#' If the layere has a coordinate reference system associated with it,
#' \code{wtsGIS::transformCRS} is used to **transform/project** the \code{sf} object
#' to that represented by \code{crs}
#'
#' @importFrom stars read_stars
#'
#' @export
#'
readRasterLayer<-function(dsn,
                          driver=character(0)){
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
