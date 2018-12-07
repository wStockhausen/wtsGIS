#'
#' @title Transform a spatial dataset to a different coordinate reference system
#'
#' @description Function to transform a spatial dataset to a different coordinate reference system.
#'
#' @param layer - spatial data layer to transform
#' @param strCRS - string representation of final CRS
#'
#' @return spatial layer of same type as input (sf or sp)
#'
#' @details Uses \code{tmaptools::set_projection}
#'
#' @export
#'
transformCRS<-function(layer,strCRS){
    if (!is.null(strCRS)) layer<-tmaptools::set_projection(layer,projection=strCRS);
    return(layer);
}
