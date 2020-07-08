#'
#' @title List the (vector) feature layers in a dsn
#'
#' @description This function creates a list the (vector) feature layers in a dsn.
#'
#' @param dsn - data source name (interpretation varies by driver)
#'
#' @return a named list by layer with sub-lists as elements which have
#' information about the layer.
#'
#' @details None.
#'
#' @importFrom rgdal ogrInfo
#' @importFrom rgdal ogrListLayers
#'
#' @export
#'
listFeatureLayers<-function(dsn){
    lyrs  <- rgdal::ogrListLayers(dsn)
    nlyrs <-length(lyrs);
    lyrInfo<-list();
    for (lyr in lyrs){
        lyrInfo[[lyr]]<-rgdal::ogrInfo(dsn=dsn,layer=lyr);
    }
    return(lyrInfo)
}
