#'
#' @title Write an object with geometry information as a shapefile
#'
#' @description Function to write an object with geometry information( (e.g. an sf dataframe)) as a shapefile.
#'
#' @param obj - object with geometry information
#' @param file - file name (default is "shapefile"; no extension is necessary)
#'
#' @return none
#'
#' @details Wrapper for \code{sf::st_write(...)} or \code{rgdal::writeOGR(...)}.
#' Note that column names are limited to 10 characters max.
#'
#' @export
#'
writeToShapefile<-function(obj,
                           file="shapefile"){
  if (inherits(obj,"data.frame")){
    if (any(nchar(names(obj))>10)) {
      msg<-"wtsGIS::writeToShapefile: cannot have column names longer than 10 characters.";
      stop(msg);
    }
  }
  folder<-dirname(file);
  layer<-basename(file);
  if (stringi::stri_sub(layer,from=-4,to=-1)==".shp") layer<-stringi::stri_sub(layer,from=1,to=-5);
  if (!dir.exists(folder)) dir.create(folder,recursive=TRUE);
  writeToGISDataset(obj,dsn=folder,layer=layer,driver="ESRI Shapefile",delete_layer=TRUE);
  return(invisible(NULL));
}
