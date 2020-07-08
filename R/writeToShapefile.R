#'
#' @title Write an object with geometry information as a shapefile
#'
#' @description Function to write an object with geometry information (e.g. an sf dataframe) as a shapefile.
#'
#' @param obj - object with geometry information
#' @param file - file name (default is "shapefile"; no extension is necessary)
#'
#' @return none
#'
#' @details Wrapper for \code{writeToGISDataset} using "ESRI Shapefile" as the driver.
#' Note that column names are limited to 10 characters max.
#'
#' @importFrom stringi stri_sub
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
  if (stringi::stri_sub(file,from=-4,to=-1)!=".shp") file<-paste0(file,".shp");
  if (!dir.exists(folder)) dir.create(folder,recursive=TRUE);
  writeToGISDataset(obj,dsn=file,delete_dsn=TRUE,delete_layer=TRUE,driver="ESRI Shapefile");
  return(invisible(NULL));
}
