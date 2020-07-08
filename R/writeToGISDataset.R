#'
#' @title Write a geospatial object to a GIS dataset
#'
#' @description Function to write a geospatial object (e.g., an sf dataset) to a GIS dataset.
#'
#' @param obj - a geospatial object to convert to GIS dataset
#' @param driver - name of dataset driver (run \code{sf::st_drivers()} to see installed drivers )
#' @param dsn - dsn for output (folder or file, depending on output)
#' @param delete_dsn - flag to delete the dsn before writing
#' @param layer - name of layer written to (assuming dsn has layers), or NULL
#' @param delete_layer - flag to delete the layer before writing
#'
#' @return an invisible NULL.
#'
#' @details Wrapper for functions \code{sf::st_write} for objects of class 'sf' or 'sfc' or
#' \code{rgdal::writeOGR} for objects of class Spatial.
#'
#' @importFrom rgdal writeOGR
#' @importFrom sf st_write
#'
#' @export
#'
writeToGISDataset<-function(
                          obj,
                          driver="ESRI Shapefile",
                          dsn=".",
                          delete_dsn=FALSE,
                          layer=NULL,
                          delete_layer=TRUE){
  if (any(inherits(obj,c("sf","sfc")))){
    sf::st_write(obj,
                 dsn=dsn,
                 layer=layer,
                 driver=driver,
                 delete_dsn=delete_dsn,
                 delete_layer=delete_layer);
  } else {
    rgdal::writeOGR(obj,
                    dsn=dsn,
                    layer=layer,
                    driver=driver,
                    delete_dsn=delete_dsn,
                    delete_layer=delete_layer);
  }
  return(invisible(NULL));
}
