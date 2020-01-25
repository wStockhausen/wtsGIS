#'
#' @title Merge a dataframe with a geometry layer
#'
#' @description This function merges a dataset with a geometry layer.
#'
#' @details If geoms is an sf-like spatial dataset, the merge is conducted using \code{sf::left_join}; otherwise,
#' it is conducted using \code{sp:merge}.
#'
#' @param dfr - dataframe to merge
#' @param geoms - geometries to merge with
#' @param dataID - name of id column in dataframe to join on
#' @param geomsID - name of id column in geometry layer to join on
#' @param allData - flag to merge all rows from dataframe
#' @param duplicateGeoms - allow geoms to be duplicated (i.e., where multiple rows in dataframe maatch a single geom)
#'
#' @return - merged spatial dataframe (based on sf or sp classes, depending on class of geoms)
#'
#' @export
#'
mergeDataframeWithLayer<-function(dfr,
                                   geoms,
                                   dataID="GIS_STATION",
                                   geomsID="STATION_ID",
                                   allData=FALSE,
                                   duplicateGeoms=TRUE){
  #join annual dfr to geoms by matching ID values
  if (inherits(geoms,c("sf","sfc","sfg"))){
    message("merging sf geoms layer using dplyr::right_join");
    byvec<-dataID;
    names(byvec)<-geomsID;
    dfr.geoms<-dplyr::right_join(geoms,dfr,
                                 by=byvec);
  } else {
    #use sp functions
    message("merging sp geoms layer using sp::merge");
    dfr.geoms<-sp::merge(geoms,dfr,
                        by.x=geomsID,
                        by.y=dataID,
                        all.x=allData,
                        duplicateGeoms=duplicateGeoms);
    if ("polygons" %in% methods::slotNames(geoms)){
      #assign unique FIDs to polygons (merge above does not assign unique IDs to duplicate geoms)
      dfr.geoms<-sp::spChFIDs(dfr.geoms,
                              as.character(1:length(dfr.geoms@polygons)));
    }
  }
  return(dfr.geoms);
}
