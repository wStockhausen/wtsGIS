#'
#' @title Merge a dataframe with a geometry layer
#'
#' @description This function merges a dataset
#'
#' @details
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
tmap.MergeDataframeWithLayer<-function(dfr,
                                       geoms,
                                       dataID="STATION_ID",
                                       geomsID="GIS_STATION",
                                       allData=FALSE,
                                       duplicateGeoms=TRUE){
  #join annual dfr to geoms by matching ID values
  if (inherits(geoms,c("sf","sfc","sfg"))){
    # str<-paste("geoms are simple features of class",class(geoms),"\n");
    # str<-c(str,
    #        "wtsGIS::tmap.MergeDataframeWithLayer() not yet implemented for simple features (sf) geometries.\n");
    # stop(str);
    dr.geoms<-sf::left_join(dfr,geoms,
                            by=c(dataID=geomsID));
  } else {
    #use sp functions
    dfr.geoms<-sp::merge(geoms,dfr,
                        by.x=dataID,
                        by.y=geomsID,
                        all.x=allData,
                        duplicateGeoms=duplicateGeoms);
    if ("polygons" %in% slotNames(geoms)){
      #assign unique FIDs to polygons (merge above does not assign unique IDs to duplicate geoms)
      dfr.geoms<-sp::spChFIDs(dfr.geoms,
                              as.character(1:length(dfr.geoms@polygons)));
    }
  }
  return(dfr.geoms);
}
