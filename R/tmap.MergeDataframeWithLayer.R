#'
#' @title Merge a dataframe with a geometry layer
#'
#' @description This function merges a dataset
#'
#' @details
#'
#' @param dfr - dataframe to merge
#' @param geoms - tmap geometry layer to merge with
#' @param dataID - name of id column in dataframe to join on
#' @param geomsID - name of id column in geometry layer to join on
#' @param allData - flag to merge all rows from dataframe
#' @param duplicateGeoms - allow geoms to be duplicated (i.e., where multiple rows in dataframe maatch a single geom)
#'
#' @return - merged tmap layer
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
#  #join annual evs to stns.pnts by station
#  evs.pnts<-sp::merge(stns.pnts,evs.csv,
#                    by.x="ID",by.y="GIS_STATION",
#                    all.x=FALSE,duplicateGeoms=TRUE);
#  #assign unique FIDs to polygons (merge above does not assign unique IDs to duplicate geoms)
#  #evs.pnts<-sp::spChFIDs(evs.pnts,as.character(1:length(evs.pnts@coords)));
  return(dfr.geoms);
}