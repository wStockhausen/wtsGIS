#'
#' @title Merge (join) a dataframe with (to) a geometry layer
#'
#' @description This function merges (joins) a dataset with (to) a geometry layer.
#'
#' @details Uses packages \code{dplyr}, \code{sf} and \code{sp}.
#' If geoms is an sp dataset, the merge is conducted using \code{sp:merge}.
#' If geoms is an sf dataset, it is conducted using one of the join functions
#' in package \code{dplyr}. The results of this join will depend on the join type:
#'
#'  - right join: matched rows in \code{dfr},         all rows in \code{geoms}
#'
#'  - left join:      all rows in \code{dfr},     matched rows in \code{geoms}
#'
#'  - inner join: only rows in \code{dfr} matched to \code{geoms}
#'
#'  - full join:  all rows in \code{dfr} and all rows in \code{geoms}
#'
#' The join column will take its name from \code{dataID} in \code{dfr} and will *not* include
#' a column named \code{geomsID}.
#'
#' @param dfr - dataframe to merge
#' @param geoms - geometries to merge with
#' @param dataID - name of id column in dataframe to join on
#' @param geomsID - name of id column in geometry layer to join on
#' @param sfJoinType - join type (dfr to geoms) for sf-like geometries
#' @param spAllData - flag to merge all rows from dataframe with sp-like geometries
#' @param spDuplicateGeoms - allow sp-like geoms to be duplicated (i.e., where multiple rows in dataframe maatch a single geom)
#'
#' @return - sf or sp dataframe (depending on the class of the geoms object).
#'
#' @export
#'
mergeDataframeWithLayer<-function(dfr,
                                   geoms,
                                   dataID="GIS_STATION",
                                   geomsID="STATION_ID",
                                   sfJoinType=c("right join","left join","inner join","full join"),
                                   spAllData=FALSE,
                                   spDuplicateGeoms=TRUE){
  #join annual dfr to geoms by matching ID values
  if (inherits(geoms,c("sf","sfc","sfg"))){
    byvec<-geomsID; names(byvec)<-dataID;
    if (tolower(sfJoinType[1])=="right join"){
      message("merging dfr to sf geoms layer using dplyr::right_join");
      dfr.geoms<-dplyr::right_join(dfr,geoms,
                                   by=byvec);
    } else if (tolower(sfJoinType[1])=="left join"){
      message("merging dfr to sf geoms layer using dplyr::left_join");
      dfr.geoms<-dplyr::left_join(dfr,geoms,
                                   by=byvec);
    } else if (tolower(sfJoinType[1])=="inner join"){
      message("merging dfr to sf geoms layer using dplyr::inner_join");
      dfr.geoms<-dplyr::inner_join(dfr,geoms,
                                   by=byvec);
    } else if (tolower(sfJoinType[1])=="full join"){
      message("merging dfr to sf geoms layer using dplyr::full_join");
      dfr.geoms<-dplyr::full_join(dfr,geoms,
                                   by=byvec);
    } else {
      msg<-paste0("wtsGIS::mergeDataframeWithLayer: sfJoinType '",sfJoinType,"' not recognized.\n");
      stop(msg);
    }
    dfr.geoms<-sf::st_as_sf(dfr.geoms);#make sure its an sf object
  } else {
    #use sp functions
    message("merging sp geoms layer using sp::merge");
    dfr.geoms<-sp::merge(geoms,dfr,
                        by.x=geomsID,
                        by.y=dataID,
                        all.x=spAllData,
                        duplicateGeoms=spDuplicateGeoms);
    if ("polygons" %in% methods::slotNames(geoms)){
      #assign unique FIDs to polygons (merge above does not assign unique IDs to duplicate geoms)
      dfr.geoms<-sp::spChFIDs(dfr.geoms,
                              as.character(1:length(dfr.geoms@polygons)));
    }
  }
  return(dfr.geoms);
}
