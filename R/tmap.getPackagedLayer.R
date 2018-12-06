#'
#' @title Get a layer stored with the package
#'
#' @description Function to get a layer stored with this package.
#'
#' @param layerName - name of layer to pull out
#' @param as.sf - TRUE/FALSE to return dataset as an sf or sp type
#'
#' @return an sf or sp dataset
#'
#' @details Possible layers to pull out are:\cr
#'  "EBS_StationLabels"         ("ebsstationlabels.shp")
#'  "EBS_SurveyBlocks"          ("NMFS_EBSSurveyBlocks.shp")
#'  "EBS_SurveyStations"        ("NMFS_EBSSurveyStations.PointsLL.shp")
#'  "EBS_SurveyStrata"          ("SurveyStrata_StandardArea.shp")
#'  "NBS_SurveyBlocks"          ("northern_blocks.shp")
#'  "NBS_SurveyStrata"          ("SurveyStrata_NBSArea.shp")
#'  "NW_SurveyStrata"           ("SurveyStrata_NWArea.shp")
#'  "BSAI_StatAreas"            ("NMFS_Statistical_Areas/BSAI.shp")
#'  "ADFG_StatAreas"            ("ADFG_Statistical_Areas/ADFG_StatAreas_EBS.shp")
#'  "Alaska"                    ("Land/Alaska.shp")
#'  "ShelfBathymetry"           ("Bathymetry/ShelfBathymetry.shp")
#'  "HCA_PribilofIslands"       ("Conservation_Areas/pribilof_hca.shp")
#'
#' @export
#'
tmap.getPackagedLayer<-function(layerName,
                              as.sf=TRUE){
  org<-options(stringsAsFactors=FALSE);
  on.exit(options(stringsAsFactors=org));
  layerNames<-rbind(data.frame(name="EBS_StationLabels", shp="NMFS_Survey_Info/ebsstationlabels.shp"),
                    data.frame(name="EBS_SurveyBlocks",  shp="NMFS_Survey_Info/NMFS_EBSSurveyBlocks.shp"),
                    data.frame(name="EBS_SurveyStations",shp="NMFS_Survey_Info/NMFS_EBSSurveyStations.PointsLL.shp"),
                    data.frame(name="EBS_SurveyStrata",  shp="NMFS_Survey_Info/SurveyStrata_StandardArea.shp"),
                    data.frame(name="NBS_SurveyBlocks",  shp="NMFS_Survey_Info/northern_blocks.shp"),
                    data.frame(name="NBS_SurveyStrata",  shp="NMFS_Survey_Info/SurveyStrata_NBSArea.shp"),
                    data.frame(name="NW_SurveyStrata",   shp="NMFS_Survey_Info/SurveyStrata_NWArea.shp"),
                    data.frame(name="BSAI_StatAreas",    shp="NMFS_Statistical_Areas/BSAI.shp"),
                    data.frame(name="ADFG_StatAreas",    shp="ADFG_Statistical_Areas/ADFG_StatAreas_EBS.shp"),
                    data.frame(name="Alaska",            shp="Land/Alaska.shp"),
                    data.frame(name="ShelfBathymetry",    shp="Bathymetry/ShelfBathymetry.shp"),
                    data.frame(name="HCA_PribilofIslands",shp="Conservation_Areas/pribilof_hca.shp")
                );
  shpFile<-layerNames$shp[layerName==layerNames$name];
  if (is.null(shpFile)|(shpFile=="")) {
    warning(paste0("Shapefile associated with layer name '",layerName,"' is not defined. Returning NULL"),immediate.=TRUE);
    return(NULL);
  }
  message(paste0("selected shape file name = '",shpFile,"'"));
  fn<-system.file(file.path("extdata/Shapefiles",shpFile),package="wtsGIS");
  message(paste0("Creating dataset from '",fn,"'"));
  shp<-tmap.CreateLayerFromShapefile(fn,
                                     strCRS=NULL,
                                     as.sf=as.sf);
  return(shp);
}
