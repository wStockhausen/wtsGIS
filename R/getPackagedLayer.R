#'
#' @title Get a spatial layer stored with this package
#'
#' @description Function to get a spatial layer stored with this package.
#'
#' @param layerName - name of layer to pull out
#' @param as.sf - TRUE/FALSE to return dataset as an sf (TRUE) or sp (FALSE) type
#'
#' @return an sf or sp dataset
#'
#' @details Possible layers to pull out are:
#' \itemize{
#'  \item{"EBS_StationLabels"         ("ebsstationlabels.shp")}
#'  \item{"EBS_SurveyBlocks"          ("NMFS_EBSSurveyBlocks.shp")}
#'  \item{"EBS_SurveyStations"        ("NMFS_EBSSurveyStations.PointsLL.shp")}
#'  \item{"EBS_SurveyStrata"          ("SurveyStrata_StandardArea.shp")}
#'  \item{"NBS_SurveyBlocks"          ("northern_blocks.shp")}
#'  \item{"NBS_SurveyStrata"          ("SurveyStrata_NBSArea.shp")}
#'  \item{"NW_SurveyStrata"           ("SurveyStrata_NWArea.shp")}
#'  \item{"BSAI_StatAreas"            ("NMFS_Statistical_Areas/BSAI.shp")}
#'  \item{"ADFG_StatAreas"            ("ADFG_Statistical_Areas/ADFG_StatAreas_EBS.shp")}
#'  \item{"Alaska"                    ("Land/Alaska.shp")}
#'  \item{"ShelfBathymetry"           ("Bathymetry/ShelfBathymetry.shp")}
#'  \item{"HCA_PribilofIslands"       ("Conservation_Areas/pribilof_hca.shp")}
#' }
#'
#' @export
#'
getPackagedLayer<-function(layerName,
                           as.sf=TRUE){
  layerNames<-rbind(data.frame(name="EBS_StationLabels",  shp="NMFS_Survey_Info/ebsstationlabels.shp",               stringsAsFactors=FALSE),
                    data.frame(name="EBS_SurveyBlocks",   shp="NMFS_Survey_Info/NMFS_EBSSurveyBlocks.shp",           stringsAsFactors=FALSE),
                    data.frame(name="EBS_SurveyStations", shp="NMFS_Survey_Info/NMFS_EBSSurveyStations.PointsLL.shp",stringsAsFactors=FALSE),
                    data.frame(name="EBS_SurveyStrata",   shp="NMFS_Survey_Info/SurveyStrata_StandardArea.shp",      stringsAsFactors=FALSE),
                    data.frame(name="NBS_SurveyBlocks",   shp="NMFS_Survey_Info/northern_blocks.shp",                stringsAsFactors=FALSE),
                    data.frame(name="NBS_SurveyStrata",   shp="NMFS_Survey_Info/SurveyStrata_NBSArea.shp",           stringsAsFactors=FALSE),
                    data.frame(name="NW_SurveyStrata",    shp="NMFS_Survey_Info/SurveyStrata_NWArea.shp",            stringsAsFactors=FALSE),
                    data.frame(name="BSAI_StatAreas",     shp="NMFS_Statistical_Areas/BSAI.shp",                     stringsAsFactors=FALSE),
                    data.frame(name="ADFG_StatAreas",     shp="ADFG_Statistical_Areas/ADFG_StatAreas_EBS.shp",       stringsAsFactors=FALSE),
                    data.frame(name="Alaska",             shp="Land/Alaska.shp",                                     stringsAsFactors=FALSE),
                    data.frame(name="ShelfBathymetry",    shp="Bathymetry/ShelfBathymetry.shp",                      stringsAsFactors=FALSE),
                    data.frame(name="HCA_PribilofIslands",shp="Conservation_Areas/pribilof_hca.shp",                 stringsAsFactors=FALSE),
                stringsAsFactors=FALSE);
  shpFile<-layerNames$shp[layerName==layerNames$name];
  if (length(shpFile)==0||is.null(shpFile)||(shpFile=="")) {
    warning(paste0("Shapefile associated with layer name '",layerName,"' is not defined. Returning NULL"),immediate.=TRUE);
    return(NULL);
  }
  message(paste0("selected shape file name = '",shpFile,"'"));
  fn<-system.file(file.path("extdata/Shapefiles",shpFile),package="wtsGIS");
  message(paste0("Creating dataset from '",fn,"'"));
  shp<-createLayerFromShapefile(fn,
                                strCRS=NULL,
                                as.sf=as.sf);
  return(shp);
}
