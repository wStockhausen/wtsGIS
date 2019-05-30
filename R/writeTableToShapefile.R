#'
#' @title Write a table (with geometry information) as a shapefile
#'
#' @description Function to write a table (with geometry information) as a shapefile.
#'
#' @param tbl - table object with geometry information (i.e., it inherits from \code{sf})
#' @param file - file name
#'
#' @return none
#'
#' @details Wrapper for \code{tmaptools::write_shape(tbl,file=file)}.
#'
#' @export
#'
writeTableToShapefile<-function(tbl,
                                 file="table.shp"){
  tmaptools::write_shape(tbl,file=file);
}
