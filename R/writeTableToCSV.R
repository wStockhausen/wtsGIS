#'
#' @title Write a table (without geometry) as a csv file
#'
#' @description Function to write a table (without geometry) as a csv file.
#'
#' @param tbl - table (sf, tibble, or dataframe object)
#' @param file - file name
#'
#' @return none
#'
#' @details Requires \code{sf} package. If \code{tbl} is a
#' \code{sf} object (i.e., with geometry information), the
#' geometry information is dropped.
#'
#' @export
#'
writeTableToCSV<-function(tbl,
                           file="table.csv"){
  if (inherits(tbl,"sf")){
    write.csv(sf::st_drop_geometry(tbl),file=file,row.names=FALSE);
  } else {
    write.csv(tbl,file=file,row.names=FALSE);
  }
}
