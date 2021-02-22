#'
#' @title Determine the name of the active geometry column in an \pkg{sf} dataframe
#'
#' @description Function to determine the name of the active geometry column in an \pkg{sf} dataframe.
#'
#' @param sf_dfr - \pkg{sf} dataframe for which to determine the name of the active geometry column
#'
#' @return vector of column names
#'
#' @details none.
#'
#' @export
#'
getActiveGeometryColumnName<-function(sf_dfr){
  col = attr(sf_dfr,"sf_column");
  return(col);
}
