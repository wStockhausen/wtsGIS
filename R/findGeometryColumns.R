#'
#' @title Determine the names of the \pkg{sf} geometry column(s) in a dataframe
#'
#' @description Function to determine the names of the \pkg{sf} geometry column(s) in a dataframe.
#'
#' @param sf_dfr - dataframe for which to determine the names of the geometry column(s)
#' @param verbose - flag to print diagnostic info
#'
#' @return vector of column names
#'
#' @details The input dataframe does not have to be a simple features (\pkg{sf}) dataframe
#' (i.e., one with class = "sf") to contain \pkg{sf} geometry columns. For example, a
#' \pkg{sf} dataframe (class = "sf") with multiple geometry columns on which \code{\link[sf]{st_drop_geometry}} is ca
#' is called will have the "active" geometry column dropped and the resultant class will be
#' "tbl_df", but the other geometry columns (those that were not the active one) will not have been dropped.
#'
#' @export
#'
findGeometryColumns<-function(sf_dfr,
                              verbose=FALSE){
  cols = vector(mode="character",length=0);
  for (nm in names(sf_dfr)){
    if (inherits(sf_dfr[[nm]],"sfc")) {
      cols = c(cols,nm)
      if (verbose) cat("'",nm,"' is a geometry column.\n",sep="");
    }
  }
  return(cols);
}

