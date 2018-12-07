#'
#' @title Get a string representation for a familiar coordinate reference system
#'
#' @description Function to get a string representation for a familiar coordinate reference system.
#'
#' @param name - familiar name of the CRS
#'
#' @return string representation of the CRS, as given by \code{tmaptools::get_proj4}
#'
#' @details Uses \code{tmaptools::get_proj4} to get a string representation of a familiar CRS based on lookup table for
#' the EPSG code.
#' Currently handles "WGS84" (EPSG 4326), "NAD83" (EPSG 4269), and "AlaskaAlbers" (EPSG 3338).
#'
#' @export
#'
getCRS<-function(name){
  org<-options(stringsAsFactors=FALSE);
  on.exit(options(stringsAsFactors=org));
  crsNames<-rbind(data.frame(name="WGS84",        crs=tmaptools::get_proj4(4326,output="character")),
                  data.frame(name="NAD84",        crs=tmaptools::get_proj4(4269,output="character")),
                  data.frame(name="AlaskaAlbers", crs=tmaptools::get_proj4(3338,output="character"))
            );
  crs<-crsNames$crs[name==crsNames$name];
  if (is.null(crs)|(crs=="")) {
    warning(paste0("CRS associated with name '",name,"' is not defined. Returning NULL"),immediate.=TRUE);
    return(NULL);
  }
  return(crs);
}
