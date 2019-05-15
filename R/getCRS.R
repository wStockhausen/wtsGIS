#'
#' @title Get a string representation for a familiar coordinate reference system
#'
#' @description Function to get a string representation for a familiar coordinate reference system.
#'
#' @param name - familiar name of the CRS
#'
#' @return string representation of the CRS, as given by \code{tmaptools::get_proj4(EPSG,output="character")}
#'
#' @details Uses \code{tmaptools::get_proj4} to get a string representation of a
#' familiar CRS based on a lookup table for the EPSG code. The following are currently handled:
#' \itemize{
#'   \item{"AlaskaAlbers" (EPSG 3338)}
#'   \item{"NAD83" (EPSG 4269)}
#'   \item{"WGS84" (EPSG 4326)}
#' }
#'
#' @examples
#' getCRS("AlaskaAlbers")
#'
#' @export
#'
getCRS<-function(name){
  crsNames<-rbind(data.frame(name="WGS84",        crs=tmaptools::get_proj4(4326,output="character"),stringsAsFactors=FALSE),
                  data.frame(name="NAD84",        crs=tmaptools::get_proj4(4269,output="character"),stringsAsFactors=FALSE),
                  data.frame(name="AlaskaAlbers", crs=tmaptools::get_proj4(3338,output="character"),stringsAsFactors=FALSE),
            stringsAsFactors=FALSE);
  crs<-crsNames$crs[name==crsNames$name];
  if (is.null(crs)|(crs=="")) {
    warning(paste0("CRS associated with name '",name,"' is not yet defined. Returning NULL"),immediate.=TRUE);
    return(NULL);
  }
  return(crs);
}
