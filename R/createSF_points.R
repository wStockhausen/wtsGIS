#'
#' @title Create an \pkg{sf} (simple features) dataset with point geometries
#'
#' @description Function to create an \pkg{sf} (simple features) dataset with point geometries.
#'
#' @param dfr - dataframe with columns for x,y coordinates for points
#' @param xCol - name of column with x coordinates (e.g., longitudes)
#' @param yCol - name of column with y coordinates (e.g., latitudes)
#' @param crs - a coordinate reference system object convertible to a \code{sf::crs} object using \code{\link{get_crs}}
#' @param wrapDateline - flag (T/F) to use 0 to 360 rather than -180 to 180 range for longitudes
#'
#' @return an \pkg{sf} dataframe: dfr with "geom" column appended
#'
#' @details Uses packages \code{tibble}, \code{sf}, \code{dplyr}.
#'
#' @importFrom dplyr bind_cols
#' @importFrom sf st_sf
#' @importFrom tibble as_tibble
#'
#' @export
#'
createSF_points<-function(dfr,
                          xCol="longitude",
                          yCol="latitude",
                          crs=get_crs(4326),
                          wrapDateline=FALSE){
  #--create point geometries
  #----don't define crs at this point
  geoms<-createSFC_points(dfr[[xCol]],dfr[[yCol]],wrapDateline=wrapDateline);

  #--add point geometries to dfr as column "geom"
  #----define crs now
  dfr <- dplyr::bind_cols(tibble::as_tibble(dfr),
                          sf::st_sf(geom=geoms,
                                    crs=crs));

  #--convert to sf object
  sf_dfr <- sf::st_sf(dfr);

  return(sf_dfr)
}
