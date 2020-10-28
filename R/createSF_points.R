#'
#' @title Create an sf (simple features) dataset with point geometries
#'
#' @description Function to create an sf (simple features) dataset with point geometries.
#'
#' @param dfr - dataframe with columns for x,y coordinates for points
#' @param xCol - name of column with x coordinates (e.g., longitudes)
#' @param yCol - name of column with y coordinates (e.g., latitudes)
#' @param strCRS - proj4string representation of a coordinate reference system
#' @param wrapDateline - flag (T/F) to use 0 to 360 rather than -180 to 180 range for longitudes
#'
#' @return an sf dataframe with "geom" column appended to dfr
#'
#' @details Uses packages \code{tibble}, \code{sf}, \code{dplyr}.
#'
#' @import dplyr
#' @import sf
#' @import tibble
#'
#' @export
#'
createSF_points<-function(dfr,
                          xCol="longitude",
                          yCol="latitude",
                          strCRS=get_proj4string("longlat"),
                          wrapDateline=FALSE){
  #--create point geometries
  geoms<-createSFC_points(dfr[[xCol]],dfr[[yCol]],wrapDateline=wrapDateline);

  #--add point geometries to dfr as column "geom"
  dfr <- dplyr::bind_cols(tibble::as_tibble(dfr),
                          sf::st_sf(geom=geoms,
                                    crs=strCRS));

  #--convert to sf object
  sf_dfr <- sf::st_sf(dfr);

  return(sf_dfr)
}
