#'
#' @title Create an sf (simple features) dataset with point geometries
#'
#' @description Function to create an sf (simple features) dataset with point geometries.
#'
#' @param dfr - dataframe with columns for x,y coordinates for points
#' @param xCol - name of column with x coordinates (e.g., longitudes)
#' @param yCol - name of column with y coordinates (e.g., latitudes)
#' @param strCRS - proj4string representation of a coordinate reference system
#'
#' @return an sf dataframe with "geom" column appended to dfr
#'
#' @details Uses packages \code{tibble}, \code{sf}, \code{dplyr}, \code{tmaptools}.
#'
#' @export
#'
createSFDataset_points<-function(dfr,
                                 xCol="longitude",
                                 yCol="latitude",
                                 strCRS=tmaptools::get_proj4("longlat",output="character")){
  #--create point geometries
  geoms<-createSFC_points(dfr[[xCol]],dfr[[yCol]]);

  #--add point geometries to dfr as column "geom"
  dfr <- dplyr::bind_cols(tibble::as.tibble(dfr),
                          sf::st_sf(geom=geoms,
                                    crs=strCRS));

  #--convert to sf object
  sf_dfr <- sf::st_sf(dfr);

  return(sf_dfr)
}
