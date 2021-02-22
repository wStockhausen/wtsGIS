#'
#' @title Read a raster file into a \pkg{stars} object
#'
#' @description This function reads a raster from a dsn and creates a
#' stars object (using \code{\link[stars]{read_stars}}).
#'
#' @param dsns - data source(s) name (interpretation varies by driver)
#' @param driver - (optional) RGDAL driver to use to read raster layer
#' @param curvilinear - (optional) length two character vector with names of sub-datasets holding longitude and latitude values for all raster cells
#'
#' @return a \pkg{stars} object.
#'
#' @details The function uses \code{\link[stars]{read_stars}} to read the raster from the dsns.
#'
#' @importFrom stars read_stars
#'
#' @export
#'
readStarsLayer<-function(dsns,
                          driver=character(0),
                          curvilinear=character(0)){
    for (dsn in dsns){
        if (!(dir.exists(dsn)|file.exists(dsn))) {
            warning(paste0("DSN '",file,"' could not be found. Returning NULL."),immediate.=TRUE);
            return(NULL);
        }
    }

    strs<-stars::read_stars(dsns,
                            driver=driver,
                            curvilinear=curvilinear);

    if (is.null(strs)) {
        msg = "The following DSNs could not be read as stars objects:\n";
        for (dsn in dsns) {msg = paste0(msg,"\t'",dsn,"'\n")};
        warning(msg,immediate.=TRUE);
        return(NULL);
    }
    return(strs);
}
