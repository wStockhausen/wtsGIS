% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_to_getCoordinateReferenceSystemInfo.R
\name{get_spCRS}
\alias{get_spCRS}
\title{Get coordinate reference system representation as sp::CRS object}
\usage{
get_spCRS(x)
}
\arguments{
\item{x}{\itemize{
\item see details
}}
}
\value{
a sp::CRS object or NULL if \code{x} is NULL
}
\description{
Function to get coordinate reference system representation as a sp::CRS object
}
\details{
\code{x} can be NULL, NA, numeric, character,
inherit from sf classes crs, sf, sfc, or sfg, or
inherit from sp classes CRS or Spatial.

If \code{x} is numeric, it should represent an EPSG code.

If \code{x} is character, it should be a valid PROJ4 string or one of the following:
"longlat","latlong","WGS84","NAD83","NAD27", "AlaskaAlbers", "utmXX" or "utmXXs". In the
latter two cases, XX should be a valid utm zone.

Based on deprecated function \code{tmaptools::get_proj4} by M. Tennekes
(see https://github.com/mtennekes/oldtmaptools/blob/master/R/get_proj4_code.R).
}
