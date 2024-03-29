% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DB_browse_spatial.R
\name{DB_browse_spatial}
\alias{DB_browse_spatial}
\title{Get information on spatial files in the database}
\usage{
DB_browse_spatial(
  path = "default",
  type,
  location = NULL,
  description = NULL,
  parameter = NULL
)
}
\arguments{
\item{path}{The path to the database, passed to \code{\link[=hydroConnect]{hydroConnect()}}. Default uses hydroConnect default path.}

\item{type}{'polygon' or 'raster'?}

\item{location}{If specifying type 'polygon', narrow by associated location(s). Specify multiple locations as a single character vector. Note that location can be NULL in the database for spatial entries and that NULL entries will not be returned when specifying locations.}

\item{description}{Narrow by polygon description(s) if you wish. Specify multiple descriptions as a single character vector. These should be standardized to only a few names. To view description options, run this function with all default parameters perform a unique() operation on the description column.}

\item{parameter}{Narrow by parameter(s) if you wish. Specify multiple parameters as a single character vector. Note that this can be NULL for polygons, and that NULLs will not be returned.}
}
\value{
A data.frame containing the database entries for polygons and/or rasters (NOT the spatial files themselves - get these using DB_get_spatial using the rowid column).
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Wondering what's in the database? This function helps you see what spatial files we have, with an eye to helping you create a query for function \code{\link[=DB_get_spatial]{DB_get_spatial()}}. Leaving all NULL defaults will show you every polygon and raster in the database.
}
\details{
Spatial files are not stored directly in the database but rather in folders situated alongside the database. The database stores the file description and other identifiers, as well as the path to the file.
}
\seealso{
\code{\link[=DB_get_spatial]{DB_get_spatial()}} if you want the spatial layer.
}
