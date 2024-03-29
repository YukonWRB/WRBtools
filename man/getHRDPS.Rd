% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getHRDPS.R
\name{getHRDPS}
\alias{getHRDPS}
\title{Download/proces ECCC HRDPS rasters}
\usage{
getHRDPS(clip = c("YT"), save_path = "choose", param = "APCP_Sfc")
}
\arguments{
\item{clip}{The two-digit abbreviation(s) as per \href{https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8}{Canadian Census} for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip}

\item{save_path}{The path to the directory (folder) where the rasters should be saved. A new sub-directory will be created based on the \code{param} selected if not already present. Default \code{"choose"} lets you select your folder (do not choose the one named after the \code{param}), or enter the path as a character string.}

\item{param}{The HRDPS parameter you wish to download. As of 2023-04-15, this list of \href{https://weather.gc.ca/grib/HRDPS_HR/HRDPS_ps2p5km_PNONZERO_deterministic_e.html}{published abbreviations} is out of date, so cross-reference it with those of the \href{https://dd.weather.gc.ca/model_hrdps/continental/2.5km/12/010/}{model outputs}. Defaults to accumulated precipitation at the surface.}
}
\value{
Up to 48 rasters (one per hour) representing the HRDPS modeled output for the parameter selected.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Utility function to retrieve gridded predictions output from the \href{https://weather.gc.ca/grib/grib2_HRDPS_HR_e.html}{HRDPS model}. In current form will delete all old files in the save directory.
}
\seealso{
\code{\link[=getHRDPA]{getHRDPA()}} if looking for precipitation reanalysis rasters instead. For nice precipitation maps and tabular reports of precipitation (past or future), try \code{\link[WRBfloods:basinPrecip]{WRBfloods::basinPrecip()}}.
}
