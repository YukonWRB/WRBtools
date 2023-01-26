#' Plots and tabular data for hydrometric locations
#'
#' This function is intended to facilitate the reporting of hydrology data by compiling basic statistics (years of record, months of operation, min, max, etc.), trend information (Mann-Kendall direction and p-value, Sen's slope), and creating simple plots of level (for lakes) or flow for all requested locations. At its most basic (parameters to FALSE or NULL where applicable), the result is a list of two data.frames to the R environment with location metadata and field measurements.
#'
#' @param db_path The path to the local hydro database including extension. Supports .sqlite database at the moment.
#' @param locations The list of locations requested, as a character vector of length n. Default "all" fetches all stations.
#' @param inactive Boolean specifying whether to include inactive stations.
#' @param save_path The path where the .csv(s) and plots should be saved.
#' @param stats set TRUE if you want basic statistics (mean, min, max) and calculated trends.
#' @param plots Set TRUE if you want plots generated for each location (level or flow).
#' @param quiet Suppresses most messages and warnings.
#'
#' @return A list with four data.frames: location metadata, basic statistics, trend information, and daily measurements is returned to the R environment. In addition, an Excel workbook is saved to the save_path with the four data.frames as tabs, and a new folder created to hold level/flow plots for each station requested.
#' @export


#What's the purpose here?
#1. Extract location metadata
#2. Calc basic stats
#     - years of record, months of operation, min, max
#3. Calc trend information
#     - should include means of alerting the user as to which years are used for trends
#4. Make plots of yearly max and min (ideally on same plot)
