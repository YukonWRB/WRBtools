#' Plots and tabular data for hydrometric locations
#'
#' This function is intended to facilitate the reporting of hydrology data by compiling basic statistics (years of record, months of operation, min, max, etc.), trend information (Mann-Kendall direction and p-value, Sen's slope), and creating simple plots of level (for lakes) or flow for all requested locations. At its most basic (parameters to FALSE or NULL where applicable), the result is a list of two data.frames to the R environment with location metadata and field measurements.
#'
#' @param db_path The path to the local hydro database including extension. See WRBtools::hydroConnect for supported database types.
#' @param locations The list of locations requested, as either a vector of location IDs or one of "WRB" (only WRB stations selected), "WSC" (only), or "all". Default "all" fetches all stations.
#' @param save_path The path where the .csv(s) and plots should be saved.
#' @param stats set TRUE if you want basic statistics (mean, min, max) and calculated trends.
#' @param plots Set TRUE if you want plots generated for each location (level or flow).
#' @param quiet Suppresses most messages and warnings.
#'
#' @return A list with four data.frames: location metadata, basic statistics, trend information, and daily measurements is returned to the R environment. In addition, an Excel workbook is saved to the save_path with the four data.frames as tabs, and a new folder created to hold level/flow plots for each station requested.
#' @export

waterInfo <- function(db_path ="//env-fs/env-data/corp/water/Common_GW_SW/Data/database/hydro.sqlite", locations = "all", save_path = "choose", stats = TRUE, plots = TRUE, quiet = FALSE) {

  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the path to the folder where you want this report saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
  }

  hydro <- hydroConnect(path = db_path)
  on.exit(DBI::dbDisconnect(hydro))



} #End of function
