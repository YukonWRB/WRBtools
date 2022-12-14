
#' Retrieve HRDPS rasters
#'
#' Utility function to retrieve gridded predictions output from the [HRDPS model](https://weather.gc.ca/grib/grib2_HRDPS_HR_e.html). In current form will delete all old files in the save directory.
#'
#' @param clip The two-digit abbreviation(s) as per [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip
#' @param save_path The path to the directory (folder) where the rasters should be saved. A new sub-directory will be created based on the `param` selected if not already present. Default `"choose"` lets you select your folder (do not choose the one named after the `param`), or enter the path as a character string.
#' @param param The HRDPS parameter you wish to download, from the list of published abbreviations at [https://weather.gc.ca/grib/HRDPS_HR/HRDPS_ps2p5km_PNONZERO_deterministic_e.html](https://weather.gc.ca/grib/HRDPS_HR/HRDPS_ps2p5km_PNONZERO_deterministic_e.html). Defaults to accumulated precipitation at the surface.
#'
#' @return Up to 48 rasters representing the HRDPS modeled output for the parameter selected.
#' @export
#'

getHRDPS <- function(clip = c("YT"),
                     save_path = "choose",
                     param = "APCP_SFC_0")
{
  #Save path
  if (save_path == "choose") {
    print("Select the path to the folder where you want to save the raster(s).")
    save_path <- as.character(utils::choose.dir(caption = "Select Save Folder"))
  }

  suppressWarnings(dir.create(paste0(save_path, "\\", param)))

  save_path <- paste0(save_path, "/", param)

  #NOTE:: models seem to be run 3 hours post and take some time to write files. Use a lag of 4 hours to be realistic.
  current_utc <- Sys.time()
  attr(current_utc, "tzone") <- "UTC"
  latest_run <- as.character(lubridate::floor_date(current_utc - 4*60*60, "6 hours"))
  if (nchar(latest_run) < 13){
    latest_run <- paste0(latest_run, " 00:00")
  }
  issue_timedate <- gsub("-", "", latest_run)
  issue_timedate <- substr(gsub(" ", "", issue_timedate), 1, 10)

  #Delete old files with the same param and NOT the same issue_timedate
  existing <- list.files(save_path, full.names = TRUE)
  keep <- paste0(param, "_", issue_timedate)
  delete <- !grepl(keep, existing)
  file.remove(existing[delete])

  #Make clip polygon
  extent <- paste(clip, collapse="_")
  if (!is.null(clip)){
    clip <- data$prov_buff[data$prov_buff$PREABBR %in% clip, ]
    clip <- terra::vect(clip)
  }

  existing <- list.files(save_path)
  clipped <- FALSE #So that clip happens the first time around
  for (i in 1:48){
    if (nchar(i)==1){ #format i so that it works in creating the url
      i <- paste0("0", i)
    }

    name <- paste0(param, "_", issue_timedate, "_", i, ".tiff")
    if (!(TRUE %in% grepl(name, existing))) { #Checks if the file exists already, runs if not.
      tryCatch({
        # download.file(paste0("https://dd.weather.gc.ca/model_hrdps/continental/grib2/",
        #                      substr(issue_timedate, 9, 10),
        #                      "/0",
        #                      i,
        #                      "/CMC_hrdps_continental_",
        #                      param,
        #                      "_ps2.5km_",
        #                      issue_timedate,
        #                      "_P0",
        #                      i,
        #                      "-00.grib2"),
        #               destfile = paste0(tempdir(), "/HRDPS", i),
        #               method = "curl",
        #               extra = "-k",
        #               quiet = TRUE)
        # raster <- terra::rast(paste0(tempdir(), "/HRDPS", i))
        #The two function calls above are to get around the default download method of terra::rast because of the YG firewall. If the terra::rast line below does not work, make sure that It is not ideal, remember to revert to line below once fixed.
        raster <- terra::rast(paste0("https://dd.weather.gc.ca/model_hrdps/continental/grib2/", substr(issue_timedate, 9, 10), "/0", i, "/CMC_hrdps_continental_", param, "_ps2.5km_", issue_timedate, "_P0", i, "-00.grib2"))

        if (clipped == FALSE){
          if (!is.null(clip)){
            clip <- terra::project(clip, raster) #project clip vector to crs of the raster
          }
          clipped <- TRUE #So that project doesn't happen after the first iteration
        }
        if (!is.null(clip)){
          raster <- terra::mask(raster, clip) #Makes NA values beyond the boundary of clip
          raster <- terra::trim(raster) #Trims the NA values
        }
        terra::writeRaster(raster, paste0(save_path, "/", name), overwrite=TRUE)
        unlink(paste0(tempdir(), "/HRDPS", i))
      }, error = function(e){
        cat(crayon::red(paste0("Fetching rasters failed on file https://dd.weather.gc.ca/model_hrdps/continental/grib2/", substr(issue_timedate, 9, 10), "/0", i, "/CMC_hrdps_continental_", param, "_SFC_0_ps2.5km_", issue_timedate, "_P0", i, "-00.grib2. This is likely temporary, try again once the files have been written to the url.")))
      })
    }
  }
}
