#' Plots and tabular data for snow survey locations
#'
#' This function is intended to facilitate the reporting of snow survey data by compiling basic statistics (years of record, missing years, mean, max, etc.), trend information (Mann-Kendall direction and p-value, Sen's slope), and creating simple plots of SWE and depth for all requested stations. At its most basic (parameters to FALSE or NULL where applicable), the result is a list of two data.frames to the R environment with location metadata and field measurements.
#'
#' @param db_path The path to the local Snow Survey database including extension.
#' @param locations The list of locations requested, as a character vector of length n. Default "all" fetches all stations.
#' @param inactive Boolean specifying whether to include inactive stations. For 10AD-SC01 and 09BA-SC02 which require conversion factors due to moved measurement locations, this filter is applied after conversion. Therefore, if set to TRUE while 10AD-SC01B or 09BA-SC02B are active then the returned data will include measurements taken at 10AD-SC01 and 09BA-SC02A under their respective current "sister" locations, with conversion factors applied.
#' @param save_path The path where the .csv(s) and plots should be saved.
#' @param stats set TRUE if you want basic statistics (mean, min, max) and calculated trends.
#' @param plots Set TRUE if you want plots generated, SWE and depth for each location.
#' @param quiet Suppresses most messages and warnings.
#'
#' @return A list with four data.frames: location metadata, basic statistics, trend information, and snow course measurements is returned to the R environment. In addition, an Excel workbook is saved to the save_path with the four data.frames, and a new folder created to hold SWE and depth plots for each station requested.
#' @export
#'

snowInfo <- function(db_path ="X:/Snow/DB/SnowDB.mdb", locations = "all", inactive = FALSE, save_path = "choose", stats = TRUE, plots = TRUE, quiet = FALSE) {

  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the path to the folder where you want this report saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
  }

  snowCon <- snowConnect(path = db_path)
  on.exit(DBI::dbDisconnect(snowCon))

  location_table <- DBI::dbReadTable(snowCon, "SNOW_COURSE")
  if (locations[1] == "all"){
    locations <- location_table
  } else {
    check_locs <- locations[!(locations %in% location_table$SNOW_COURSE_ID)]
    if (length(check_locs) > 0){
      print(paste0("Could not find a record for location ", check_locs, ". Other locations will be returned."))
    }
    locations <- location_table[location_table$SNOW_COURSE_ID %in% locations , ]
  }

  dir.create(paste0(save_path, "/SnowExport_", Sys.Date()))

  #Get the measurements
  meas <- DBI::dbGetQuery(snowCon, paste0("SELECT * FROM SNOW_SAMPLE WHERE SNOW_COURSE_ID IN ('", paste(locations$SNOW_COURSE_ID, collapse = "', '"), "')"))

  #Manipulate/preprocess things a bit
  meas <- meas[which(meas$EXCLUDE_FLG==0),] # OMIT VALUES OF EXCLUDEFLG=1, aka TRUE
  meas$SAMPLE_DATE <- as.Date(meas$SAMPLE_DATE)
  meas$year <- lubridate::year(meas$SAMPLE_DATE)
  meas$month <- lubridate::month(meas$SAMPLE_DATE)

  #Deal with special cases
  if ("09BA-SC02A" %in% locations$SNOW_COURSE_ID & "09BA-SC02B" %in% locations$SNOW_COURSE_ID){
    # Special case (i) Twin Creeks: 09BA-SC02B will take precedence over A from 2016 onwards.
    # Calculate correction factors:
    subset <- meas[meas$SNOW_COURSE_ID %in% c("09BA-SC02A", "09BA-SC02B"),]
    duplicated <- data.frame(table(subset$SAMPLE_DATE))
    duplicated <- duplicated[duplicated$Freq > 1 , ]
    duplicated <- as.Date(as.vector(duplicated$Var1))
    swe_factor <- NULL
    for (i in 1:length(duplicated)){
      a <- subset[subset$SNOW_COURSE_ID == "09BA-SC02A" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
      b <- subset[subset$SNOW_COURSE_ID == "09BA-SC02B" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
      swe_factor[i] <- 1 + (b-a)/a
    }
    depth_factor <- NULL
    for (i in 1:length(duplicated)){
      a <- subset[subset$SNOW_COURSE_ID == "09BA-SC02A" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
      b <- subset[subset$SNOW_COURSE_ID == "09BA-SC02B" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
      depth_factor[i] <- 1 + (b-a)/a
    }
    swe_correction <- mean(swe_factor)
    depth_correction <- mean(depth_factor)

    # Remove 09BA-SC02A values in 2016 and later.
    meas <- meas[!(meas$SNOW_COURSE_ID=="09BA-SC02A" & meas$year >= 2016),]
    # Multiply all 09BA-SC02A values by correction factors:
    meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- swe_correction*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"])
    meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- depth_correction*(meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"])
    # Rename as 09BA-SC02B (A will no longer exist here)
    meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- "09BA-SC02B"
    locations <- locations[!(locations$SNOW_COURSE_ID == "09BA-SC02A") , ]
    corrected <- TRUE
  } else if (("09BA-SC02A" %in% locations$SNOW_COURSE_ID | "09BA-SC02B" %in% locations$SNOW_COURSE_ID ) & !quiet) {
    print("Be careful with stations 09BA-SC02A and B. A is no longer active. When requesting data from both, a correction factor determined by operating the stations in parallel over several years is applied to A, and the result reported as 09BA-SC02B. Since you requested only data from A or B, no correction was applied.")
  }

  if ("10AD-SC01" %in% locations$SNOW_COURSE_ID & "10AD-SC01B" %in% locations$SNOW_COURSE_ID){
    # Special case (ii) Hyland 10AD-SC01 and 10AD-SC01B. B will take precedence over (no letter) from 2018 onwards.
    # Calculate correction factors:
    subset <- meas[meas$SNOW_COURSE_ID %in% c("10AD-SC01", "10AD-SC01B"),]
    duplicated <- data.frame(table(subset$SAMPLE_DATE))
    duplicated <- duplicated[duplicated$Freq > 1 , ]
    duplicated <- as.Date(as.vector(duplicated$Var1))
    swe_factor <- NULL
    for (i in 1:length(duplicated)){
      a <- subset[subset$SNOW_COURSE_ID == "10AD-SC01" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
      b <- subset[subset$SNOW_COURSE_ID == "10AD-SC01B" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
      swe_factor[i] <- 1 + (b-a)/a
    }
    depth_factor <- NULL
    for (i in 1:length(duplicated)){
      a <- subset[subset$SNOW_COURSE_ID == "10AD-SC01" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
      b <- subset[subset$SNOW_COURSE_ID == "10AD-SC01B" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
      depth_factor[i] <- 1 + (b-a)/a
    }
    swe_correction <- mean(swe_factor)
    depth_correction <- mean(depth_factor)

    #Step 1: Remove SC01 blank values in 2018 and later.
    meas <- meas[!(meas$SNOW_COURSE_ID=="10AD-SC01" & meas$year>=2018),]
    #Step 2: Multiply all remaining SC01 values by correction factors:
    meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="10AD-SC01"] <- swe_correction*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="10AD-SC01"])
    meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"] <- depth_correction*(meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"])
    # Step 3: Rename as 010AD-SC01B (blank will no longer exist)
    meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="10AD-SC01"] <- "10AD-SC01B"
    corrected <- TRUE
    locations <- locations[!(locations$SNOW_COURSE_ID == "10AD-SC01") , ]
  } else if (("10AD-SC01" %in% locations$SNOW_COURSE_ID | "10AD-SC01B" %in% locations$SNOW_COURSE_ID) & !quiet) {
    print("Be careful with stations 10AD-SC01 (no letter) and 10AD-SC01B. The first is no longer active. When requesting data from both, a correction factor determined by operating the stations in parallel over several years is applied to the first, and the result reported as 10AD-SC01B. Since you requested only data from (no letter) or B, no correction was applied.")
  }

  if (!inactive){ #Filter out the inactive stations if inactive is FALSE
    inactive <- locations[locations$ACTIVE_FLG==TRUE,]$SNOW_COURSE_ID
    meas <- meas[meas$SNOW_COURSE_ID %in% inactive , ]
    locations <- locations[locations$ACTIVE_FLG == TRUE ,]
  }
  if (corrected & !quiet){
    print("Warning: locations 09BA-SC02B and/or 10AD-SC01B are in fact composites of defunct locations 09BA-SC02A and/or 10AD-SC01. A correction factor (determined by operating locations in parallel over several years) was applied to defunct location data to make it comparable to the new locations.")
  }


  if (stats){
    #Calculate station basic stats: min, max, mean, median, total yrs, gaps
    stats_df <- data.frame()
    for (i in 1:nrow(locations)){
      yrs <- lubridate::year(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$SAMPLE_DATE)
      total_yrs <- max(yrs) - min(yrs)
      gaps <- seq(min(yrs), max(yrs))[!(seq(min(yrs), max(yrs)) %in% yrs)]
      sample_months <- sort(unique(lubridate::month(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$SAMPLE_DATE, label = TRUE, abbr = TRUE)))
      allMaxSWE <- max(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$SNOW_WATER_EQUIV, na.rm=TRUE)
      allMaxDepth <- max(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$DEPTH, na.rm=TRUE)

      depthMaxes <- NULL
      SWEMaxes <- NULL
      for (j in unique(yrs)){
        subset <- meas[meas$year == j & meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i], ]
        months <- unique(subset$month)
        if (3 %in% months & 4 %in% months){
          subsetDepth <- max(subset$DEPTH, na.rm=TRUE)
          subsetSWE <- max(subset$SNOW_WATER_EQUIV, na.rm=TRUE)
          depthMaxes <- c(depthMaxes, subsetDepth)
          SWEMaxes <- c(SWEMaxes, subsetSWE)
        }
      }

      medianMaxDepth <- stats::median(depthMaxes)
      meanMaxDepth <- mean(depthMaxes)
      medianMaxSWE <- stats::median(SWEMaxes)
      meanMaxSWE <- mean(SWEMaxes)

      stats_df <- rbind(stats_df,
                     data.frame("location_ID" = locations$SNOW_COURSE_ID[i],
                                "total_record_yrs" = total_yrs,
                                "start" = min(yrs),
                                "end" = max(yrs),
                                "missing_yrs" = paste(gaps, collapse=", ", sep = ", "),
                                "sample_months" = paste(sample_months, collapse = ", "),
                                "max_SWE" = allMaxSWE,
                                "mean_max_SWE" = meanMaxSWE,
                                "median_max_SWE" = medianMaxSWE,
                                "max_depth" = allMaxDepth,
                                "mean_max_depth" = meanMaxDepth,
                                "median_max_depth" = medianMaxDepth
                     )
      )
    }

    #Calculate trends and significance
    #Calculate the overall trend and p-value
    #1. start with the mean max snowpack for each year (average of the max measurements)
    #2. Calculate stats on that
    #3.
    trends <- data.frame()
    #Calculate same for all locations
    for (i in 1:nrow(locations)){
      yrs <- unique(lubridate::year(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$SAMPLE_DATE))
      AllSWEMax <- numeric(0)
      for (j in yrs){
        AllSWEMax <- c(AllSWEMax, max(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] & lubridate::year(meas$SAMPLE_DATE) == j, ]$SNOW_WATER_EQUIV))
      }
      AllSWEMax <- stats::na.omit(hablar::rationalize(AllSWEMax))
      if (length(AllSWEMax) > 6){
        AllSWESensMax <- trend::sens.slope(AllSWEMax)
      } else {
        AllSWESensMax$estimates <- NA
        AllSWESensMax$p.value <- NA
      }

      AllDepthMax <- numeric(0)
      for (j in yrs){
        AllDepthMax <- c(AllDepthMax, max(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] & lubridate::year(meas$SAMPLE_DATE) == j, ]$DEPTH))
      }
      AllDepthMax <- stats::na.omit(hablar::rationalize(AllDepthMax))
      if(length(AllDepthMax) > 6) {
        AllDepthSensMax <- trend::sens.slope(AllDepthMax)
      } else {
        AllDepthSensMax$estimates <- NA
        AllDepthSensMax$p.value <- NA
      }

      trends <- rbind(trends,
                      data.frame("location_ID" = locations$SNOW_COURSE_ID[i],
                                 "p.value_SWE_max" = round(unname(AllSWESensMax$p.value), 3),
                                 "sens.slope_SWE_max" = round(unname(AllSWESensMax$estimates), 3),
                                 "n_SWE" = AllSWESensMax$parameter,
                                 "p.value_depth_max" = round(unname(AllDepthSensMax$p.value), 3),
                                 "sens.slope_depth_max" = round(unname(AllDepthSensMax$estimates), 3),
                                 "n_depth" = AllDepthSensMax$parameter
                      ))
    }

    if (plots){
      #plot of min, max, mean for each month? Might be helpful to visualize trends? Would be one plot per location...
      #Monthly frequency plot?
      #Create box plot?
      plotsSWE <- list()
      plotsDepth <- list()
      for (i in 1:nrow(locations)){
        plot_meas <- meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]

        plotSWE <- ggplot2::ggplot(data=plot_meas[plot_meas$SNOW_WATER_EQUIV > 0 , ], ggplot2::aes(x = .data$SAMPLE_DATE, y = SNOW_WATER_EQUIV)) +
          ggplot2::labs(x = "Sample date", y = "mm SWE") +
          ggplot2::scale_x_date() +
          ggplot2::geom_point() +
          ggplot2::geom_line(linewidth = 0.1) +
          ggplot2::theme_classic()

        plotDepth <- ggplot2::ggplot(data=plot_meas[plot_meas$DEPTH > 0 , ], ggplot2::aes(x = .data$SAMPLE_DATE, y = DEPTH)) +
          ggplot2::labs(x = "Sample date", y = "mm SWE") +
          ggplot2::scale_x_date() +
          ggplot2::geom_point() +
          ggplot2::geom_line(linewidth = 0.1) +
          ggplot2::theme_classic()

        if (!is.null(save_path)){
          ggplot2::ggsave(filename=paste0(save_path, "/SnowExport_", Sys.Date(), "/plots/", locations$SNOW_COURSE_ID[i], "_SWE.png"), plot=plotSWE, height=8, width=12, units="in", device="png", dpi=500)
          ggplot2::ggsave(filename=paste0(save_path, "/SnowExport_", Sys.Date(), "/plots/", locations$SNOW_COURSE_ID[i], "_DEPTH.png"), plot=plotDepth, height=8, width=12, units="in", device="png", dpi=500)
        }
      }
    }
  }

  locations$LATITUDE_SEC[is.na(locations$LATITUDE_SEC)] <- as.numeric(0)
  latitude <- locations$LATITUDE_DEG + locations$LATITUDE_MIN/60 + locations$LATITUDE_SEC/3600
  locations$LONGITUDE_SEC[is.na(locations$LONGITUDE_SEC)] <- as.numeric(0)
  longitude <- locations$LONGITUDE_DEG + locations$LONGITUDE_MIN/60 + locations$LONGITUDE_SEC/3600
  locations <- locations[ , c("SNOW_COURSE_ID", "SNOW_COURSE_NAME", "ACTIVE_FLG", "ELEVATION")]
  locations <- cbind(locations, latitude, longitude)
  names(locations) <- c("location_ID", "location_name", "active", "elevation_m", "latitude", "longitude")

  if (stats){
    results <- list("locations" = locations, "stats" = stats_df, "trends" = trends, "measurements" = meas)
    if (!is.null(save_path)){
      openxlsx::write.xlsx(results, paste0(save_path, "/SnowExport_", Sys.Date(), "/measurements+stats.xlsx"))
    }
  } else {
    results <- list("locations" = locations, "measurements" = meas)
  }

  return(results)
} #End of function
