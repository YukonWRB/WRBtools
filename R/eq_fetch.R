#' Data retrieval from EQWin
#'
#' Fetches sample data from the WRB database and returns a list of data frames suitable for modification for plot generation and other comparisons
#'
#' @param EQcode Site code as it appears in EQWin eg. "(LOB)" or "(KNO)"
#' @param stationIDs "all" for all stations(default) OR vector of selected stations as they appear in the EQWin database WITHOUT the EQcode
#' @param paramIDs "all" for all parameters (default) OR vector of selected parameters exactly as they appear in the EQWin database
#' @param dates "all" for all dates (default) OR vector of length 2 of start and end date in format c("YYYY-MM-DD", "YYYY-MM-DD")
#' @param BD Treatment of values below detection limits (0 = Set to zero; 1 = Set to NA; 2 = Set to 0.5*(LOD); 3 = Set to sqrt(2)LOD).
#' @param apply_standards TRUE or FALSE, include standards with data
#'
#' @return A list of lists, each one containing 2 data frames with sample data and calculated standards
#'
#' @export

EQ_fetch <- function(EQcode,
                     stationIDs = "all",
                     paramIDs = "all",
                     dates = "all",
                     BD = 1,
                     apply_standards = TRUE){

  # EQcode <- "(KNO)"
  # stationIDs <- "all"# Specify a vector of station IDs without the EQWin code (eg. c("GW-4", "GW-5") OR "all")
  # paramIDs <- "all" # Specify a vector of parameter IDs exactly as they appear in EQWin (eg. c("Zn-T, Zn-D") OR "all")
  # dates <- "all"
  # BD <- 1
  # apply_standards = TRUE

  # Set a few options (I'll probs remove these)
  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)

  dbpath <- "X:/EQWin/WR/DB/Water Resources.mdb"

  #### Begin EQWin fetch ####
  EQWin <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dbpath))
  on.exit(DBI::dbDisconnect(EQWin))

  # Download stations and filter to user input
  eqstns <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstns") %>%
                                        subset(select=c("StnId", "StnCode", "StnName", "StnType", "udf_Stn_Status")))
  if(tolower(paste(stationIDs, collapse = "") == "all")){
    stns <- dplyr::filter(eqstns, grepl(EQcode, StnCode)) %>%
      dplyr::mutate(StnCode = gsub(EQcode, "", StnCode, fixed = TRUE))
  } else {stns <- dplyr::filter(eqstns, StnCode %in% paste0(EQcode, stationIDs)) %>%
    dplyr::mutate(StnCode = gsub(EQcode, "", StnCode, fixed = TRUE))}

  # Download all samples for specified stations, filter by user choice
  eqsampls <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqsampls") %>%
                                          subset(select=c("SampleId", "StnId", "CollectDateTime")) %>%
                                          dplyr::filter(StnId %in% stns$StnId))
  if(tolower(paste(dates, collapse = "") != "all")){
    samps <- eqsampls %>%
      dplyr::filter(between(as.Date(CollectDateTime), as.Date(dates[1]), asDate(dates[2])))
  } else {
    samps <- eqsampls
  }

  # Download list of all parameters, filter to user choice
  eqparams <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqparams") %>%
                                          subset(select=c("ParamId", "ParamCode", "Units")))
  if(tolower(paste(paramIDs, collapse = "") != "all")){
    params <- eqparams %>%
      dplyr::filter(paramIDs)
  } else {
    params <- eqparams
  }

  # Download all results
  print("Fetching sample results")
  results <- DBI::dbGetQuery(EQWin, paste0("SELECT ", paste0('SampleId', ", ", 'ParamId', ", ", 'Result'), " FROM eqdetail WHERE ParamID IN (", paste(eqparams$ParamId, collapse = ", "),") AND SampleId IN (", paste0(eqsampls$SampleId, collapse = ", "), ")"))

  # Deal with values below detection limits according to user choice
  if(BD == 0){
    results[grepl(results$Result, pattern = "<"),] <- 0
    results$Result <- suppressWarnings(as.numeric(results$Result))
  } else if(BD == 1){
    results$Result <- suppressWarnings(as.numeric(results$Result))
  } else if(BD == 2){
    isBD <- grepl("<", results$Result)
    results$Result[isBD] <- round(as.numeric(gsub("<(.*)", "\\1", results$Result[isBD]))/2, digits = 4)
    rm(isBD)
  } else if(BD == 3){
    isBD <- grepl("<", results$Result)
    results$Result[isBD] <- round(as.numeric(gsub("<(.*)", "\\1", results$Result[isBD]))/sqrt(2), digits = 4)
    rm(isBD)
  }

  # Sequentially merge data frames to agglomerate samples, pivot to wide format and minor formatting tweaks
  merge1 <- merge(samps, stns, by.x = "StnId", by.y = "StnId")
  merge2 <- merge(results, merge1, by.x = "SampleId", by.y = "SampleId")
  merge3 <- merge(merge2, params, by.x = "ParamId", by.y = "ParamId")
  suppressMessages(sampledata <- merge3 %>%
                     dplyr::mutate(Param = paste0(merge3$ParamCode, " (", merge3$Units, ")")) %>%
                     dplyr::select(StnCode, CollectDateTime, StnType, Param, Result) %>%
                     dplyr::group_by(StnCode, CollectDateTime, StnType, Param) %>%
                     dplyr::summarize(Result = mean(as.numeric(Result))) %>%
                     tidyr::pivot_wider(id_cols = c("StnCode", "CollectDateTime", "StnType"), names_from = Param, values_from = Result) %>%
                     data.table::as.data.table())
  sampledata <<- sampledata[with(sampledata, order(StnCode)), ]
  rm(merge1, merge2, merge3)
  rownames(sampledata) <- NULL

  # Download all standards, filter by user choice via popup window
  if(apply_standards == TRUE){
    print("Processing standards")
    # Extract eqstds and eqstdval tables from access database, merge together by StdId
    stds <- merge(data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstds") %>%
                                              subset(select=c("StdId", "StdCode", "StdName", "udf_StnGroup"))),
                  data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstdval") %>%
                                              subset(select=c("StdId", "ParamId", "MaxVal", "MinVal"))),
                  by.x = "StdId", by.y = "StdId")

    # Filter stds by user choice, merge with parameters to associate standards with parameters by param code
    stds <- dplyr::filter(stds, stds$StdCode %in% select.list(choices = sort(unique(stds$StdCode)),
                                                              title = "Select Standards",
                                                              graphics = TRUE,
                                                              multiple = TRUE)) %>%
      merge(params, by.x ="ParamId", by.y = "ParamId")
    stds <- stds[, c("ParamCode", "ParamId", "StdCode", "StdName", "MaxVal", "MinVal","Units")] # Select relevant columns, reorder

    # Separate calculated from set standards
    std_set <- suppressWarnings(stds %>%
                                  dplyr::mutate_at("MaxVal", as.numeric) %>% # Convert MaxVal to numeric
                                  tidyr::drop_na("MaxVal"))
    std_calc_tmp <- stds %>%
      dplyr::filter(stringr::str_extract(MaxVal, "=*") == "=") # Extract standards with MaxVal value beginning with "=" (calculated standard)
    std_calc_tmp$MaxVal <- stringr::str_remove_all(std_calc_tmp$MaxVal, "=*") # Remove equal sign, leaving MaxVal with values matching values in eqcalcs access table
    std_calc_tmp <<- std_calc_tmp

    # Process calculated standards ##
    std_calcs <- WRBtools::EQ_std_calc(fun_sampledata = sampledata, fun_std_calc = std_calc_tmp)

    # Combine set and calculated standards, order
    stddata <- rbind(std_set, std_calcs)
    stddata <- stddata[order(stddata$ParamId), ]
    rownames(stddata) <- NULL
  }

  # Extract by-station data and station standards, put into by-location list then add list to master EQ_fetch output
  EQ_fetch_list <- list()
  for(i in unique(stns$StnCode)){
    list <- list()
    stndata <- sampledata %>%
      dplyr::filter(StnCode == i)
    list[["stndata"]] <- stndata
    if(apply_standards == TRUE){
      list[["stnstd"]] <- stddata
    }
    EQ_fetch_list[[i]] <- list
  }
  rm(sampledata, std_calc_tmp, envir = .GlobalEnv)
  return(EQ_fetch_list)
}
