#' Data retrieval from EQWin
#'
#' Fetches sample data from the WRB database and returns a list of data frames suitable for modification for plot generation and other comparisons
#'
#' @param EQcode Site code as it appears in EQWin eg. "(LOB)" or "(KNO)"
#' @param stationIDs "all" for all stations(default) OR vector of selected stations as they appear in the EQWin database WITHOUT the EQcode
#' @param paramIDs "all" for all parameters (default) OR vector of selected parameters exactly as they appear in the EQWin database
#' @param dates "all" for all dates (default) OR vector of length 2 of start and end date in format c("YYYY-MM-DD", "YYYY-MM-DD")
#' @param BD Treatment of values below detection limits (0 = Set to zero; 1 = Set to NA; 2 = Set to 0.5(LOD); 3 = Set to sqrt(2)LOD).
#'
#' @return A list of data frames containing sample information and results
#'
#' @export
#'
#' @examples
EQ_fetch <- function(EQcode,
                     stationIDs = "all",
                     paramIDs = "all",
                     dates = "all",
                     BD = 0,
                     apply_standards = TRUE)

  EQcode <- "(LOB)"
  stationIDs <- "all"# Specify a vector of station IDs without the EQWin code (eg. c("GW-4", "GW-5") OR "all")
  paramIDs <- "all" # Specify a vector of parameter IDs exactly as they appear in EQWin (eg. c("Zn-T, Zn'D") OR "all")
  dates <- "all"
  BD <- 1
  apply_standards = TRUE

  # Set a few options (I'll probs remove these)
  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)

  dbpath <- "X:/EQWin/WR/DB/Water Resources.mdb"

  #### Begin EQWin fetch ####
  EQWin <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dbpath))
  on.exit(DBI::dbDisconnect(EQWin))

  # Download stations and filter to user input
  stns <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstns") %>%
                                      subset(select=c("StnId", "StnCode", "StnName", "StnType", "udf_Stn_Status")))
  if(tolower(paste(stationIDs, collapse = "") == "all")){
    stns <- dplyr::filter(stns, grepl(EQcode, StnCode)) %>%
      dplyr::mutate(StnCode = gsub(EQcode, "", StnCode, fixed = TRUE))
  } else {stns <- dplyr::filter(stns, StnCode %in% paste0(EQcode, stationIDs)) %>%
    dplyr::mutate(StnCode = gsub(EQcode, "", StnCode, fixed = TRUE))}

  # Download all samples for specified stations and filter by specified dates
  samps <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqsampls") %>%
                                       subset(select=c("SampleId", "StnId", "CollectDateTime")) %>%
                                       dplyr::filter(StnId %in% stns$StnId))
  if(tolower(paste(dates, collapse = "") != "all")){
    samps <- samps %>%
      dplyr::filter(between(as.Date(CollectDateTime), as.Date(dates[1]), asDate(dates[2])))}

  # Download list of all parameters
  params <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqparams") %>%
                                        subset(select=c("ParamId", "ParamCode", "Units")))

  # Download all results
  results <- DBI::dbGetQuery(EQWin, paste0("SELECT ", paste0('SampleId', ", ", 'ParamId', ", ", 'Result'), " FROM eqdetail WHERE ParamID IN (", paste(params$ParamId, collapse = ", "),") AND SampleId IN (", paste0(samps$SampleId, collapse = ", "), ")"))

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
  sampledata <- merge3 %>%
    dplyr::mutate(Param = paste0(merge3$ParamCode, " (", merge3$Units, ")")) %>%
    dplyr::select(StnCode, CollectDateTime, StnType, Param, Result) %>%
    dplyr::group_by(StnCode, CollectDateTime, StnType, Param) %>%
    dplyr::summarize(Result = mean(as.numeric(Result))) %>%
    tidyr::pivot_wider(id_cols = c("StnCode", "CollectDateTime", "StnType"), names_from = Param, values_from = Result) %>%
    data.table::as.data.table()
  sampledata <- sampledata[with(sampledata, order(StnCode)),]
  rownames(sampledata) <- NULL

  # Download all standards, filter by user choice
  if(apply_standards == TRUE){
    stds <- merge(data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstds") %>%
                                              subset(select=c("StdId", "StdCode", "StdName", "udf_StnGroup"))),
                  data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstdval") %>%
                                              subset(select=c("StdId", "ParamId", "MaxVal", "MinVal"))),
                  by.x = "StdId", by.y = "StdId")
    stds <- dplyr::filter(stds, stds$StdCode %in% select.list(choices = sort(unique(stds$StdCode)),
                                                              title = "Select Standards",
                                                              graphics = TRUE,
                                                              multiple = TRUE)) %>%
      merge(params, by.x ="ParamId", by.y = "ParamId")
    stds <- stds[, c("ParamCode", "ParamId", "StdCode", "StdName", "MaxVal", "MinVal","Units")]
    stds$MaxVal <- stringr::str_remove_all(stds$MaxVal, "=")
  }
  rcalcs$MaxVal <- as.character(rcalcs$MaxVal)
  test <- merge(stds, rcalcs, by.x = "StdCode", by.y = "MaxVal", all.x = T)

  # Extract calculated standards
  std_calcs <- stds %>%
    dplyr::filter(stringr::str_extract(MaxVal, "=*") == "=")

  # Extract by-station data and apply standards
  EQfetch_list <- list()
  for(i in unique(stns$StnCode)){
    list <- list()
      stndata <- sampledata %>%
        dplyr::filter(StnCode == i)
      list[["stndata"]] <- stndata
      list[["stnstd"]] <- stds


    EQfetch_list[[i]] <- list
  }
rcalcs_test <- rcalcs %>%
  dplyr::select(CalcCode, MaxVal)
stds_test <- stds %>%
  dplyr::select(StdCode, MaxVal)

vec <- intersect(stds$MaxVal, as.numeric(stds$MaxVal))
