#' Data retrieval from EQWin
#'
#' Fetches sample data from the WRB database and outputs a list of data frames suitable for modification for plot generation and other comparisons
#'
#' @param EQcode Site abbreviation as it appears in EQWin eg. "(LOB)" or "(KNO)"
#' @param stationIDs "all" for all stations(default) OR vector of selected stations as they appear in the EQWin database WITHOUT the EQcode
#' @param paramIDs "all" for all parameters (default) OR vector of selected parameters exactly as they appear in the EQWin database
#' @param dates "all" for all dates (default) OR vector of length 2 of start and end date in format c("YYYY-MM-DD", "YYYY-MM-DD")
#'
#' @return A list of data frames containing sample information and results
#'
#' @export
#'
#' @examples
EQ_fetch <- function(EQcode,
                     stationIDs = "all",
                     paramIDs = "all",
                     dates = "all"){

  EQcode <- "(LOB)"
  stationIDs <- c("all")# Specify a vector of station IDs without the EQWin code (eg. c("GW-4", "GW-5") OR "all")
  paramIDs <- "all" # Specify a vector of parameter IDs exactly as they appear in EQWin (eg. c("Zn-T, Zn'D") OR "all")
  dates <- "all"
  audit_dates <- c("2022-09-06", "2022-09-13", "2022-09-29")

  dbpath <- "X:/EQWin/WR/DB/Water Resources.mdb"

  #### Begin EQWin fetch ####
  EQWin <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dbpath))
  on.exit(DBI::dbDisconnect(EQWin))

  # Download stations and filter to user input
  stns <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstns") %>%
                                      subset(select=c("StnId", "StnCode", "StnName", "StnType", "udf_Stn_Status")))
  if(paste(stationIDs, collapse = "") == "all"){
    stns <- dplyr::filter(stns, grepl(EQcode, StnCode))
  } else {stns <- dplyr::filter(stns, StnCode %in% paste0(EQcode, stationIDs))}

  # Download all samples for specified stations and filter by specified dates
  samps <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqsampls") %>%
                                       subset(select=c("SampleId", "StnId", "CollectDateTime")) %>%
                                       dplyr::filter(StnId %in% stns$StnId))
  if(paste(dates, collapse = "") != "all"){
    samps <- samps %>%
      dplyr::filter(between(as.Date(CollectDateTime), as.Date(dates[1]), asDate(dates[2])))}

  # Download list of all parameters
  params <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqparams") %>%
                                        subset(select=c("ParamId", "ParamCode", "ParamName", "Units")))

  # Download all results
  results <- DBI::dbGetQuery(EQWin, paste0("SELECT ", paste0('SampleId', ", ", 'ParamId', ", ", 'Result'), " FROM eqdetail WHERE ParamID IN (", paste(params$ParamId, collapse = ", "),") AND SampleId IN (", paste0(samps$SampleId, collapse = ", "), ")"))

  # Sequentially merge data frames to agglomerate samples
  merge1 <- merge(samps, stns, by.x = "StnId", by.y = "StnId")
  merge2 <- merge(results, merge1, by.x = "SampleId", by.y = "SampleId")
  merge3 <- merge(merge2, params, by.x = "ParamId", by.y = "ParamId")
  fulldf <- merge3 %>%
    dplyr::mutate(Param = paste0(merge3$ParamCode, " (", merge3$Units, ")")) %>%
    dplyr::mutate(SmplProv = dplyr::case_when(as.character(as.Date(CollectDateTime)) %in% audit_dates ~ "Audit",
                                              TRUE ~ "License")) %>%
    dplyr::select(c(StnCode, StnType, SmplProv, CollectDateTime, Param, Result))
  rm(merge1, merge2, merge3)
  fulldf <- tidyr::pivot_wider(fulldf, names_from = Param, values_from = Result)
  fulldf <- fulldf[with(fulldf, order(StnCode)),]
  rownames(fulldf) <- NULL

  # Download all standards, filter by user choice
  stds <- merge(data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstds") %>%
                                            subset(select=c("StdId", "StdCode", "StdName", "udf_StnGroup"))),
                data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstdval") %>%
                                            subset(select=c("StdId", "ParamId", "MaxVal", "MinVal"))),
                by.x = "StdId", by.y = "StdId")
  stds <- merge(stds, params, by.x ="ParamId", by.y = "ParamId")
  stds <- stds[, c("ParamCode", "ParamName", "StdCode", "StdName", "MaxVal", "MinVal","Units")]
  stds <- stds %>%
    dplyr::filter(StdCode %in% select.list(choices = sort(unique(stds$StdCode)),
                                           title = "Select Standards",
                                           graphics = TRUE,
                                           multiple = TRUE))



  calcs <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqcalcs"))

  for(i in 1:nrow(calcs)){
    print(i)
    text <- gsub(" ", "", calcs$CalcScript[i])
    if(length(grep("#Z", text)) == 1){
    calcs$Z[i] <- substr(text, unlist(gregexpr("#Z", text)[1])+3, unlist(gregexpr("#Z", text)[1])+3)
    } else {calcs$Z[i] <- NA}
  }

  for(i in 1:nrow(calcs)){
    print(i)
    text <- gsub(" ", "", calcs$CalcScript[i])
    if(length(grep("#S", text)) == 1){
      calcs$S[i] <- substr(text, unlist(gregexpr("#S", text)[1])+3, unlist(gregexpr("#S", text)[1])+3)
    } else {calcs$S[i] <- NA}
  }

  # Extract sample results from EQWin
  df <- DBI::dbGetQuery(EQWin, paste0("SELECT ", paste0('SampleId', ", ", 'ParamId', ", ", 'Result'), " FROM eqdetail WHERE ParamID IN (", paste(params$ParamId, collapse = ", "),") AND SampleId IN (", paste0(samps$SampleId, collapse = ", "), ")"))



  rm(merge1, merge2, merge3, params, samps, stns, df)

  # Add column for sample source
