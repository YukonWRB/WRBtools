#' Data retrieval from EQWin
#'
#' Fetches sample data from the WRB database and outputs a list of data frames suitable for modification for plot generation and other comparisons
#'
#' @param EQcode Site abbreviation as it appears in EQWin eg. "(LOB)" or "(KNO)"
#' @param stationIDs "all" for all stations(default) OR vector of selected stations exactly as they appear in the EQWin database
#' @param paramIDs "all" for all parameters (default) OR vector of seleted parameters exactly as they appear in the EQWin database
#' @param dates "all" for all dates (default) OR vector of length 2 of start and end date in format "YYYY-MM-DD"
#'
#' @return A list of data frames containing sample information and results
#'
#' @export
#'
#' @examples
EQ_fetch <- function(EQcode,
                     stationIDs,
                     paramIDs = "all",
                     dates = "all"){

EQcode <- "(LOB)"
stationIDs <- "all" # Specify a vector of station IDs without the EQWin code (eg. c("GW-4", "GW-5") OR "all")
paramIDs <- "all" # Specify a vector of parameter IDs exactly as they appear in EQWin (eg. c("Zn-T, Zn'D") OR "all")
dates <- "all"
audit_dates <- c("2022-09-06", "2022-09-13", "2022-09-29")

dbpath <- "X:/EQWin/WR/DB/Water Resources.mdb"

#### Begin EQWin fetch ####
EQWin <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dbpath))
on.exit(DBI::dbDisconnect(EQWin))

# Download data from EQWin
params <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqparams") %>%
                                      subset(select=c("ParamId", "ParamCode", "ParamName", "Units")))
samps <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqsampls") %>%
                                     subset(select=c("SampleId", "StnId", "CollectDateTime")))
stns <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqstns") %>%
                                    subset(select=c("StnId", "StnCode", "StnName", "StnDesc", "StnType", "UTMZone", "Easting", "Northing", "udf_Stn_Status")))

#### Filter and format data into a table ####

# Select desired parameter IDs, or "all"
if(paste(paramIDs, collapse = "") == "all"){
  print("All parameters selected")
} else {
  print(paste0("Parameters Selected: ", paste(paramIDs, collapse = ", ")))
  params <- dplyr::filter(params, ParamCode %in% paramIDs)
}

# Select desired stations
if(paste(stationIDs, collapse = "") == "all"){
  print("All stations selected")
  stns <- dplyr::filter(stns, grepl(EQcode, StnCode))
} else {
  print(paste0("Stations selected: ", paste(stationIDs, collapse = ", ")))
  stns <- dplyr::filter(stns, StnCode %in% stationIDs)
}

# Select desired date range
if(paste(dates, collapse = "") == "all"){
  print("All dates selected")
  dates <- c("1950-01-01", as.character(Sys.Date()))
} else {
  print(paste0("Time period selected: ", paste(dates[1], " - ", dates[2])))
}

# Filter unnecessary rows from samples database
samps <- samps %>%
  dplyr::filter(StnId %in% stns$StnId)

# Extract sample results from EQWin
df <- DBI::dbGetQuery(EQWin, paste0("SELECT ", paste0('SampleId', ", ", 'ParamId', ", ", 'Result'), " FROM eqdetail WHERE ParamID IN (", paste(params$ParamId, collapse = ", "),") AND SampleId IN (", paste0(samps$SampleId, collapse = ", "), ")"))

# Sequentially merge data frames to agglomerate samples
merge1 <- merge(samps, stns, by.x = "StnId", by.y = "StnId")
merge2 <- merge(df, merge1, by.x = "SampleId", by.y = "SampleId")
merge3 <- merge(merge2, params, by.x = "ParamId", by.y = "ParamId")


fulldf <- merge3 %>%
  dplyr::mutate(Param = paste0(merge3$ParamCode, " (", merge3$Units, ")")) %>%
  dplyr::mutate(SmplProv = dplyr::case_when(as.character(as.Date(CollectDateTime)) %in% audit_dates ~ "Audit",
                                            TRUE ~ "License")) %>%
  dplyr::select(c(StnCode, StnDesc, StnType, SmplProv, CollectDateTime, Param, Result))

fulldf <- fulldf[with(fulldf, order(StnCode)),]
rownames(fulldf) <- NULL

rm(merge1, merge2, merge3, params, samps, stns, df)

# Add column for sample source
