#' Get data from the hydromet database
#'
#' @param path The path to the database, passed to hydroConnect. Default uses hydroConnect default path.
#' @param locations The location code/id(s) for the requested timeseries as a character vector of 1 or more. Refer to DB_get_timeseries if unsure of location code.
#' @param parameter The parameter requested for the timeseries.  Refer to DB_get_timeseries if unsure of parameter spelling.
#' @param frequency One of "daily", "realtime", or "discrete".
#' @param start The start date or datetime of records requested (inclusive). Specify a Date or poSIXCT object, or a character vector of form "2022-01-01" or "2022-01-01 10:10:10". Set before timeseries start to get all records up to the end date/time.
#' @param end The end date or datetime of records requested (inclusive). Format as per 'start'. Set after timeseries end to get all records after the start date/time
#' @param save_path Specify a path here if you want an Excel workbook saved to disk. "choose" lets you interactively choose your folder.
#'
#' @return A list of data frames containing the information requested and, optionally, an Excel workbook saved to disk.
#' @export
#'

DB_get_data <- function(path = "default", locations, parameter, frequency, start, end, save_path = "choose") {

  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the folder where you want this information saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
  }
  if (!dir.exists(save_path)){
    stop("The save path you pointed me to does not exist.")
  }

  if (length(parameter) != 1){
    stop("You can only request data for one parameter at a time.")
  }
  if (length(frequency) != 1){
    stop("You can only request data for one frequency type at a time.")
  }

  DB <- hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(DB), add=TRUE)

  ls <- list()
  for (i in locations){
    data <- DBI::dbGetQuery(DB, paste0("SELECT * FROM '", frequency, "' WHERE location = '", i, "' AND parameter = '", parameter, "' AND ", if(frequency == "daily") "date" else if (frequency == "datetime_UTC") "realtime" else if (frequency == "discrete") "sample_date", " BETWEEN '", start, "' AND '", end, "'"))
    if (nrow(data) > 0){
      ls[[i]] <- data
    }
  }

  if (length(ls) > 0){
    if (!is.null(save_path)){
      openxlsx::write.xlsx(ls, paste0(save_path, "/db_extract_", Sys.Date(), ".xlsx"))
    }
    return(data)
  } else {
    print("No records matched your inputs.")
  }
}
