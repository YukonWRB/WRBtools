#' Download ECCC weather station data
#'
#' This script downloads data from ECCC stations for a given date range, combines the resultant .csv sheets, manipulates the data to make the date a POSIXct object, and removes redundant and unnecessary fields. Note that this function may take a long time to complete if you are requesting multiple years of data!
#'
#'
#' @param station The station for which you want data. You can specify the 7-digit/letter Climate ID, the 4 or 5 digit ECCC station ID, the 5-digit WMO ID (starts with a 7), or the three-letter Transport Canada ID (i.e YDA and not CYDA). If working interactively you can also specify the station name or part thereof (as character vector) and select form a list.
#' @param start The start date for which you want data. Will download whole months only. Input either a character vector of form "2022-12-30" or a Date formated object.
#' @param end The end date for which you want data. Input either a character vector of form "2022-12-30" or a Date formated object.
#' @param save_path The path where you wish to save the resultant .csv file. Defaults to NULL, in which case you should assign the function to an R object. Set to "choose" to interactively select the location.
#'
#' @return A data.frame of weather data and, if save_path is specified, a csv of this same data located in the save_path.
#' @export
#'
getWeather <- function(station,
                       start,
                       end = Sys.Date(),
                       save_path = NULL)
{

  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the path to the folder where you want this data saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
  }

  station <- as.character(station)
  station <- toupper(station)

  #Match the input numbers to the proper ECCC station ID
    if (grepl("^[7]{1}", station)){ #Then WMO ID
      station <- data$ECCC_stations[data$ECCC_stations$WMO.ID==station & !is.na(data$ECCC_stations$WMO.ID),]
    } else if (grepl("^[0-9]{4}[0-9A-Za-z]{3}$", station)){ #Climate ID
      station <- data$ECCC_stations[data$ECCC_stations$Climate.ID==station & !is.na(data$ECCC_stations$Climate.ID),]
    } else if (grepl("^[0-6,8-9]{1}", station)){ #Station ID
      station <- data$ECCC_stations[data$ECCC_stations$Station.ID==station & !is.na(data$ECCC_stations$Station.ID),]
    } else if (grepl("^[A-Za-z]{3}$", station)) { #TC ID
      station <- data$ECCC_stations[data$ECCC_stations$TC.ID==station & !is.na(data$ECCC_stations$TC.ID),]
    } else if (grepl("^[A-Za-z]{4,}", station)){ #station name or part of
      possibilities <- dplyr::filter(data$ECCC_stations, grepl(station, Name))
      possible_names <- possibilities$Name
      possible_yrs <- paste0(possibilities$First.Year, " to ", possibilities$Last.Year)
      possible_coords <- paste0(substr(possibilities$Latitude, 1, 2), ".", substr(possibilities$Latitude, 3, 3), ", ", substr(possibilities$Longitude, 1, 4), ".", substr(possibilities$Longitude, 5, 5))
      print("The following ECCC stations are possible matches for your input:")
      for (i in 1:nrow(possibilities)) {
        cat(crayon::bold$blue$underline("Choice", i, ":"), possible_names[i], crayon::bold$green(" Years"), possible_yrs[i], crayon::bold$green(" Coords"), possible_coords[i], "\n")
      }
      choice <- readline(prompt =
                           writeLines(crayon::bold$red("\nChoose your desired station from the list and enter the number corresponding to the choice below:")))
      station <- possibilities[choice,]
    }

  yr_start <- substr(start, 1, 4)
  yr_end <- substr(end, 1, 4)

  if (station$Last.Year+2 < yr_start){
    stop(paste0("You are requesting data prior to the start of records. Records at this station are from ", station$First.Year, " to ", station$Last.Year))
  }

  if (station$Last.Year+2 < yr_end){
    end <- gsub(substr(end, 1, 4), as.numeric(station$Last.Year)+2, end)
    message(paste0("Your specified end date is long after the last available records. The end date year has been modified to ", as.numeric(station$Last.Year)+1), ".")
  }

  if (station$First.Year > yr_start){
    start <- gsub(substr(start, 1, 4), station$First.Year, start)
    message(paste0("Your specified start date is before the actual start of records. The start date has been modified to begin in year ", station$First.Year))
  }

  DateSequence <- format(seq(as.Date(start), as.Date(end), by="month")) #create date sequence according to user inputs or defaults; truncate according to first/last available data

  #download the data
  suppressWarnings(dir.create(paste0(tempdir(), "/", station$Station.ID)))
  pb <- utils::txtProgressBar(min = 0,
                              max = length(DateSequence),
                              style = 3,
                              width = 50,
                              char = "=")
  for(i in 1:length(DateSequence)){
    download.file(paste0("https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=", station$Station.ID, "&Year=", substr(DateSequence[i], start=1, stop=4), "&Month=", substr(DateSequence[i], start=6, stop=7), "&Day=14&timeframe=1&submit=%20Download+Data"),
                  destfile=paste0(tempdir(), "/", station$Station.ID, "/", DateSequence[i]),
                  method = "curl", extra = "-k",
                  quiet=TRUE)
    utils::setTxtProgressBar(pb, i)
    #TODO: replace method = curl and -k to use standard libcurl ASAP
  }
  close(pb)


  #combine the many csv files generated, sort the file, and delete the old ones
  filenames <- list.files(paste0(tempdir(), "/", station$Station.ID), full.names=TRUE) #stores each month's csv name
  #unlink(list.files(paste0(tempdir(), "/", station$Station.ID), full.names = TRUE))
  files <- lapply(filenames, read.csv) #stores the contents in a list

  files_stacked <- do.call("rbind", files)

  files_stacked$Date.Time..LST. <- as.POSIXct(files_stacked$Date.Time..LST., tz = "MST", format = "%Y-%m-%d %H:%M")  #make sure dttm is correctly formatted as POSIXct objects
  files_stacked <- files_stacked[order(files_stacked$Date.Time..LST.),] #order the whole deal

  #manipulate sheet to remove unnecessary fields nand rows
  files_stacked <- files_stacked[-c(6:9, 22:23, 26:30)] #drop redundant columns
  files_stacked <- tidyr::drop_na(files_stacked, Date.Time..LST.) #drop na rows

  #write the output to a .csv file for upload into Aquarius.
  if (!(is.null(save_path))){
    write.csv(files_stacked, file=paste0(save_path, "/ECCC_station",station$Station.ID,"_from",start,"_to",end,".csv"), row.names=FALSE)

    writeLines(paste0("All done! Your data is in the folder ", save_path))
  }

  return(files_stacked)
}
