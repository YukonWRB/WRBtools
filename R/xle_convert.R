#' Convert Solinst logger files to csv format
#'
#' Reads a Solinst .xle file and converts it into a .csv with proper column names. Converts units to those in common usage at the Yukon Water Resources Branch, standardizes file naming, and ensures that times are represented in UTC-7.
#'
#' @param xle_file The file you wish to convert. Default "choose" allows you to point to the file.
#' @param location The ID of the well in the form "YOWN-1500".
#' @param save_path The location where the csv file should be saved.
#' @param YOWN_master The location to the YOWN master spreadsheet.
#'
#' @return A csv of the logger data, ready for export to Aquarius or for general use.
#' @export
#'
xle_convert <- function(xle_file = "choose",
                           location,
                           save_path = "choose",
                           YOWN_master = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/MASTER for R - remember to update.xlsx"
                           )
{

  if (xle_file == "choose"){
    print("Select the path to the logger file.")
    xle_file <- as.character(utils::choose.files(caption="Select logger file"))
  }
  if (save_path == "choose"){
    print("Select the path to the folder where you want this data saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  # Set directories
  filepath_to_master <- YOWN_master

  user_location <- location #The script will check this location against the YOWN Master table to ensure consistency, and will check against the location saved to the .xle logger file


  # NAME CHECK AND CORRECTION AGAINST MASTER SPREADSHEET
  # Read in master sheet
  yown_stn_names <- dplyr::filter(openxlsx::read.xlsx(filepath_to_master, sheet = 1),
                           !is.na(`YOWN.Code`), !is.na(`Name`))
  possible_names <- c()
  for (i in 1:nrow(yown_stn_names)) {

    user_code <- stringr::str_extract(user_location, "YOWN-....")
    user_name <- stringr::str_remove(user_location, "YOWN-....")
    if (nchar(user_name) == 0){
      user_name <- "no match"
    }

    # If the YOWN-xxxx provided by user matches a row in the master xlsx OR if the station name provided matches loosely with a row, save that station's code and name in possible_names
    if (grepl(user_code, yown_stn_names$`YOWN.Code`[i]) |
        agrepl(user_name, yown_stn_names$Name[i])) {
      possible_names <- c(possible_names, paste(yown_stn_names$`YOWN.Code`[i],
                                                yown_stn_names$Name[i]))
    }
  }

  # Now correct_name is a list (with 0, 1, or more elements) which corresponds to the different entries in the master spreadsheet that resembled the location you inputted. Here, either select the position of the element in correct_name that matches with the location you desire, or take this time to input the correct location (format: YOWN-XXXX STATIONNAME).
  if (length(possible_names) > 1){
    print("Stations in the YOWN Master sheet that matched your input:")
    for (i in 1:length(possible_names)) {
      cat("Position ", i, " :", possible_names[i], "\n")
    }

    choice <- readline(prompt =
                         writeLines(paste("\nChoose the correct name by selecting",
                                          "its position in the list.",
                                          "\nIf the available options are not",
                                          "correct, type in the correct name",
                                          "(format: YOWN-XXXX STATIONNAME)"
                         )))

    # This regex expression returns true only if "choice" is a digit, false otherwise
    if (grepl("^[[:digit:]]+$", choice)) {
      user_location <- possible_names[as.numeric(choice)]
      print("Location has been updated to match format in master spreadsheet")
    } else {
      user_location <- choice
      print("New name has been inputted by user")
    }
  }

  # CONVERT XLE TO XML ###########################################################

  # As far as Lana sees, encodings of .xle files are either UTF-8 or Windows-1252 (Where Windows-1252 is the same as ANSI)

  tryCatch( {result <- XML::xmlParse(file = xle_file, encoding = "UTF-8") },
            error=function(e) {"Encoding is not UTF-8"})

  # If UTF-8 encoding works, running the ANSI encoding will give bad output
  # Only try ANSI encoding if UTF-8 cannot be applied
  # Note that ANSI encoding still yields ÂµS/cm rather than µS/cm
  if (!exists("result")) {
    tryCatch( {result <- XML::xmlParse(file = xle_file, encoding = "Windows-1252") },
              error = function(e) {"Encoding not Windows-1252/ANSI or UTF-8 and this script will not work."})
  }

  xml_data <- XML::xmlToList(result)

  #### Set up proper header names and units. Perform unit conversions where needed
  # Get each measurement's header name, parameter name and unit. Store in "check"
  check <- data.frame(header_name = character(), # Specify empty vectors
                      section_name = character(),
                      unit_current = numeric(),
                      stringsAsFactors = FALSE)
  j <- 1 # Counter

  # Populate "check"
  for (i in 1:length(xml_data)) {
    name <- names(xml_data)[i]
    if (stringr::str_detect(name, "Ch._data_header")) {
      check[j,] <- list(xml_data[[name]][["Identification"]],
                        name,
                        xml_data[[name]][["Unit"]])
      j <- j + 1
    }
  }

  # Here "check" looks like (approximately)
  # header_name  | section_name    | unit_current
  # CHECK INSTRUMENT TYPE AND UNITS ----
  # LEVEL        | Ch1_data_header | m
  # TEMPERATURE  | Ch2_data_header | °C
  # CONDUCTIVITY | Ch3_data_header | mS/cm

  # Sometimes logger files' parameter names have typos
  # and/or must be converted to different unit

  # Storing proper parameter names and units
  proper_LTC <- data.frame("parameter" = c("LEVEL", "TEMPERATURE","CONDUCTIVITY"),
                           "unit_proper" = c("m", "°C", "µS/cm"))
  proper_BL  <- data.frame("parameter" = c("LEVEL", "TEMPERATURE"),
                           "unit_proper" = c("kPa", "°C"))

  # Storing proper parameter units and their conversions
  conversions_LTC <- data.frame("parameter" = c("CONDUCTIVITY"),
                                "unit_proper" = c("µS/cm"),
                                "unit_current" = c("mS/cm"),
                                "multiplier" = c(1000))
  conversions_BL  <- data.frame("parameter" = c("LEVEL", "LEVEL", "LEVEL"),
                                "unit_proper" = c("kPa", "kPa", "kPa"),
                                "unit_current" = c("psi", "m", "mbar"),
                                "multiplier" = c(6.89467, (1/0.101972), 0.1))

  # Instrument_type either contains "LTC" (are working with levelogger) OR "LT" (are working with barologger)
  Instrument_type <- xml_data[["Instrument_info"]][["Instrument_type"]]

  # NOTE FOR GHISLAIN: 2019 onwards usually has levelogger files = LTC, barologger files = LT. That is the basis for the if statement in this loop, as well as the "proper_LTC/BL".
  if (grepl("LTC", Instrument_type)) {
    # Are working with LTC Instrument -> levelogger
    Instrument_type <- "LTC"
    check <- proper_LTC %>%
      # Do left join on approximate match bw parameter & parameter name from .xle
      # Necessary because of occasional typos in logger file
      fuzzyjoin::stringdist_left_join(check, by = c("parameter" = "header_name"),
                           max_dist = 2, ignore_case = TRUE) %>%
      dplyr::select(parameter, section_name, unit_proper, unit_current) %>%
      dplyr::left_join(conversions_LTC,
                by = c("parameter", "unit_proper", "unit_current"))

  } else if (grepl("LT[^(LTC)]", Instrument_type)) {
    # Are working with LT Instrument -> barologger
    Instrument_type <- "BL"

    check <- proper_BL %>%
      # Do left join on approximate match bw parameter & parameter name from .xle
      # Necessary because of occasional typos in logger file
      fuzzyjoin::stringdist_left_join(check, by = c("parameter" = "header_name"),
                           max_dist = 2, ignore_case = TRUE) %>%
      dplyr::select(parameter, section_name, unit_proper, unit_current) %>%
      dplyr::left_join(conversions_BL,
                by = c("parameter", "unit_proper", "unit_current"))

  } else {

    stop("This script is not designed to handle this logger file structure.")

  }

  # Store the logged data
  df <- XML::xmlToDataFrame(nodes = XML::xmlChildren(XML::xmlRoot(result)[["Data"]]))
  df$Date <- as.Date(df$Date)

  # Loop through columns of dataframe and give columns containing level, temp, or conductivity data the correct column name AND perform unit conversion if needed
  for (i in 1:length(df)) {
    curr_name <- names(df)[i]
    if (stringr::str_detect(curr_name, "ch.")) { # If column name has ch we are looking at LTC/LT vals

      # Row containing info we need, including parameter name, multiplier, proper unit
      check_row <- dplyr::filter(check, grepl(curr_name, section_name ,
                                       ignore.case = TRUE))

      # Give column in df proper name
      colnames(df)[i] <- c(check_row$parameter)

      # Unit conversion if needed
      if (!is.na(check_row$multiplier)) {
        df[[i]] <- as.numeric(unlist(df[[i]]))

        # If we're converting from m to kPa, we are using an old logger
        # and the conversion is not as simple as multiplication:
        if (agrepl("kPa", check_row$unit_proper) &
            agrepl("m", check_row$unit_current)) {
          # If Baro pressure was calculated in meters,
          # First add 9.5 (automatic offset), then convert to kPa
          # then subtract the difference in pressure at that elevation
          # from sea level
          altitude <- as.numeric(
            xml_data[["Ch1_data_header"]][["Parameters"]][["Altitude"]][1])
          df[[i]] <- (9.5 + df[[i]])*(check_row$multiplier) -
            (101.325 - (101.325*(1-2.25577*10^(-5)*altitude)^5.25588))
        } else {
          # Any other unit conversion is just a simple multiplication
          df[[i]] <- df[[i]]*check_row$multiplier
        }

      }
    }
  }

  # Just in case. Makes sure columns are in the order shown below
  if (identical("LTC", Instrument_type)) {
    df <- dplyr::select(df, Date, Time, ms, LEVEL, TEMPERATURE, CONDUCTIVITY)
  } else if (identical("BL", Instrument_type)) {
    df <- dplyr::select(df, Date, Time, ms, LEVEL, TEMPERATURE)
  } else {
    stop("This script is not designed to handle this logger file structure.")
  }

  # Handling daylight savings
  start_datetime <- as.POSIXct(paste(df$Date[1], df$Time[1]))

  if (start_datetime < "2020-03-08 02:00:00") {
    # Are in time period where daylight savings was used
    # With exception of 2020, dst returns true for UTC-07, false for UTC-08
    if (!lubridate::dst(start_datetime)) {
      # Started on UTC-08. Bump up each time stamp by 1h
      df$Time <- format(as_datetime(paste(df$Date, df$Time)) + hours(1),
                        "%H:%M:%S")
      df$Date <- format(as_datetime(paste(df$Date, df$Time)) + hours(1),
                        "%Y-%m-%d")
    }
  }

  # Setting up file w header and data & exporting to .csv

  Serial_number     <- xml_data[["Instrument_info"]][["Serial_number"]]
  Project_ID        <- xml_data[["Instrument_info_data_header"]][["Project_ID"]]

  if (is.null(xml_data[["Instrument_info_data_header"]][["Location"]])==TRUE) {
    Location        <- "No Location in logger file"
  } else {
    Location          <- xml_data[["Instrument_info_data_header"]][["Location"]]
  }

  Field_visit_date  <- xml_data[["File_info"]][["Date"]]



  # If user's supplied location does not match loosely to the field visit in
  # the file, user is given a chance to choose if their location is accurate,
  # or if the location listed in the file is correct.
  # To cover instances of a) missing location names in .xle files and b) incorrect
  # location names (because of a logger moved between locations, for example)
  if (!agrepl(user_location, Location, ignore.case = TRUE)) {
    choice <- readline(prompt = writeLines(paste("\n\nYour input location:                          ",
                                                 user_location,
                                                 "\nThe location specified in the logger file:    ",
                                                 Location,
                                                 "\n\nThe location you provided",
                                                 "does not match the location",
                                                 "listed in the xle.\n",
                                                 "Do you want to override the",
                                                 "file's location with your",
                                                 "inputted location?\nY/N\n")))
    if (grepl("^y", choice, ignore.case = TRUE)) {
      Location <- user_location
      print("Correct location was user's location")
    } else {
      print("Correct location was file's location")

      possible_names <- c()
      for (i in 1:nrow(yown_stn_names)) {

        user_code <- stringr::str_extract(Location, "YOWN-....")
        user_name <- stringr::str_remove(Location, "YOWN-....")

        # If the YOWN-xxxx provided by user matches a row in the master xlsx
        # OR if the station name provided matches loosely with a row,
        # save that station's code and name in possible_names
        if (grepl(user_code, yown_stn_names$`YOWN Code`[i]) |
            agrepl(user_name, yown_stn_names$Name[i])) {
          possible_names <- c(possible_names, paste(yown_stn_names$`YOWN Code`[i],
                                                    yown_stn_names$Name[i]))
        }
      }

      print("Stations that matched the file:")
      for (i in 1:length(possible_names)) {
        cat("Position ", i, " :", possible_names[i], "\n")
      }
      choice <- readline(prompt =
                           writeLines(paste("Choose the correct name by selecting",
                                            "its position in the list (possible_names)",
                                            "\nIf the available options are not",
                                            "correct, type in the correct name",
                                            "(format: YOWN-XXXX STATIONNAME)"
                           )))
      # This regex expression return true only if "choice" is a digit, false otherwise
      if (grepl("^[[:digit:]]+$", choice)) {
        # Are choosing position in list
        user_location <- possible_names[as.numeric(choice)]
        print("Location has been updated to match format in master spreadsheet")
      } else {
        user_location <- choice
        print("New name has been inputted by user")
      }

    }

    writeLines(paste("NOTE: if string listed under the variable \"Location\" is",
                     "still incorrect, set the variable now by writing the line:",
                     "\n\tLocation <- correct_location_name"))

  }



  # csv name will be startdate_to_enddate_location_loggertype.csv
  startdate <- gsub("-", "", df$Date[1])
  enddate <- gsub("-", "", df$Date[nrow(df)])

  filename <- paste(user_location,
                    "_",
                    startdate,
                    "_to_",
                    enddate,
                    "_",
                    Instrument_type,
                    ".csv",
                    sep = ""
  )

  f <- file(paste0(save_path, "/", filename), "w")

  params_units <- NA
  # params_units will be a string for the header rows (info) for the csv
  # Will look something like:
  # LEVEL
  # UNIT: kPa
  # TEMPERATURE
  # UNIT: °C


  if (identical("LTC", Instrument_type)) {

    Level_offset <- as.numeric(xml_data[["Ch1_data_header"]][["Parameters"]][["Offset"]][1])

    # We use "check" to determine the units
    params_units <- paste(c("LEVEL",
                            paste("UNIT: ", dplyr::select(dplyr::filter(check, parameter == "LEVEL"),
                            unit_proper)),
                            paste("Offset: ", Level_offset),
                            "TEMPERATURE",
                            paste("UNIT: ",
                                  dplyr::select(dplyr::filter(check,
                                                parameter == "TEMPERATURE"),
                                         unit_proper)),
                            "CONDUCTIVITY",
                            paste("UNIT: ",
                                  dplyr::select(dplyr::filter(check,
                                                parameter == "CONDUCTIVITY"),
                                         unit_proper))
                            ))

  } else if (identical("BL", Instrument_type)) {

    params_units <- paste(c("LEVEL",
                            paste("UNIT: ",
                                  dplyr::select(filter(check,
                                                parameter == "LEVEL"),
                                         unit_proper)),
                            "TEMPERATURE",
                            paste("UNIT: ",
                                  dplyr::select(filter(check, parameter == "TEMPERATURE"),
                                         unit_proper)),
                            "CONDUCTIVITY",
                            "NOT REPORTED"
                            ))

  } else {

    stop("This script is not designed to handle this logger file structure.")

  }

  # Write the following string to the csv as the header lines
  writeLines(paste(c("Serial_number:", Serial_number,
                     "Project ID:", Project_ID,
                     "Location:", user_location,
                     "Timezone:", "UTC-07:00",
                     params_units)),
             f)

  # Write the actual data into the csv and create the csv
  utils::write.csv(df, f, row.names = FALSE)

  close(f)

  writeLines(paste0("\n\t\n\tThank you for using this script! Your file is now in ", save_path, "."))
}
