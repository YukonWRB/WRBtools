#New plotting function that works with the DB. Should be able to plot discrete and continuous data, zoomed in or not, etc.


#' Multi-purpose plotting function
#'
#'@description
#' `r lifecycle::badge('experimental')`
#'
#' Intended to supersede [WRBfloods::levelPlot()] and [WRBfloods::flowPlot()]: see details.
#'
#' Notice: in many cases, you're better off using the Shiny app at [WRBfloods::hydroApp()] to generate and export your plot. Read on if you need additional control over the final product.
#'
#' This function plots data from the local hydrometric database (maintained by the WRBdatabase package) and yields consistent-looking plots for discrete and continuous data. This function can only plot what's in the database, so if you're unsure use the function [DB_browse_ts()] to see what's in there first.
#'
#' Plots of continuous-type data will include bands representing the historic min, max, and 25th-75th percentiles based on the last year of data plotted.
#'
#' If necessary for performance, data may be down-sampled to daily means: see details.
#'
#' @details
#' Superseded function [WRBfloods::levelPlot()] and [WRBfloods::flowPlot()] can be used in the event that this function does not yield your desired graph, or if you cannot use the local database and must use data directly from the Water Survey of Canada.
#'
#' ## Down sampling of data
#' When requesting several years of data where high-frequency (realtime) data is available, some of the data may be displayed as daily means. In this case, the 100000 to 150000 points nearest to the spefied `endDate` will be loaded at maximum resolution while subsequent points will use daily means.
#'
#' ## Return period details:
#' Return periods generated using the "calculate" option are calculated using all available data up to and including the most recent plotted data. Extreme values are isolated for each year and passed to [fasstr::compute_frequency_analysis()] using a Log Pearson Type 3 distribution and the method of moments to fit a curve to the distribution. Years (or rather periods within years; see below) missing more than the percentage of data specified in `allowed_missing` are excluded from consideration.
#'
#' As the time of year during which minimum or maximum values happen and to prevent filtering out years that are missing irrelevant data (most rivers peak in spring, some peak in late summer, snow pack peaks in winter), the parameters return_months and allowed_missing should be carefully selected. This may require you to make a graph first to evaluate when the annual min/max of interest happens. Consider as well here that you can ask for return periods specific for a month of the year, such as return periods of minimum flows in September, or maximum flows in January. This also applies to levels or any other parameter which this function can plot.
#'
#' If specifying the option "table", the function will attempt to find returns for the location within the package internal data. The returns contained there are from consultant work involving a detailed analysis of which data to keep/discard, as well as a human determination of the most appropriate distribution and curve fitting methods. The caveat here is that these tables are not necessarily up to date and that most locations in Yukon have no calculated return periods.
#'
#' @param location The location for which you want a plot.
#' @param parameter The parameter you wish to plot. The location:parameter combo must be in the local database.
#' @param type The type of data you wish to plot. Select from "discrete" or "continuous". Again, must match what's in the database.
#' @param startDay The start day of year for the plot x-axis. Can be specified as a number from 1 to 365, as a character string of form "yyyy-mm-dd", or as a date object. Either way the day of year is the only portion used, specify years to plot under parameter `years`.
#' @param endDay The end day of year for the plot x-axis. As per `startDay`.
#' @param tzone The timezone to use for graphing. Only really evident for a small number of days.
#' @param years The years to plot. If `startDay` and `endDay` cover December 31 - January 1, select the December year(s). Max 10 years, NULL = current year.
#' @param datum Should a vertical datum be applied to the data, if available? TRUE or FALSE.
#' @param title Should a title be included?
#' @param returns Should returns be plotted? You have the option of using pre-determined level returns only (option "table"), auto-calculated values(option "calculate"), "auto" (priority to "table", fallback to "calculate"), or "none". Defaults to "auto". Only for `type = "continuous"`
#' @param return_type Use minimum ("min") or maximum ("max") values for returns?
#' @param return_months Numeric vector of months during which to look for minimum or maximum values. Only works with calculated returns. Does not have to be within `startDay` and `endDay`, but will only consider data up to the last year specified in `years`. For months overlapping the new year like November-April, should look like c(11:12,1:4). IMPORTANT: the first month in the range should be the first element of the vector: c(1:4, 11:12) would not be acceptable. Think of it as defining a season. Passed to 'months' argument of [fasstr::calc_annual_extremes()] and also used to set the 'water_year_start' parameter.
#' @param allowed_missing Allowable % of data missing during the months specified in 'return_months' to still retain the year for analysis when calculating returns. Passed to 'allowed_missing' argument of [fasstr::calc_annual_extremes()].
#' @param save_path Default is NULL and the graph will be visible in RStudio and can be assigned to an object. Option "choose" brings up the File Explorer for you to choose where to save the file, or you can also specify a save path directly.
#' @param dbPath The path to the local hydromet database, passed to [hydroConnect()] or [DB_get_ts()].
#'
#' @return A .png file of the plot requested (if a save path has been selected), plus the plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment as a ggplot object which can be further modified
#' @export
#'

hydrometPlot <- function(location,
                         parameter,
                         type = "continuous",
                         startDay = 1,
                         endDay = 365,
                         tzone = "MST",
                         years = NULL,
                         datum = TRUE,
                         title = TRUE,
                         returns = "auto",
                         return_type = "max",
                         return_months = c(5:9),
                         allowed_missing = 10,
                         save_path = NULL,
                         dbPath = "default")
{

  #TODO Should give a decent error message if the user requests something that doesn't exist. Station not existing, timeseries not existing, years not available (and where they are), etc.
  type <- tolower(type)
  if (type == "discrete"){
    stop("This function is not yet capable of plotting discrete data.")
  }

  # Checks on input parameters  and other start-up bits------------------
  parameter <- tolower(parameter)
  return_type <- tolower(return_type)
  returns <- tolower(returns)
  if (!returns %in% c("table", "auto", "calculate", "none")){
    stop("Your entry for the parameter 'return' is invalid. Please review the function documentation and try again.")
  }

  if (!(type %in% c("continuous", "discrete"))) {
    stop("Parameter 'type' must be one of 'continuous' or 'discrete'.")
  }
  if (is.null(years)){
    years <- as.numeric(substr(Sys.Date(), 1, 4))
    years <- sort(years, decreasing = TRUE)
  } else {
    years <- as.numeric(years)
    if (length(years) > 10){
      years <- years[1:10]
      print("The parameter 'years' can only have up to 10 years. It's been truncated to the first 10 years in the vector.")
    }
  }
  # Select save path
  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the folder where you want this graph saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
  }

  #Connect
  con <- WRBtools::hydroConnect(path = dbPath, silent = TRUE)
  on.exit(DBI::dbDisconnect(con))

  # Dealing with start/end dates ----------------------
  # Sort out startDay and endDay into actual dates if needed
  last_year <- max(years)
  leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  tryCatch({
    startDay <- as.character(startDay)
    startDay <- as.POSIXct(startDay, tz = tzone)
    lubridate::year(startDay) <- last_year
  }, error = function(e) {
    if (last_year %in% leap_list){
      if (startDay > 59){
        startDay <<- startDay + 1
      }
    }
    startDay <<- as.POSIXct(startDay*60*60*24, origin = paste0(last_year-1, "-12-31"), tz = "UTC")
    startDay <<- lubridate::force_tz(startDay, tzone)
  })
  tryCatch({
    endDay <- as.character(endDay)
    endDay <- as.POSIXct(endDay, tz = tzone)
    lubridate::year(endDay) <- last_year
  }, error = function(e) {
    tempStartDay <- lubridate::yday(startDay) #using yday because start is now in proper Date format and needs to be back-converted to yday
    if (last_year %in% leap_list){
      if (endDay > 59){
        endDay <<- endDay + 1
      }
    }
    if (tempStartDay < endDay){
      endDay <<- as.POSIXct(endDay*60*60*24, origin = paste0(last_year-1, "-12-31 23:59:59"), tz = "UTC")
      endDay <<- lubridate::force_tz(endDay, tzone)
    } else {
      endDay <<- as.POSIXct(endDay*60*60*24, origin = paste0(last_year, "-12-31 23:59:59"), tz = "UTC")
      endDay <<- lubridate::force_tz(endDay, tzone)
    }
  })
  day_seq <- seq.POSIXt(startDay, endDay, by = "day")

  # Find the necessary datum
  if (datum & parameter == "level"){
    datum <- DBI::dbGetQuery(con, paste0("SELECT * FROM datum_conversions WHERE location = '", location, "'"))
    datum <- datum[datum$current == TRUE , ]
    datum_name <- DBI::dbGetQuery(con, paste0("SELECT datum_name_en FROM datum_list WHERE datum_id = ", datum$datum_id_to))
  } else {
    datum <- data.frame(conversion_m = 0)
  }

  #Find the ts units
  units <- DBI::dbGetQuery(con, paste0("SELECT units FROM timeseries WHERE parameter = '", parameter, "' AND location = '", location, "'"))

  # Get the necessary data -------------------
  if (type == "continuous"){
    realtime <- data.frame()
    dates <- lubridate::POSIXct(tz = tzone)
    daily_end <- endDay
    lubridate::year(daily_end) <- last_year
    daily <- DBI::dbGetQuery(con, paste0("SELECT * FROM daily WHERE location = '", location, "' AND parameter = '", parameter, "' AND date < '", as.character(daily_end), "'"))
    if (is.null(nrow(daily)) | nrow(daily) == 0){
      stop(paste0("There doesn't appear to be a match in the database for location ", location, " parameter ", parameter, " and continuous data type."))
    }
    daily$date <- as.POSIXct(daily$date, tz = tzone) #to posixct and not date so that it plays well with realtime df
    names(daily)[names(daily) == "date"] <- "datetime_UTC"
    for (i in years){
      start <- as.POSIXct(paste0(i, substr(startDay, 5, 16)), tz = tzone)
      start_UTC <- start
      attr(start_UTC, "tzone") <- "UTC"
      end <- as.POSIXct(paste0(i, substr(endDay, 5, 10), " 23:59:59"), tz = tzone)
      end_UTC <- end
      attr(end_UTC, "tzone") <- "UTC"
      if (nrow(realtime) < 100000){
        new_realtime <- DBI::dbGetQuery(con, paste0("SELECT * FROM realtime WHERE location = '", location, "' AND parameter = '", parameter, "' AND datetime_UTC BETWEEN '", as.character(start_UTC), "' AND '", as.character(end_UTC), "'"))
        new_realtime$datetime_UTC <- as.POSIXct(new_realtime$datetime_UTC, tz = "UTC")
        attr(new_realtime$datetime_UTC, "tzone") <- tzone
        realtime <- rbind(realtime, new_realtime)
        if (nrow(new_realtime) > 0){
          new_dates <- seq.POSIXt(start, end, by = "days")
          dates <- c(dates, new_dates)
          get_daily <- FALSE
        } else {
          get_daily <- TRUE
        }
      } else {
        get_daily <- TRUE
      }
      if (get_daily) {
        new_realtime <- daily[daily$datetime_UTC > start & daily$datetime_UTC < end , ]
        new_realtime <- new_realtime[ , c("location", "parameter", "datetime_UTC", "value", "grade", "approval")]
        realtime <- rbind(realtime, new_realtime)
      }
    }
    #Find out where values need to be filled in with daily means
    if (length(dates) > 0){
      for (i in 1:length(dates)){
        toDate <- as.Date(dates[i]) #convert to plain date to check if there are any datetimes with that plain date
        if (!(toDate %in% as.Date(realtime$datetime_UTC))){
          row <- daily[daily$datetime_UTC == dates[i], c("location", "parameter", "datetime_UTC", "value", "grade", "approval")]
          realtime <- rbind(realtime, row)
        }
      }
    }
    #Add the ribbon values for the times between startDay and endDay
    ribbon <- data.frame()
    for (i in 1:length(day_seq)){
      target_date <- day_seq[i]
      lubridate::year(target_date) <- last_year
      row <- daily[daily$datetime_UTC == target_date, !names(daily) %in% c("value", "grade", "approval")]
      if (nrow(row) == 0){
        prev_yr <- target_date
        lubridate::year(prev_yr) <- last_year - 1
        row <- daily[daily$datetime_UTC == prev_yr, !names(daily) %in% c("value", "grade", "approval")]
        lubridate::year(row$datetime_UTC) <- last_year
      }
      ribbon <- rbind(ribbon, row)
    }
    realtime <- merge(realtime, ribbon, all = TRUE)

    realtime$year <- lubridate::year(realtime$datetime_UTC) #year, month columns used for removing Feb 29 later
    realtime$month <- lubridate::month(realtime$datetime_UTC)
    realtime$day <- lubridate::day(realtime$datetime_UTC)
    realtime <- realtime[!(realtime$month == 2 & realtime$day == 29), ] #Remove Feb 29
    realtime$fake_datetime <- as.POSIXct(gsub("[0-9]{4}", last_year, realtime$datetime_UTC), tz = tzone) #Make fake datetimes to facilitate plotting years together as separate lines. This DOESN'T work if Feb 29 isn't removed first!
    daily$year <- lubridate::year(daily$datetime_UTC)
    daily$month <- lubridate::month(daily$datetime_UTC)
    daily$day <- lubridate::month(daily$datetime_UTC)
    daily <- daily[!(daily$month == 2 & daily$day == 29), ] #Remove Feb 29
    # apply datum correction where necessary
    if (datum$conversion_m > 0){
      daily[ , c("value", "max", "min", "QP90", "QP75", "QP50", "QP25", "QP10")] <- apply(daily[ , c("value", "max", "min", "QP90", "QP75", "QP50", "QP25", "QP10")], 2, function(x) x + datum$conversion_m)
      realtime[ , c("value", "max", "min", "QP90", "QP75", "QP50", "QP25", "QP10")] <- apply(realtime[ , c("value", "max", "min", "QP90", "QP75", "QP50", "QP25", "QP10")], 2, function(x) x + datum$conversion_m)

    }
  } else if (type == "discrete"){
    discrete <- data.frame()
    all_discrete <- DB_get_ts(path = dbPath, location = location, parameter = parameter, frequency = type, end = paste0(max(years), substr(endDay, 5, 16)))[[1]]
    #TODO date >- posixct
    for (i in years){
      start <- paste0(i, substr(startDay, 5, 16))
      end <- paste0(i, substr(endDay, 5, 16))
      new_discrete <- DB_get_ts(path = dbPath, location = location, parameter = parameter, frequency = type, start = start, end = end)[[1]]
      #TODO date >- posixct
      discrete <- rbind(discrete, new_discrete)
    }
    #TODO shift Feb 29 if there is no March 1 data point
  }

  # Make the plots  --------------------------
  if (type == "continuous"){
    colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon4")
    line_size = 1
    point_size = 0.75

    minHist <- min(realtime$min, na.rm=TRUE)
    maxHist <- max(realtime$max, na.rm=TRUE)
    minLines <- min(realtime$value, na.rm=TRUE)
    maxLines <- max(realtime$value, na.rm=TRUE)
    min <- if (minHist < minLines) minHist else minLines
    max <- if (maxHist > maxLines) maxHist else maxLines

    # x axis settings
    if (length(day_seq) > 60) {
      date_breaks = "1 month"
      labs = scales::label_date("%b %d", tz=Sys.timezone())
    } else if (length(day_seq) > 14) {
      date_breaks="1 week"
      labs = scales::label_date("%b %d", tz = Sys.timezone())
    } else if (length(day_seq) > 7) {
      date_breaks="2 days"
      labs=scales::label_date("%b %d", tz = Sys.timezone())
    } else if (length(day_seq) >= 2) {
      date_breaks="1 days"
      labs=scales::label_date("%b %d", tz = Sys.timezone())
    } else if (length(day_seq) > 1){
      date_breaks="24 hours"
      labs=scales::label_time("%H:%M", tz = Sys.timezone())
    } else if (length(day_seq) == 1) {
      date_breaks="12 hour"
      labs=scales::label_time(format="%H:%M", tz = Sys.timezone())
    }

    legend_length <- length(unique(stats::na.omit(realtime$year)))

    plot <- ggplot2::ggplot(realtime, ggplot2::aes(x = fake_datetime, y = value)) +
      ggplot2::ylim(min, max) +
      ggplot2::scale_x_datetime(date_breaks = date_breaks, labels = labs) +
      ggplot2::labs(x = "", y = paste0(stringr::str_to_title(parameter), " (", units, ")")) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "right", legend.justification = c(0, 0.95), legend.text = ggplot2::element_text(size = 8))
    if (!is.infinite(minHist)){
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = min, ymax = max, fill = "Min - Max"), na.rm = T) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "25th-75th Percentile"), na.rm = T) +
        ggplot2::scale_fill_manual(name = "Historical Range", values = c("Min - Max" = "gray85", "25th-75th Percentile" = "gray65"))
    }
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(colour = as.factor(year)), linewidth = line_size, na.rm = T) +
      ggplot2::scale_colour_manual(name = "Year", labels = rev(unique(realtime$year)), values = colours[1:legend_length], na.translate = FALSE, breaks=rev(unique(stats::na.omit(realtime$year))[1:legend_length]))

    # Get or calculate return periods -------------
    if (returns != "none"){
      if (returns %in% c("table", "auto")){
        #search for the location in the table
        returns_table <- paste0(parameter, "_returns_", return_type)
        if (location %in% data[[returns_table]]$ID){
          returns <- "table"
          loc_returns <- data[[returns_table]][data[[returns_table]]$ID == location , ]
          loc_returns[is.na(loc_returns)==TRUE] <- -10 #This prevents a ggplot error when it tries to plot a logical along with numerics, but keeps the values out of the plot.

          plot <- plot +
            ggplot2::geom_hline(yintercept=loc_returns$twoyear, linetype="dashed", color = "black") +
            ggplot2::geom_hline(yintercept=loc_returns$fiveyear, linetype="dashed", color = "black") +
            ggplot2::geom_hline(yintercept=loc_returns$tenyear, linetype="dashed", color = "black") +
            ggplot2::geom_hline(yintercept=loc_returns$twentyyear, linetype="dashed", color = "black") +
            ggplot2::geom_hline(yintercept=loc_returns$fiftyyear, linetype="dashed", color = "black") +
            ggplot2::geom_hline(yintercept=loc_returns$onehundredyear, linetype="dashed", color="black") +
            ggplot2::geom_hline(yintercept=loc_returns$twohundredyear, linetype="dashed", color="black") +
            ggplot2::geom_hline(yintercept=loc_returns$fivehundredyear, linetype="dashed", color="black") +
            ggplot2::geom_hline(yintercept=loc_returns$thousandyear, linetype="dashed", color="black") +
            ggplot2::geom_hline(yintercept=loc_returns$twothousandyear, linetype="dashed", color="black") +
            ggplot2::annotate("text", x=mean(realtime$fake_datetime), y=c(loc_returns$twoyear, loc_returns$fiveyear, loc_returns$tenyear, loc_returns$twentyyear, loc_returns$fiftyyear, loc_returns$onehundredyear, loc_returns$twohundredyear, loc_returns$fivehundredyear, loc_returns$thousandyear, loc_returns$twothousandyear), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
        } else if (returns == "auto"){ # if there is no entry to the table AND the user specified auto, calculate loop will run
          returns <- "calculate"
        }
      }
      if (returns == "calculate"){
        tryCatch({
          extremes <- suppressWarnings(fasstr::calc_annual_extremes(daily, dates = datetime_UTC, values = value, water_year_start = return_months[1], months = return_months, allowed_missing = allowed_missing))
        extremes$Measure <- "1-Day"
        if (return_type == "max"){
          analysis <- fasstr::compute_frequency_analysis(data = extremes, events = Year, values = "Max_1_Day", use_max = TRUE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005))
          return_yrs <- c(min(lubridate::year(analysis$Freq_Analysis_Data$Max_1_Day_Date)), max(lubridate::year(analysis$Freq_Analysis_Data$Max_1_Day_Date)))
        } else if (return_type == "min"){
          analysis <- fasstr::compute_frequency_analysis(data = extremes, events = Year, values = "Min_1_Day", use_max = FALSE, fit_quantiles = c(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005))
          return_yrs <- c(min(lubridate::year(analysis$Freq_Analysis_Data$Min_1_Day_Date)), max(lubridate::year(analysis$Freq_Analysis_Data$Min_1_Day_Date)))
        }
        freq <- analysis$Freq_Fitted_Quantiles

        plot <- plot +
          ggplot2::geom_hline(yintercept=as.numeric(freq[10,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[9,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[8,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[7,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[6,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[5,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[4,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[3,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[2,4]), linetype="dashed", color = "black") +
          ggplot2::geom_hline(yintercept=as.numeric(freq[1,4]), linetype="dashed", color = "black") +
          ggplot2::annotate("text", x=mean(realtime$fake_datetime), y=c(as.numeric(freq[10,4]), as.numeric(freq[9,4]), as.numeric(freq[8,4]), as.numeric(freq[7,4]), as.numeric(freq[6,4]), as.numeric(freq[5,4]), as.numeric(freq[4,4]), as.numeric(freq[3,4]), as.numeric(freq[2,4]), as.numeric(freq[1,4])), label= c("two year return", "five year return", "ten year return", "twenty year return", "fifty year return", "one hundred year return", "two hundred year return", "five hundred year return", "one-thousand year return", "two-thousand year return"), size=2.6, vjust=-.2)
        }, error = function(e){
          returns <<- "failed"
        })
      }
    }
    #Add some information below the legend
    spread <- max-min
    end_time <- max(realtime$fake_datetime)
    if (!is.infinite(minHist)){
      line1 <- paste0("\n         \n         \n        Historical range based\n        on years ", lubridate::year(min(daily$datetime_UTC)), " to ", lubridate::year(max(daily$datetime_UTC))-1, "." )
    } else {
      line1 <- "\n         \n         \n        Not enough data for\n        historical ranges"
      plot <- plot + #Adjust the legend spacing so that the text isn't pushed off the plot area
        ggplot2::theme(legend.box.spacing = ggplot2::unit(40, "pt"))
    }
    if (returns == "calculate"){
      line2 <- paste0("        \n        \n        Return periods calculated\n        using months ", month.abb[return_months[1]], " to ",  month.abb[return_months[length(return_months)]], " \n        and years ", return_yrs[1], " to ", return_yrs[2], ". \n        For informational purposes.")
      lines <- paste0(line1, line2)
      plot <- plot +
        ggplot2::coord_cartesian(clip="off", default=TRUE) +
        ggplot2::annotation_custom(grid::textGrob(lines, gp = grid::gpar(fontsize=8), just="left"), xmin=end_time, ymin = (max-spread/2)-8*spread/30, ymax =(max-spread/2)-8*spread/30)
    } else if (returns == "table"){
      line2 <- "        \n        \n        Return periods are based\n        on statistical analysis\n        of select data from the\n        start of records to 2021."
      lines <- paste0(line1, line2)
      plot <- plot +
        ggplot2::coord_cartesian(clip="off", default=TRUE) +
        ggplot2::annotation_custom(grid::textGrob(lines, gp = grid::gpar(fontsize=8), just="left"), xmin=end_time, ymin = (max-spread/2)-8*spread/30, ymax =(max-spread/2)-8*spread/30)
    } else if (returns == "failed") {
      line2 <- "        \n        \n        Insufficient data to \n        calculate returns using\n        last requested year."
      lines <- paste0(line1, line2)
      plot <- plot +
        ggplot2::coord_cartesian(clip="off", default=TRUE) +
        ggplot2::annotation_custom(grid::textGrob(lines, gp = grid::gpar(fontsize=8), just="left"), xmin=end_time, ymin = (max-spread/2)-8*spread/30, ymax =(max-spread/2)-8*spread/30)

    } else {
      plot <- plot +
        ggplot2::coord_cartesian(clip="off", default=TRUE) +
        ggplot2::annotation_custom(grid::textGrob(line1, gp = grid::gpar(fontsize=8), just = "left"), xmin=end_time, ymin = (max-spread/2)-7*spread/30, ymax=(max-spread/2)-7*spread/30)
    }
  }


  if (type == "discrete") {

  }
  # Wrap things up and return() -----------------------
  if (title == TRUE){
    stn_name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations where location = '", location, "'"))
    plot <- plot +
      ggplot2::labs(title=paste0("Location ", location, ": ", stn_name)) +
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.05, size=14))
  }

  #Save it if requested
  if (!is.null(save_path)){
    ggplot2::ggsave(filename=paste0(save_path,"/", location, "_", parameter, "_", Sys.Date(), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz=tzone)), lubridate::minute(as.POSIXct(format(Sys.time()), tz=tzone)), ".png"), plot=plot, height=8, width=12, units="in", device="png", dpi=500)
  }

  return(plot)
}
