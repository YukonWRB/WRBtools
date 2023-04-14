#' Calculated standard processing sub-function required by EQ_fetch function
#'
#' Utility function to populate the std_calc table (product of EQ_fetch function) with calculated values, using averaged or calculated station parameters (pH, hardness, DOC, temp)
#'
#' @param fun_sampledata  Assigned to "sampledata" data frame created during EQ_fetch function process containing sample data
#' @param fun_std_calc Assigned to "std_calc" data frame of calculated standards, populated by this function
#'
#' @return Populated std_calc table in EQfetch function
#'
#' @export
#'
#' @details interim parameter vales are assigned on an as-needs basis where required, such as standards where hardness is assigned a certain value if NA. These interim parameters are represented by the .x subscript (ie. pHx, hardx)

EQ_std_calc <- function(fun_sampledata = sampledata,
                        fun_std_calc = std_calc){

  #### Calculate input parameters from data ####

  # Calculate pH with calculation order preference
  if(!all(is.na(fun_sampledata$`pH-F (pH units)`))) {
    pH <- mean(na.omit(fun_sampledata$`pH-F (pH units)`))
  } else if(!all(is.na(fun_sampledata$`pH-L (pH units)`))) {
    pH <- mean(na.omit(fun_sampledata$`pH-L (pH units)`))
  } else {pH <- NA}

  # Calculate hardness with calculation order preference
  if(!all(is.na(fun_sampledata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(fun_sampledata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(fun_sampledata$`Ca-D (mg/L)`))*mean(na.omit(fun_sampledata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(fun_sampledata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(fun_sampledata$`Mg-D (mg/L)`))
  } else if(!all(is.na(fun_sampledata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(fun_sampledata$`Hard-T (mg/L)`))
  } else if(!is.na(mean(na.omit(fun_sampledata$`Ca-T (mg/L)`))*mean(na.omit(fun_sampledata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(fun_sampledata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(fun_sampledata$`Mg-T (mg/L)`))
  } else {
    hard <- NA}

  # Calculate DOC
  if(!all(is.na(fun_sampledata$`C-DOC (mg/L)`))){
    DOC <- mean(na.omit(fun_sampledata$`C-DOC (mg/L)`))
  } else {
    DOC <- NA
  }

  # Calculate temp
  if(!all(is.na(fun_sampledata$`Temp-F (C)`))) {
    temp <- plyr::round_any(mean(na.omit(fun_sampledata$`Temp-F (C)`)), 5, f = floor)
  } else {
    temp <- NA
  }

  #### CCME Long Term (T/D) ####

  # CCME_Al_lt
  if(!is.na(pH)){
    if(pH < 6.5){
      CCME_Al_lt <- 0.005}
    else if (pH >= 6.5){
      CCME_Al_lt <- 0.1}
  } else {
    CCME_Al_lt <- NA}
  std_calc[std_calc$MaxVal == "CCME_Al_lt", "MaxVal"] <- CCME_Al_lt

  # CCME_Cd_lt
  if(hard < 17 | is.na(hard)){
    CCME_Cd_lt <- 0.04/1000
  } else if(hard >=17 & hard <=280){
    CCME_Cd_lt <- 10^(0.83*(log10(hard))-2.46)/1000
  } else if(hard > 280){
    CCME_Cd_lt <- 0.37/1000
  }
  std_calc[std_calc$MaxVal == "CCME_Cd_lt", "MaxVal"] <- CCME_Cd_lt

  # CCME_Cu_lt
  if(is.na(hard)){
    CCME_Cu_lt <- 2/1000
  } else if(hard >= 82 & hard <= 180){
    CCME_Cu_lt <- 0.2*exp((0.8545*log(hard)-1.465))/1000
  } else if(hard > 180){
    CCME_Cu_lt <- 4/1000
  }
  std_calc[std_calc$MaxVal == "CCME_Cu_lt", "MaxVal"] <- CCME_Cu_lt

  # CCME_Mn-D_lt
  if(is.na(pH)){
    pHx <- 7.5
  } else {
    pHx <- pH
  }
  pHx <- plyr::round_any(pHx, accuracy = 0.1, f = floor)

  if(is.na(hard)){
    hardx <- 50
  } else {
    hardx <- hard
  }
  hardx <- floor(hardx)

  lookup <- as.data.frame(readxl::read_xlsx(path="G:/water/Common_GW_SW/R-packages/WRBtools/EQfetch_std_lookup.xlsx",sheet="Mn", col_names=TRUE))
  colnames(lookup) <- suppressWarnings(as.character(plyr::round_any(as.numeric(colnames(lookup)), accuracy = 0.1, f = ceiling)))
  colnames(lookup)[1] <- "Min"
  colnames(lookup)[2] <- "Max"
  `CCME_Mn-D_lt` <- dplyr::pull(dplyr::filter(lookup, hardx >= Min & hardx <= Max)[which(colnames(lookup) == as.character(pHx))])
  std_calc[std_calc$MaxVal == "CCME_Mn-D_lt", "MaxVal"] <- `CCME_Mn-D_lt`

  #### CCME Short Term (t/d) ####
  # CCME_Cd_st
  if(is.na(hard)){
    CCME_Cd_st <- 0.11/1000
  } else if(hard >=5.3 & hard <=360){
    CCME_Cd_st <- 10^(1.016*(log10(hard))-1.71)/1000
  } else if(hard > 360){
    CCME_Cd_st <- 7.7/1000
  }
  std_calc[std_calc$MaxVal == "CCME_Cd_st", "MaxVal"] <- CCME_Cd_st


  ####CCME_Mn-D_st####
  if(is.na(hard)){
    hardx <- 50
  } else {
    hardx <- hard
  }
  `CCME_Mn-D_st` <- exp((0.878*log(hardx)+4.76))/1000
  std_calc[std_calc$MaxVal == "CCME_Mn-D_st", "MaxVal"] <- `CCME_Mn-D_st`

  ####CCME_NH4_lt####
  if(is.na(pH)){
    pHx <- 7.5
  } else {
    pHx <- pH
  }
  pHx <- plyr::round_any(pHx, accuracy = 0.5, f = floor)
  if(is.na(temp)){
    tempx <- 30
  } else {
    tempx <- temp
  }

  lookup <- as.data.frame(readxl::read_xlsx(path="G:/water/Common_GW_SW/R-packages/WRBtools/EQfetch_std_lookup.xlsx",sheet="NH4", col_names=TRUE))
  CCME_NH4_lt <- dplyr::pull(dplyr::filter(lookup, Temp == tempx)[which(colnames(lookup) == as.character(pHx))])
  std_calc[std_calc$MaxVal == "CCME_NH4_lt", "MaxVal"] <- CCME_NH4_lt

  ####CCME_Ni_lt####
  if(is.na(hard) | hard <= 60){
    CCME_Ni_lt <- 25/1000
  } else if(hard > 60 & hard <=180){
    CCME_Ni_lt <- exp((0.76*log(hard)+1.06))/1000
  } else if(hard > 180){
    CCME_Ni_lt <- 150/1000
  }
  std_calc[std_calc$MaxVal == "CCME_Ni_lt", "MaxVal"] <- CCME_Ni_lt

  ####CCME_Pb_lt####
  if(is.na(hard)){
    CCME_Pb_lt <- 1/1000
  } else if(hard <= 60){
    CCME_Pb_lt <- 1/1000
  } else if(hard > 60 & hard <=180){
    CCME_Pb_lt <- exp((1.273*log(hard)-4.705))/1000
  } else if(hard > 180){
    CCME_Pb_lt <- 7/1000
  }
  std_calc[std_calc$MaxVal == "CCME_Pb_lt", "MaxVal"] <- CCME_Pb_lt

  ####CCME_Zn_lt####
  if(any(is.na(c(DOC, pH, hard)))){
    CCME_Zn_lt <- NA
  } else if(pH < 6.5 | pH > 8.13 | hard < 23.4 | hard > 399 | DOC < 0.3 | DOC > 22.9){
    CCME_Zn_lt <- NA
  } else {
    CCME_Zn_lt <- exp((0.947*log(hard)) - (0.815*pH) + (0.398*log(DOC)) + 4.625)/1000
  }
  std_calc[std_calc$MaxVal == "CCME_Zn_lt", "MaxVal"] <- CCME_Zn_lt

  ####CCME_Zn_st####
  if(any(is.na(c(DOC, pH, hard)))){
    CCME_Zn_st <- NA
  } else if(pH < 6.5 | pH > 8.13 | hard < 13.8 | hard > 250.5 | DOC < 0.3 | DOC > 17.3){
    CCME_Zn_st <- NA
  } else {
    CCME_Zn_st <- exp((0.833*log(hard)) + (0.240*log(DOC)) + 0.526)/1000
  }
  std_calc[std_calc$MaxVal == "CCME_Zn_st", "MaxVal"] <- CCME_Zn_st

  return(std_calc)
}
