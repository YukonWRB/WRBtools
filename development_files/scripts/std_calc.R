# Utility function to populate the rcalcs table with calculated values, using averaged pH and hardness values for the station.


# Calculate EQwin standards
dbpath <- "X:/EQWin/WR/DB/Water Resources.mdb"
EQWin <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dbpath))
calcs <- data.table::as.data.table(DBI::dbReadTable(EQWin, "eqcalcs"))
rcalcs <- calcs %>%
  dplyr::mutate(MaxVal = as.numeric(NA),
                MinVal = as.numeric(NA)) %>%
  dplyr::filter(Category == "Calculated Standard")

####CCME_Al_lt####
if(!all(is.na(stndata$`pH-F (pH units)`))) {
  pH <- mean(na.omit(stndata$`pH-F (pH units)`))
} else if(!all(is.na(stndata$`pH-L (pH units)`))) {
  pH <- mean(na.omit(stndata$`pH-L (pH units)`))
} else {pH <- NA}
if(!is.na(pH)){
  if(pH < 6.5){
    CCME_Al_lt <- 0.005
  } else if (pH >= 6.5){
    CCME_Al_lt <- 0.1
  }
} else {
  CCME_Al_lt <- NA
}
rcalcs[rcalcs$CalcId == 1, "MaxVal"] <- CCME_Al_lt

####CCME_Cd_lt####
CCME_Cd_lt <- function(){
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- 16
    }
  if(hard < 17){
    X <- 0.04/1000
  } else if(hard >=17 & hard <=280){
    X <- 10^(0.83*(log10(hard))-2.46)/1000
  } else if(hard > 280){
    X <- 0.37/1000
  }
  return(X)
}
rcalcs[rcalcs$CalcId == 2, "MaxVal"] <- CCME_Cd_lt()

####CCME_Cd_st####
CCME_Cd_st <- function(){
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- 5.2
  }
  if(hard < 5.3){
    X <- 0.11/1000
  } else if(hard >=5.3 & hard <=360){
    X <- 10^(1.016*(log10(hard))-1.71)/1000
  } else if(hard > 360){
    X <- 7.7/1000
  }
  return(X)
}
rcalcs[rcalcs$CalcId == 117, "MaxVal"] <- CCME_Cd_st()

####CCME_Cu_lt####
CCME_Cu_lt <- function(){
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- 81
  }
  if(hard < 82){
    X <- 2/1000
  } else if(hard >= 82 & hard <= 180){
    X <- 0.2*exp((0.8545*log(hard)-1.465))/1000
  } else if(hard > 180){
    X <- 4/1000
  }
  return(X)
}
rcalcs[rcalcs$CalcId == 3, "MaxVal"] <- CCME_Cu_lt()

####CCME_Mn_D_lt ####
CCME_Mn_D_lt <- function(){
  if(!all(is.na(stndata$`pH-F (pH units)`))) {
    pH <- mean(na.omit(stndata$`pH-F (pH units)`))
  } else if(!all(is.na(stndata$`pH-L (pH units)`))) {
    pH <- mean(na.omit(stndata$`pH-L (pH units)`))
  } else {
    pH <- 7.5
  }
  pH <- plyr::round_any(pH, accuracy = 0.1, f = floor)
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- 50
  }
  hard <- floor(hard)
  lookup <- as.data.frame(readxl::read_xlsx(path="G:/water/Common_GW_SW/R-packages/WRBtools/EQfetch_std_lookup.xlsx",sheet="Mn", col_names=TRUE))
  colnames(lookup) <- suppressWarnings(as.character(plyr::round_any(as.numeric(colnames(lookup)), accuracy = 0.1, f = ceiling)))
  colnames(lookup)[1] <- "Min"
  colnames(lookup)[2] <- "Max"
  X <- pull(dplyr::filter(lookup, hard >= Min & hard <= Max)[which(colnames(lookup) == as.character(pH))])
  return(X)
}
rcalcs[rcalcs$CalcId == 100, "MaxVal"] <- CCME_Mn_D_lt()

####CCME_Mn_D_st####
CCME_Mn_D_st <- function(){
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- 50
  }
  X <- exp((0.878*log(hard)+4.76))/1000
  return(X)
}
rcalcs[rcalcs$CalcId == 99, "MaxVal"] <- CCME_Mn_D_st()

####CCME_NH4_lt####
CCME_Mn_D_lt <- function(){
  if(!all(is.na(stndata$`pH-F (pH units)`))) {
    pH <- mean(na.omit(stndata$`pH-F (pH units)`))
  } else if(!all(is.na(stndata$`pH-L (pH units)`))) {
    pH <- mean(na.omit(stndata$`pH-L (pH units)`))
  } else {
    pH <- 7.5
  }
  pH <- plyr::round_any(pH, accuracy = 0.5, f = floor)
  if(!all(is.na(stndata$`Temp-F (C)`))) {
    temp <- plyr::round_any(mean(na.omit(stndata$`Temp-F (C)`)), 5, f = floor)
  } else {
    temp <- NA
  }
  lookup <- as.data.frame(readxl::read_xlsx(path="G:/water/Common_GW_SW/R-packages/WRBtools/EQfetch_std_lookup.xlsx",sheet="NH4", col_names=TRUE))
  X <- pull(dplyr::filter(lookup, Temp == temp)[which(colnames(lookup) == as.character(pH))])
  return(X)
}
rcalcs[rcalcs$CalcId == 46, "MaxVal"] <- CCME_Mn_D_st()

####CCME_Ni_lt####
CCME_Ni_lt <- function(){
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- 59
  }
  if(hard <= 60){
    X <- 25/1000
  } else if(hard > 60 & hard <=180){
    X <- exp((0.76*log(hard)+1.06))/1000
  } else if(hard > 180){
    X <- 150/1000
  }
  return(X)
}
rcalcs[rcalcs$CalcId == 5, "MaxVal"] <- CCME_Ni_lt()
####CCME_Pb_lt####
CCME_Pb_lt <- function(){
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- 59
  }
  if(hard <= 60){
    X <- 1/1000
  } else if(hard > 82 & hard <=180){
    X <- exp((1.273*log(hard)-4.705))/1000
  } else if(hard > 180){
    X <- 7/1000
  }
  return(X)
}
rcalcs[rcalcs$CalcId == 4, "MaxVal"] <- CCME_Pb_lt()

####CCME_Zn_lt####
CCME_Zn_lt <- function(){
  if(!all(is.na(stndata$`C-DOC (mg/L)`))){
    DOC <- mean(na.omit(stndata$`C-DOC (mg/L)`))
  } else {DOC <- NA}
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- NA
  }
  if(!all(is.na(stndata$`pH-F (pH units)`))) {
    pH <- mean(na.omit(stndata$`pH-F (pH units)`))
  } else if(!all(is.na(stndata$`pH-L (pH units)`))) {
    pH <- mean(na.omit(stndata$`pH-L (pH units)`))
  } else {pH <- NA}
  if(any(is.na(c(DOC, pH, hard)))){
    X <- NA
  } else if(pH < 6.5 | pH > 8.13 | hard < 23.4 | hard > 399 | DOC < 0.3 | DOC > 22.9){
    X <- NA
  } else {
    X <- exp((0.947*log(hard)) - (0.815*pH) + (0.398*log(DOC)) + 4.625)/1000
  }
  return(X)
}
rcalcs[rcalcs$CalcId == 109, "MaxVal"] <- CCME_Zn_lt()

####CCME_Zn_st####
CCME_Zn_st <- function(){
  if(!all(is.na(stndata$`C-DOC (mg/L)`))){
    DOC <- mean(na.omit(stndata$`C-DOC (mg/L)`))
  } else {DOC <- NA}
  if(!all(is.na(stndata$`Hard-D (mg/L)`))) {
    hard <- mean(na.omit(stndata$`Hard-D (mg/L)`))
  } else if(!is.na(mean(na.omit(stndata$`Ca-D (mg/L)`))*mean(na.omit(stndata$`Mg-D (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-D (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-D (mg/L)`))
  } else if(!all(is.na(stndata$`Hard-T (mg/L)`))){
    hard <- mean(na.omit(stndata$`Hard-T (mg/L)`))
  }else if(!is.na(mean(na.omit(stndata$`Ca-T (mg/L)`))*mean(na.omit(stndata$`Mg-T (mg/L)`)))) {
    hard <- 2.497*mean(na.omit(stndata$`Ca-T (mg/L)`)) + 4.118*mean(na.omit(stndata$`Mg-T (mg/L)`))
  } else {
    hard <- NA
  }
  if(!all(is.na(stndata$`pH-F (pH units)`))) {
    pH <- mean(na.omit(stndata$`pH-F (pH units)`))
  } else if(!all(is.na(stndata$`pH-L (pH units)`))) {
    pH <- mean(na.omit(stndata$`pH-L (pH units)`))
  } else {pH <- NA}
  if(any(is.na(c(DOC, pH, hard)))){
    X <- NA
  } else if(pH < 6.5 | pH > 8.13 | hard < 13.8 | hard > 250.5 | DOC < 0.3 | DOC > 17.3){
    X <- NA
  } else {
    X <- exp((0.833*log(hard)) + (0.240*log(DOC)) + 0.526)/1000
  }
  return(X)
}
rcalcs[rcalcs$CalcId == 118, "MaxVal"] <- CCME_Zn_st()
