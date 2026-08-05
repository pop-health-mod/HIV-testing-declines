###########################################################################
####  Useful functions. Source:https://github.com/mrc-ide/eppasm/blob/master/R/read-spectrum-files.R  ####
###########################################################################

get_dp_version <- function(dp){
  
  ## Check the following tags to identify the version:
  ## * <General 3>: 2013, 2014 Spectrum files;
  ## * <General5>: 2015 Spectrum files
  ## * <FirstYear MV>: 2016 Spectrum file
  ## * <FirstYear MV2>: 2017+ spectrum file
  
  exists_dptag <- function(tag, tagcol=1){tag %in% dp[,tagcol]}
  
  dp.vers <- if (exists_dptag("<General 3>")) {
    "<General 3>"
  } else if (exists_dptag("<General5>")) {
    "<General5>"
  } else if (exists_dptag("<FirstYear MV>")) {
    "Spectrum2016"
  } else if (exists_dptag("<FirstYear MV2>")) {
    "Spectrum2017"
  } else {
    stop("Spectrum DP file version not recognized. Package probably needs to be updated to most recent Spectrum version.")
  }
  
  return(dp.vers)
}


read_dp <- function(pjnz, use_ep5 = FALSE){
  
  if(use_ep5) {
    dpfile <- grep(".ep5$", unzip(pjnz, list=TRUE)$Name, value=TRUE)
  } else {
    dpfile <- grep(".DP$", unzip(pjnz, list=TRUE)$Name, value=TRUE)
  }
  
  dp <- vroom::vroom(unz(pjnz, dpfile), delim = ",",
                     col_types = vroom::cols(.default = vroom::col_character()),
                     .name_repair = "minimal", progress = FALSE)
  dp <- as.data.frame(dp)
  
  return(dp)
}
 
read_pjn <- function(pjnz){
  pjnfile <- grep(".PJN$", unzip(pjnz, list=TRUE)$Name, value=TRUE)
  pjn <- vroom::vroom(unz(pjnz, pjnfile), delim = ",",
                      col_types = vroom::cols(.default = vroom::col_character()),
                      .name_repair = "minimal", progress = FALSE)
  pjn <- as.data.frame(pjn)
  
  return(pjn)
}

read_region <- function(pjnz){
  pjn <- read_pjn(pjnz)
  region <- pjn[which(pjn[,1] == "<Projection Parameters - Subnational Region Name2>")+2, 4]
  if(is.na(region))
    return(NULL)
  else
    return(region)
}

read_country <- function(pjnz){
  pjn <- read_pjn(pjnz)
  cc <- as.integer(pjn[which(pjn[,1] == "<Projection Parameters>")+2, 4])
  return(with(spectrum5_countrylist, Country[Code == cc]))
}

read_iso3 <- function(pjnz){
  pjn <- read_pjn(pjnz)
  cc <- as.integer(pjn[which(pjn[,1] == "<Projection Parameters>")+2, 4])
  return(with(spectrum5_countrylist, iso3[Code == cc]))
}

##' sub-setting function 
dpsub <- function(tag, rows, cols, tagcol=1){ 
  dp[which(dp[,tagcol]==tag)+rows, cols]
}

