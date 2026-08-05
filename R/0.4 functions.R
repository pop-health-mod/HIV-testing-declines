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

sex_to_gender <- function(sex){
  gender <- ifelse(tolower(sex) == "female","Women",ifelse(tolower(sex) == "male","Men",stop("missing sex")))
  gender
  }


# extract arbitrary
pool_age_groups <- function(obj_all,
                            age_grps  = c("15-24","25-34", "35-49", "50-99"),
                            sex_grps = c("male","female"),
                            age_label = paste(age_grps, collapse = "+"),
                            sex_label = paste(sex_grps, collapse = "+")) {
  
  target_order <- c("year", "age", "sex", "time_dx_avg", "time_dx_med",
                    "prb6mo", "prb1yr", "prb2yr", "prb5yr", "prb500cd4",
                    "prb350cd4", "propdx", "sampi", "w")
  
  obj_all <- obj_all[order(names(obj_all))]
  
  lapply(obj_all, function(cnt_obj) {
    dt <- data.table::as.data.table(cnt_obj$out_simul_tdx_all$groups)[age %in% age_grps & sex %in% sex_grps]
    
    pooled <- dt[, .(
      age         = age_label,
      sex         = sex_label,
      time_dx_avg = weighted.mean(time_dx_avg, w),
      time_dx_med = weighted.mean(time_dx_med, w),
      prb6mo      = weighted.mean(prb6mo,    w),
      prb1yr      = weighted.mean(prb1yr,    w),
      prb2yr      = weighted.mean(prb2yr,    w),
      prb5yr      = weighted.mean(prb5yr,    w),
      prb500cd4   = weighted.mean(prb500cd4, w),
      prb350cd4   = weighted.mean(prb350cd4, w),
      propdx      = weighted.mean(propdx,    w),
      w           = sum(w)
    ), by = .(year, sampi)]
    
    pooled[, `:=`(age = as.character(age), sex = as.character(sex))]
    data.table::setcolorder(pooled, target_order)
    data.table::setkey(pooled, year)
    list(out_simul_tdx_all = pooled)
  })
}

# extracts pooled objects
extract_pooled <- function(obj_all) {
  obj_all <- obj_all[order(names(obj_all))]          # alphabetical countries
  lapply(obj_all, function(cnt_obj) {
    dt <- data.table::as.data.table(cnt_obj$out_simul_tdx_all$pooled)
    list(out_simul_tdx_all = dt)
  })
}

