####################
##' This code extracts Spectrum data from .DP files. 
##' Source: https://github.com/mrc-ide/eppasm/blob/master/R/read-spectrum-files.R 
#################### 

#---- functions for code ----
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

# find pjnz files by country
read_fun <- function (country) {
  
  for (i in seq_along(country)) { 
    #source(paste0(code_dir,'/0.5 functions.R'), local = TRUE)
    
    fls <- list.files(path = code_dir_country,pattern = "pjnz", ignore.case = T,recursive = T) 
    countryindex = grep(paste0(country[i],"_2"),fls,ignore.case = T)
    fls = fls[countryindex]
  }
  fls = "spectrum_files_2024/"%s+%fls
  return(fls)
}

# find PJNZ files for regional country files
read_fun_2030 <- function (country) {
  
  for (i in seq_along(country)) { 
    #source(paste0(code_dir,'/0.5 functions.R'), local = TRUE)
    
    fls <- list.files(path = code_dir_country,pattern = "pjnz", ignore.case = T,recursive = T) 
    
    countryindex = grep(paste0(country[i]),fls,ignore.case = T)
    if(country[i] == 'Zimbabwe'){
      countryindex = grep("zw",fls,ignore.case = T)
    }
    fls = fls[countryindex]
  }
  fls = "spectrum_files_2024/"%s+%fls
  return(fls)
}


#---- load packages and files ----
library(devtools)
library(rlang)
library(purrr)
library(dplyr)
library(stringr)
library(stringi)


outputs_dir <- here::here('anc testing/data')

code_dir_country = here::here("spectrum_files_2024/")
country_names <- c('Angola', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 'Cabo Verde', 'Cameroon', 'Central African Republic', 
                   'Chad', 'cotedivoire', 'Comoros', 'Congo', 'DRC', 'Equatorial Guinea', 'Eritrea', 'Eswatini', 'Gabon', 'Gambia', 'Ghana',
                   'Guinea', 'Guinea Bissau', 'Lesotho', 'Liberia' ,'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique',
                   'Namibia','Nigeria', 'Niger',  'Rwanda', 'Senegal', 'Sierra Leone',  'South Africa', 'South Sudan', 'STP', 'Tanzania', 
                   'Togo', 'Uganda', 'Zambia' )

country_names2 <- c('Kenya', 'Zimbabwe', 'Ethiopia') ## these countries have region-specific pjnz files 

data_files <- sapply(country_names, read_fun, simplify = T)
data_files2 <-sapply(country_names2, read_fun_2030, simplify = T)

#identify missing files based on country
which(sapply(data_files, function(x) identical(x, character(0))))
which(sapply(data_files2, function(x) identical(x, character(0))))

########################################################################################################
####  Function to read PMTCT outputs: ANC testing, PMTCT interventions, PMTCT transmission probabilities  ####
#####################################################################################################
subset_dts <- function( pjnz) {
  
  ##' sub-setting function 
  options(warn=1)
  print(pjnz)
  
  
  
  ##' read the dataset . DP
  dp <- read_dp(pjnz)
  dp.vers <- get_dp_version(dp)
  
  #' sub-setting function 
  dpsub <- function(tag, rows, cols, tagcol=1){ 
    dp[which(dp[,tagcol]==tag)+rows, cols]
  }
  
  
  ##' create labels 
  dplab <- function(tag, rows, cols, tagcol=1){ 
    if (!all(is.na(dp$Notes))) { 
      nts <- as.character(dp[which( dp[,tagcol]==tag)+rows, "Notes"])
      nts[is.na(nts)] <- "NA"
      nts <- make.unique(nts)}
    return(nts)
  }
  
  ##' identify the number of rows to subset for 
  rows <- function (tag, tagcol=1) { 
    start.x <- which( dp[,tagcol] %in% tag)
    end.x <- which (dp  == "<End>")
    end.x <-   end.x[ min (which(end.x > start.x) )]
    dp3 <- dp[  start.x: end.x,]
    row.x <- nrow(dp3) 
    return (row.x)
  }

  exists_dptag <- function(tag, tagcol=1){tag %in% dp[,tagcol]}

  ## projection parameters
  if(dp.vers %in% c("<General 3>", "<General5>")){
    yr_start <- as.integer(dp[which(dp[,2] == "First year")+1,4])
    yr_end <- as.integer(dp[which(dp[,2] == "Final year")+1,4])
  } else if(dp.vers == "Spectrum2016"){
    yr_start <- as.integer(dpsub("<FirstYear MV>",3,4))
    yr_end <- as.integer(dpsub("<FinalYear MV>",3,4))
  } else if(dp.vers == "Spectrum2017"){
    yr_start <- as.integer(dpsub("<FirstYear MV2>",2,4))
    yr_end <- as.integer(dpsub("<FinalYear MV2>",2,4))
    t0 <- as.numeric(dpsub("<FirstYearOfEpidemic MV>",2,4))
  }
  proj.years <- yr_start:yr_end
  timedat.idx <- 4+1:length(proj.years)-1
  yr_start = 1975
  yr_end = 2025
  
  
  '%ni%' <- Negate('%in%')
  ## Subset to only specific tags 
  anc_indicators <- c("anc_clients", "anc_tested", "anc_tested_pos", "anc_known_pos", "anc_known_neg")
  if (exists_dptag("<ANCTestingValues MV>")) {
    anc_testing <- dpsub("<ANCTestingValues MV>", 2:5, timedat.idx)
    anc_testing <- sapply(anc_testing, as.integer)
    dimnames(anc_testing) <- list(indicator = anc_indicators[1:4], year = proj.years)
  } else if (exists_dptag("<ANCTestingValues MV2>")) {
    anc_testing <- dpsub("<ANCTestingValues MV2>", 2:5, timedat.idx)
    anc_testing <- sapply(anc_testing, as.integer)
    dimnames(anc_testing) <- list(indicator = anc_indicators[1:4], year = proj.years)
  } else if (exists_dptag("<ANCTestingValues MV4>")) {
    anc_testing <- dpsub("<ANCTestingValues MV4>", c(2:5, 10), timedat.idx)
    anc_testing <- sapply(anc_testing, as.integer)
    dimnames(anc_testing) <- list(indicator = anc_indicators, year = proj.years)
  } else {
    stop("ANC testing inputs not found in .DP file")
  }
  anc_testing[anc_testing == -9999] <- NA_real_
  
  if (exists_dptag("<Births MV>")) {
    births <- dpsub("<Births MV>", 2, timedat.idx)
    births <- sapply(births, as.integer)
    names(births) <- proj.years
  }
  
  ## number of pregnant women living with HIV (needing PMTCT )
  if ( exists_dptag("<ChildNeedPMTCT MV>")) {  
    row.x <- rows ("<ChildNeedPMTCT MV>")-2
    pregwlhiv<- dpsub("<ChildNeedPMTCT MV>", 2:row.x, timedat.idx)
    names(pregwlhiv) <- ( proj.years)
  }
  
  
  ## number of pregnant women living with HIV receiving PMTCT 
  if ( exists_dptag("<ChildOnPMTCT MV>")) {  
    row.x <- rows("<ChildOnPMTCT MV>")-2
    receivepmtct <- dpsub("<ChildOnPMTCT MV>", 2:row.x, timedat.idx)
    names(receivepmtct ) <- ( proj.years)
    
  }
  

  
  
  
  anc_testing <- as.data.frame(anc_testing) %>% select(`2000`:`2030`)
  pregwlhiv<- pregwlhiv %>% select(`1970`:`2030`)
  receivepmtct <- receivepmtct %>% select(`1970`:`2030`)
  
  pmtct <- list("anc_test" = anc_testing,
                "needpmtct" = pregwlhiv,
                "receivepmtct" = receivepmtct,
                "births" = births)
 
  return(   pmtct )
}
data_files = data_files[lengths(data_files)>0]
data_files2 = data_files2[lengths(data_files2)>0]

pmtct_list  <- sapply(data_files, subset_dts, simplify = F)
#pmtct_hold = pmtct_list
pmtct_list_k  <- sapply(data_files2$Kenya, subset_dts, simplify = F)
pmtct_list_z  <- sapply(data_files2$Zimbabwe, subset_dts, simplify = F)
pmtct_list_e  <- sapply(data_files2$Ethiopia, subset_dts, simplify = F)




##################################################################################################################################################################
####  function to read HIV projection parameters: ASFR, TFR,  Fertility reduction due to HIV  ####
######################################################

read_hivdemo_param <- function(pjnz, use_ep5=FALSE){
  options(warn=1)
  print(pjnz)
  ## read .DP file
  
  dp <- read_dp(pjnz, use_ep5)
  #' sub-setting function 
  dpsub <- function(tag, rows, cols, tagcol=1){ 
    dp[which(dp[,tagcol]==tag)+rows, cols]
  }
  
  if(use_ep5) {
    dp.vers <- "Spectrum2017"
  } else {
    dp.vers <- get_dp_version(dp)
  }
  exists_dptag <- function(tag, tagcol=1){tag %in% dp[,tagcol]}
  vers_str <- dpsub("<ValidVers MV>", 2, 4)

  
  if(dp.vers %in% c("<General 3>", "<General5>")){
    version <- as.numeric(dp[which(dp[,2] == "Version")+1,4])
    validdate <- dp[which(dp[,1] == "<ValidDate>")+2,3]
    validversion <- as.numeric(dp[which(dp[,1] == "<ValidVers>")+2,4])
  } else if(dp.vers == "Spectrum2016") {
    version <- as.numeric(dpsub("<VersionNum MV>", 2, 4))
    validdate <- dpsub("<ValidDate MV>",2,3)
    validversion <- dpsub("<ValidVers MV>",2,4)
  } else if(dp.vers == "Spectrum2017") {
    version <- as.numeric(dpsub("<VersionNum MV2>", 3, 4))
    validdate <- dpsub("<ValidDate MV>",2,3)
    validversion <- dpsub("<ValidVers MV>",2,4)
  }
  
  
  ## find tag indexes (tidx)
  if(dp.vers == "<General 3>"){
    aids5.tidx <- which(dp[,1] == "<AIDS5>")
  } else if(dp.vers == "<General5>"){
    hivtfr.tidx <- which(dp[,1] == "<HIVTFR2>")
    hivsexrat.tidx <- which(dp[,1] == "<HIVSexRatio>")
    hivagedist.tidx <- which(dp[,1] == "<HIVDistribution2>")
  }
  
  if(dp.vers %in% c("<General 3>", "<General5>")){
    nathist.tidx <- which(dp[,1] == "<AdultTransParam2>")
    cd4initdist.tidx <- which(dp[,1] == "<DistNewInfectionsCD4>")
    infectreduc.tidx <- which(dp[,1] == "<InfectReduc>")
    adult.artnumperc.tidx <- which(dp[,1] == "<HAARTBySexPerNum>")
    adult.art.tidx <- which(dp[,1] == "<HAARTBySex>")
    adult.arteligthresh.tidx <- which(dp[,1] == "<CD4ThreshHoldAdults>")
    specpopelig.tidx <- which(dp[,1] == "<PopsEligTreat1>")
  }
  
  ## state space dimensions
  NG <- 2
  AG <- 17
  DS <- 7
  TS <- 3
  
  ## projection parameters
  if(dp.vers %in% c("<General 3>", "<General5>")){
    yr_start <- as.integer(dp[which(dp[,2] == "First year")+1,4])
    yr_end <- as.integer(dp[which(dp[,2] == "Final year")+1,4])
  } else if(dp.vers == "Spectrum2016"){
    yr_start <- as.integer(dpsub("<FirstYear MV>",3,4))
    yr_end <- as.integer(dpsub("<FinalYear MV>",3,4))
  } else if(dp.vers == "Spectrum2017"){
    yr_start <- as.integer(dpsub("<FirstYear MV2>",2,4))
    yr_end <- as.integer(dpsub("<FinalYear MV2>",2,4))
  }
  proj.years <- yr_start:yr_end
  timedat.idx <- 4+1:length(proj.years)-1
  
  #Fertility multiplier by age for HIV+ women  OFF  ART
 
  if(dp.vers == "<General 3>"){
    fert_notart<- as.numeric(dp[which(dp[,1] == "<AIDS5>")+185, 4+0:6])
    fert_notart <- array(rep(fert_notart, length(proj.years)), c(7, length(proj.years)))
    dimnames(fert_notart) <- list(agegr=seq(15, 45, 5), year=proj.years)
  } else if(dp.vers == "<General5>") {
    fert_notart <- sapply(dp[hivtfr.tidx+2:8, 3+seq_along(proj.years)], as.numeric)
    dimnames(fert_notart) <- list(agegr=seq(15, 45, 5), year=proj.years)
  } else if(dp.vers == "Spectrum2016") {
    fert_notart <- sapply(dpsub("<HIVTFR MV>", 2:8, timedat.idx), as.numeric)
    dimnames(fert_notart) <- list(agegr=seq(15, 45, 5), year=proj.years)
  } else if(exists_dptag("<HIVTFR MV2>")) {
    fert_notart<- sapply(dpsub("<HIVTFR MV2>", 2:7, timedat.idx), as.numeric)
    dimnames(fert_notart) <- list(agegr=c(15, 18, seq(20, 35, 5)), year=proj.years)  # this version of Spectrum stratified fertility reduction by 15-17, 18-19, 20-24, ...
  } else if(exists_dptag("<HIVTFR MV3>")) {
    fert_notart <- sapply(dpsub("<HIVTFR MV3>", 2:8, timedat.idx), as.numeric)
    dimnames(fert_notart) <- list(agegr=seq(15, 45, 5), year=proj.years)
  } else if(exists_dptag("<HIVTFR MV4>")) {
    fert_notart <- vapply(dpsub("<HIVTFR MV4>", 2:8, c(timedat.idx)), as.numeric, numeric(7))
    dimnames(fert_notart) <- list(agegr=seq(15, 45, 5), year=proj.years)
  }
  
  #  Fertility multiplier by age for HIV+ women ON ART 
  if(exists_dptag("<RatioWomenOnART MV>"))
    frr_onart1 <- rep(as.numeric(dpsub("<RatioWomenOnART MV>", 2, 4)), 7)
  else if(exists_dptag("<RatioWomenOnART MV2>"))
    frr_onart1  <- as.numeric(dpsub("<RatioWomenOnART MV2>", 2, 4+0:6))
  else
    { frr_onart1  <- rep(1.0, 7) } 
  
   names( frr_onart1) <- seq(15, 45, 5)

   fert_onart <- matrix(data = (NA), nrow = nrow(  fert_notart ), ncol =ncol(  fert_notart ))
   dimnames(  fert_onart )<- list(agegr=seq(15, 45, 5), year=proj.years)
   fert_onart [] <- frr_onart1



   # Fertility multiplier by  CD4 count for HIV+ women off ART
   if(dp.vers == "Spectrum2017") {
     cd4fert_rat <- as.numeric(dpsub("<FertCD4Discount MV>", 2, 4+1:DS))
   } else {
     cd4fert_rat <- rep(1.0, DS)
   }
  
  ## births among all women ( not just HIV positive women )
  if(exists_dptag("<Births MV>")) {
    tot_births <- vapply(dpsub("<Births MV>", 2, timedat.idx), as.numeric, numeric(1))
    names(tot_births) <- proj.years
  }

   
  ## asfr calculation 
  tfr.tidx <- which(dp[,1] == "<TFR MV>")
  asfd.tidx <- which(dp[,1] == "<ASFR MV>")
  
  tfr <- setNames(as.numeric(dp[tfr.tidx + 2, timedat.idx]), proj.years)
  
  asfd <- sapply(dp[asfd.tidx + 3:9, timedat.idx], as.numeric)/100 ## divide everything by 100 to get a proportion
  asfd <- apply(asfd / 5, 2, rep, each=5) ## this code divides everything by 5 to give us asfd by each year 


  ## Internally, Spectrum normalizes the ASFR before multiplying by TFR
  asfd_sum <- colSums(asfd) ##sum each column, should add up to 1 since each one is a percent 
  asfd_sum[asfd_sum == 0.0] <- 1.0
  asfd <- sweep(asfd, 2, asfd_sum, "/") ## divide each ASFR value by the sum of asfrs. now colsums(asfd) = EXACTLY 1. 
  
  dimnames(asfd) <- list(age=15:49, year=proj.years)

  asfr <- sweep(asfd, 2, tfr, "*")  # then multiply this asfd by tfr to get the final asfr. 
 # Souce: 
 # https://github.com/mrc-ide/eppasm/blob/62e6b0fdfddedb31d4123e611bbc2e15908e39a3/R/spectrum.R#L648-L656 
 
  ## gives us the output of tfr and asfr as the spectrum uses it 
  
  projp <- list("yr_start" = yr_start,
                "yr_end" = yr_end,
                "spectrum_version" = vers_str,
                "fert_notart[hiv+]" = fert_notart, ## fertility multiplier for women NOT on ART 
                "fert_onart[hiv+]" = fert_onart, ## fertility multiplier for women ON ART 
                "cd4fert_rat" =cd4fert_rat, ## Ratio of fertility rate among WLHIV to the fertility rate of women not living with HIV by CD4 cell count category
                "asfr" = asfr, 
                "tfr" = tfr,
                "tot_births" = tot_births
                )
  class(projp) <- "projp"
  attr(projp, "version") <- version
  attr(projp, "validdate") <- validdate
  attr(projp, "validversion") <- validversion
  
  return(projp)
}

hivdemo_proj_dt <- sapply(data_files, read_hivdemo_param, simplify = F)

hivdemo_proj_dt_k <- sapply(data_files2$Kenya, read_hivdemo_param, simplify = F)
hivdemo_proj_dt_z <- sahivdemo_proj_dt_z <- sahivdemo_proj_dt_z <- sapply(data_files2$Zimbabwe, read_hivdemo_param, simplify = F)
hivdemo_proj_dt_e <- sapply(data_files2$Ethiopia, read_hivdemo_param, simplify = F)

# create proper PMTCT and HIV demo proj for reginal pjnz files

## ethiopia
Ethiopia = as.list(list.files(path = "spectrum_files_2024/ESA/Ethiopia",full.names = T))
pop_tot =0
pop_list = list()
for (i in 1:length(Ethiopia)) {
  pop_tot = pop_tot + apply(specio::read_total_pop(pjnz_path = Ethiopia[[i]])[15:49,2,],2,sum)
  pop_list[[i]] = apply(specio::read_total_pop(pjnz_path = Ethiopia[[i]])[15:49,2,],2,sum)
}

pop_year= 0
pop_list2 = list()
for (i in 1:length(pop_list)) {
  pop_list2[[i]] = pop_list[[i]]/pop_tot
  
  pop_year = pop_year + pop_list2[[i]] }

pop_year/pop_tot

asfr = list()
tfr = list()
anc_test = list()
anc_test_tot = 0
births = list()
births_tot = 0
asfr_tot = 0
need_pmtct = list()
need_pmtct_tot = rep(0,61)
receivepmtct = list()
receivepmtct_tot = rep(0,61)

for (i in 1:length(hivdemo_proj_dt_e)) {
  asfr[[i]] = hivdemo_proj_dt_e[[i]]$asfr[,] * pop_list2[[i]]
  asfr_tot = asfr_tot + asfr[[i]]
  tfr[[i]] = hivdemo_proj_dt_e[[i]]$tfr* pop_list2[[i]]
  anc_test[[i]] = pmtct_list_e[[i]]$anc_test
  anc_test_tot = anc_test_tot+pmtct_list_e[[i]]$anc_test
  births[[i]] = pmtct_list_e[[i]]$births
  births_tot = births_tot+pmtct_list_e[[i]]$births
  need_pmtct[[i]] = pmtct_list_e[[i]]$needpmtct
  need_pmtct_tot = need_pmtct_tot + as.numeric(need_pmtct[[i]])
  receivepmtct[[i]] = pmtct_list_e[[i]]$receivepmtct
  receivepmtct_tot = receivepmtct_tot + as.numeric(receivepmtct[[i]])
  
}
anc_test_tot
tfr_tot = 0
for(i in 1:length(tfr)){
  tfr_tot = tfr_tot + tfr[[i]]
}
hivdemo_proj_dt_e_full = hivdemo_proj_dt_e$`spectrum_files_2024/ESA/Ethiopia/Ethiopia Benis Feb 28_2024.PJNZ`
hivdemo_proj_dt_e_full$asfr = asfr_tot
pmtct_e_full = pmtct_list_e$`spectrum_files_2024/ESA/Ethiopia/Ethiopia Benis Feb 28_2024.PJNZ`
pmtct_e_full$anc_test = anc_test_tot
pmtct_e_full$births = births_tot
pmtct_e_full$receivepmtct = receivepmtct_tot
pmtct_e_full$needpmtct = need_pmtct_tot

# ---zimbabwe----

Zimbabwe = as.list(list.files(path = "spectrum_files_2024/ESA/Zimbabwe 2024 05 31/",full.names = T))
pop_tot =0
pop_list = list()
for (i in 1:length(Zimbabwe)) {
  pop_tot = pop_tot + apply(specio::read_total_pop(pjnz_path = Zimbabwe[[i]])[15:49,2,],2,sum)
  pop_list[[i]] = apply(specio::read_total_pop(pjnz_path = Zimbabwe[[i]])[15:49,2,],2,sum)
}

pop_year= 0
pop_list2 = list()
for (i in 1:length(pop_list)) {
  pop_list2[[i]] = pop_list[[i]]/pop_tot
  
  pop_year = pop_year + pop_list2[[i]] }

pop_year/pop_tot

asfr = list()
tfr = list()
anc_test = list()
anc_test_tot = 0
births = list()
births_tot = 0
asfr_tot = 0
need_pmtct = list()
need_pmtct_tot = rep(0,61)
receivepmtct = list()
receivepmtct_tot = rep(0,61)

for (i in 1:length(hivdemo_proj_dt_z)) {
  asfr[[i]] = hivdemo_proj_dt_z[[i]]$asfr[,] * pop_list2[[i]]
  asfr_tot = asfr_tot + asfr[[i]]
  tfr[[i]] = hivdemo_proj_dt_z[[i]]$tfr* pop_list2[[i]]
  anc_test[[i]] = pmtct_list_z[[i]]$anc_test
  anc_test_tot = anc_test_tot+pmtct_list_z[[i]]$anc_test
  births[[i]] = pmtct_list_z[[i]]$births
  births_tot = births_tot+pmtct_list_z[[i]]$births
  need_pmtct[[i]] = pmtct_list_z[[i]]$needpmtct
  need_pmtct_tot = need_pmtct_tot + as.numeric(need_pmtct[[i]])
  receivepmtct[[i]] = pmtct_list_z[[i]]$receivepmtct
  receivepmtct_tot = receivepmtct_tot + as.numeric(receivepmtct[[i]])
  
}

tfr_tot = 0
for(i in 1:length(tfr)){
  tfr_tot = tfr_tot + tfr[[i]]
}
hivdemo_proj_dt_z_full = hivdemo_proj_dt_z$`spectrum_files_2024/ESA/Zimbabwe 2024 05 31/ZW_Bulawayo_2024.PJNZ`
hivdemo_proj_dt_z_full$asfr = asfr_tot
pmtct_z_full = pmtct_list_z$`spectrum_files_2024/ESA/Zimbabwe 2024 05 31/ZW_Bulawayo_2024.PJNZ`
pmtct_z_full$anc_test = anc_test_tot
pmtct_z_full$births = births_tot
pmtct_z_full$receivepmtct = receivepmtct_tot
pmtct_z_full$needpmtct = need_pmtct_tot

# ----kenya----

Kenya = as.list(list.files(path = "spectrum_files_2024/ESA/Kenya 2024 06 17",full.names = T))
Kenya = Kenya[3:10]
pop_tot =0
pop_list = list()
for (i in 1:length(Kenya)) {
  pop_tot = pop_tot + apply(specio::read_total_pop(pjnz_path = Kenya[[i]])[15:49,2,],2,sum)
  pop_list[[i]] = apply(specio::read_total_pop(pjnz_path = Kenya[[i]])[15:49,2,],2,sum)
}

pop_year= 0
pop_list2 = list()
for (i in 1:length(pop_list)) {
  pop_list2[[i]] = pop_list[[i]]/pop_tot
  
  pop_year = pop_year + pop_list2[[i]] }

pop_year/pop_tot

asfr = list()
tfr = list()
anc_test = list()
anc_test_tot = 0
births = list()
births_tot = 0
asfr_tot = 0
need_pmtct = list()
need_pmtct_tot = rep(0,61)
receivepmtct = list()
receivepmtct_tot = rep(0,61)

for (i in 1:length(hivdemo_proj_dt_k)) {
  asfr[[i]] = hivdemo_proj_dt_k[[i]]$asfr[,] * pop_list2[[i]]
  asfr_tot = asfr_tot + asfr[[i]]
  tfr[[i]] = hivdemo_proj_dt_k[[i]]$tfr* pop_list2[[i]]
  anc_test[[i]] = pmtct_list_k[[i]]$anc_test
  anc_test_tot = anc_test_tot+pmtct_list_k[[i]]$anc_test
  births[[i]] = pmtct_list_k[[i]]$births
  births_tot = births_tot+pmtct_list_k[[i]]$births
  need_pmtct[[i]] = pmtct_list_k[[i]]$needpmtct
  need_pmtct_tot = need_pmtct_tot + as.numeric(need_pmtct[[i]])
  receivepmtct[[i]] = pmtct_list_k[[i]]$receivepmtct
  receivepmtct_tot = receivepmtct_tot + as.numeric(receivepmtct[[i]])
  
}

tfr_tot = 0
for(i in 1:length(tfr)){
  tfr_tot = tfr_tot + tfr[[i]]
}
hivdemo_proj_dt_k_full = hivdemo_proj_dt_k$`spectrum_files_2024/ESA/Kenya 2024 06 17/Kenya-Central_19Apr2024_KOS.PJNZ`  
hivdemo_proj_dt_k_full$asfr = asfr_tot
pmtct_k_full = pmtct_list_k$`spectrum_files_2024/ESA/Kenya 2024 06 17/Kenya-Central_19Apr2024_KOS.PJNZ`
pmtct_k_full$anc_test = anc_test_tot
pmtct_k_full$births = births_tot
pmtct_k_full$receivepmtct = receivepmtct_tot
pmtct_k_full$needpmtct = need_pmtct_tot

pmtct_list$Kenya = pmtct_k_full
pmtct_list$Ethiopia = pmtct_e_full
pmtct_list$Zimbabwe = pmtct_z_full

hivdemo_proj_dt$Kenya = hivdemo_proj_dt_k_full
hivdemo_proj_dt$Ethiopia = hivdemo_proj_dt_e_full
hivdemo_proj_dt$Zimbabwe = hivdemo_proj_dt_z_full


# ---- correct erorrs burundi ----
#pmtct has weird values(0s instead of NA's), set 2014-2017 to NA
pmtct_list$Burundi$anc_test$`2014` = c(NA,NA,NA,NA,NA)
pmtct_list$Burundi$anc_test$`2015` = c(NA,NA,NA,NA,NA)
pmtct_list$Burundi$anc_test$`2016` = c(NA,NA,NA,NA,NA)
pmtct_list$Burundi$anc_test$`2017` = c(NA,NA,NA,NA,NA)

# REPLACE GUNIEAS MISSING DATA WITH UNAIDS reported HIV testing of pregnant women #https://aidsinfo.unaids.org/
# they report 2014 = 69% 2015 = 47%, 2016 = 64%, 2017 = 66%, 2018 = 89% 
pmtct_list$Guinea$receivepmtct[c("2014","2015","2016","2017","2018")] = 
  as.numeric(pmtct_list$Guinea$needpmtct[c("2014","2015","2016","2017","2018")])*c(0.69,0.47,0.64,0.66,0.89)

#excplicitly align and sort pmtct and hiv demoproj into alphabetical order
names(pmtct_list)[which(names(pmtct_list) == "Cabo Verde")] = "Cape Verde"
names(hivdemo_proj_dt)[which(names(hivdemo_proj_dt) == "Cabo Verde")] = "Cape Verde"
names(pmtct_list)[which(names(pmtct_list) == "cotedivoire")] = "Côte d'Ivoire"
names(hivdemo_proj_dt)[which(names(hivdemo_proj_dt) == "cotedivoire")] = "Côte d'Ivoire"
names(pmtct_list)[which(names(pmtct_list) == "DRC")] = "Democratic Republic of the Congo"
names(hivdemo_proj_dt)[which(names(hivdemo_proj_dt) == "DRC")] = "Democratic Republic of the Congo"
names(pmtct_list)[which(names(pmtct_list) == "Eswatini")] = "Swaziland"
names(hivdemo_proj_dt)[which(names(hivdemo_proj_dt) == "Eswatini")] = "Swaziland"
names(pmtct_list)[which(names(pmtct_list) == "Tanzania")] = "United Republic of Tanzania"
names(hivdemo_proj_dt)[which(names(hivdemo_proj_dt) == "Tanzania")] = "United Republic of Tanzania"
names(pmtct_list)[which(names(pmtct_list) == "Guinea Bissau")] = "Guinea-Bissau"
names(hivdemo_proj_dt)[which(names(hivdemo_proj_dt) == "Guinea Bissau")] = "Guinea-Bissau"
names(pmtct_list)[which(names(pmtct_list) == "Swaziland")] = "eSwatini"
names(hivdemo_proj_dt)[which(names(hivdemo_proj_dt) == "Swaziland")] = "eSwatini"



hivdemo_proj_dt <- hivdemo_proj_dt[order(names(hivdemo_proj_dt))]
pmtct_list <- pmtct_list[order(names(pmtct_list))]

saveRDS(hivdemo_proj_dt, file = paste0 (outputs_dir,'/hivdemo_proj_dt_cnt.rds'))
saveRDS(pmtct_list, file = paste0 (outputs_dir ,'/pmtct_list_cnt.rds'))



