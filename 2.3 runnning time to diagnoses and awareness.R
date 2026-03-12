source(paste0(here::here("anc testing"), "/0.6 time dx functions.R"))
source(paste0(here::here("anc testing"), "/0.5 simul-aware-functions.R"))

counter_year <- read_rds("anc testing/data/counter_years.rds")
counter_anc_years <- read_rds("anc testing/data/counter_anc_years.rds")

library(Matrix)
library(first90)
library(tidyverse)
library(Rcpp)

path_anc <- here::here("anc testing")
make_country = readRDS(paste0(path_anc, "/data/make_country_simul_final.rds"))

# extract the PMTCT information from the Spectrum files
pmtct_list <- readRDS(paste0(path_anc, "/data/pmtct_list_cnt.rds"))

# extract the HIV/demographic projections
hivdemo_proj_list <-
  readRDS(paste0(path_anc, "/data/hivdemo_proj_dt_cnt.rds"))


#define time to diagnoses lists
tdx_agg_simul_male = list()
tdx_agg_simul_female = list()
tdx_agg_simul_both = list()

tdx_agg_simul_male_counter = list()
tdx_agg_simul_female_counter = list()
tdx_agg_simul_both_counter = list()

aware_agg_simul_male = list()
aware_agg_simul_female = list()
aware_agg_simul_both = list()

aware_agg_simul_male_counter = list()
aware_agg_simul_female_counter = list()
aware_agg_simul_both_counter = list()

# select countries to use, for chad it must be run with new parameters (see 2.0 simulating Shiny90)
cntlist = names(make_country)[c(-6)]

for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  print(cnt)
  
  #ensure all data frames are clean
  simul_b_counter = NULL
  simul_b_counter = NULL
  simul_b_counter = NULL
  
  simul_f = NULL
  simul_m = NULL
  simul_b = NULL
  
  simul_tdxM_counter = NULL
  simul_tdxF_counter = NULL
  simul_tdxB_counter = NULL
  
  simul_tdxM = NULL
  simul_tdxF = NULL
  simul_tdxB = NULL
  
  if (cnt %in% cntlist) {
    tryCatch(
      expr = {
        fp <- make_country[[cnt]]$fp
        
        pgrm_dat <-
          first90::select_prgmdata(make_country[[cnt]]$prgm_dat,
                                   cnt = cnt)
        
        survey_hts <-
          first90::select_hts(
            make_country[[cnt]]$survey_hts,
            cnt = cnt,
            age_group = c('15-24', '25-34', '35-49')
          )
        
        # define parameters by country
        samp = make_country[[cnt]]$samp
        opt = make_country[[cnt]]$opt
        mod = make_country[[cnt]]$mod
        
        # we select pmtct data
        pmtct <- pmtct_list[[cnt]]
        
        names(pmtct$receivepmtct) = as.character(1970:2030)
        names(pmtct$needpmtct) = as.character(1970:2030)
        
        # we select HIV demo proj
        hivdemo_proj <- hivdemo_proj_list[[cnt]]
        
        
        #clear all counter factual sample and pmtct
        samp_1 = NULL
        pmtct_1 = NULL
        
        if (!is.na(counter_years[counter_years$country == cnt,]$start)) {
          # define counterfactuals
          counter_years_loop = counter_years[counter_years$country == cnt,]
          
          # Apply counterfactual opperation to every row of samp
          samp_new <- function(samp, counter_years_loop) {
            stopifnot(is.matrix(samp) || is.data.frame(samp))
            
            samp <- as.matrix(samp)
            
            convert <- (ncol(samp) - 4) / 2
            
            start <- as.numeric(counter_years_loop[2])
            
            # if NA, end becomes 30(ie no recovery)
            end <- as.numeric(ifelse(
              !is.na(counter_years_loop[3]),
              (as.numeric(counter_years_loop[3]) - 30 - 1),
              30
            ))
            
            start_female <- start - 30
            start_dx     <- convert + (start - 40)
            end_dx       <- start_dx + (end - start_female)
            
            clamp <-
              function(i)
                max(1, min(ncol(samp), as.integer(i)))
            
            sf <- clamp(start_female)
            e1 <- clamp(min(end, convert))
            
            sd <- clamp(start_dx)
            e2 <- clamp(min(end_dx, convert + convert - 10))
            
            # apply to all rows
            if (sf <= e1)
              samp[, sf:e1] <- samp[, sf]
            if (sd <= e2)
              samp[, sd:e2] <- samp[, sd]
            
            samp
          }
          if (!is.na(counter_years_loop$start)) {
            samp_1 = samp_new(samp, counter_years_loop)
          } else{
            print("non-counter samp")
            samp_1 = samp
          }
          if (is.na(counter_years_loop$start) &
              is.na(counter_years[counter_years$country == cnt, 1:3]$start)) {
            print("skipping")
            next
          }
          
          #counterfactual ANC
          
          start_anc = counter_years_loop$start
          
          names(pmtct$anc_test) = as.character(2000:2030)
          names(pmtct$needpmtct) = as.character(1970:2030)
          names(pmtct$receivepmtct) = as.character(1970:2030)
          
          pmtct_1 = pmtct
          
          # find the number of years which have data, using testing data
          anc_indx_years <-
            pmtct$anc_test[!is.na(pmtct$anc_test[2, ])]
          anc_data_years <-
            as.integer(names(pmtct$anc_test)[!(is.na(pmtct$anc_test[2, ]) |
                                                 pmtct$anc_test[2, ] == 0)])
          anc_data_idx <-
            fp$ss$PROJ_YEARS - ((fp$ss$PROJ_YEARS + 1970 - 1) - anc_data_years)
          
          # Find valid columns where row 2 is not NA
          valid_cols <-
            which(!(is.na(pmtct$anc_test[2, ]) |
                      pmtct$anc_test[2, ] == 0))  # Faster than repeated indexing
          
          # anc tested and tested anc pos
          anc_tested <-
            as.integer(colSums(pmtct$anc_test[c(2, 5), valid_cols], na.rm = T))
          anc_pos <- as.integer(pmtct$anc_test[3, valid_cols])
          
          # Extract birth indices
          births_idx <- colnames(pmtct$anc_test)[valid_cols]
          
          # Compute ANC coverage,  nb women tested at anc1 / births
          anc_cov <- anc_tested / pmtct$births[births_idx]
          
          # Compute ANC positive proportion
          anc_pos_prop <- anc_pos / anc_tested
          
          anc_tests_prob = as.numeric(pmtct$receivepmtct) / as.numeric(pmtct$needpmtct)
          anc_tests_prob[is.na(anc_tests_prob)] <- 0
          names(anc_tests_prob) <-
            fp$ss$proj_start:(fp$ss$proj_start + fp$ss$PROJ_YEARS - 1)
          
          #if (any(anc_tests_prob > 1)) { print("check spectrum's pmtct inputs, prob of being tested at anc >100%") }
          anc_tests_prob[anc_tests_prob > 1] <-
            0.999 # or we could put a default of 95% (to discuss)
          
          # linear interpolation to remove any missing data
          if (length(anc_cov) != 0) {
            # apply known data
            anc_tests_prob[anc_data_idx] <- anc_cov
            
            # predict using artcov method past the date (for projection)
            anc_tests_prob[max(anc_data_idx):fp$ss$PROJ_YEARS] <-
              anc_tests_prob[max(anc_data_idx):fp$ss$PROJ_YEARS]
            
            
          }
          # identify years when ANC should be applied
          Years_anc = names(which(anc_tests_prob[start_anc] > anc_tests_prob[start_anc:61]))
          year_start = names(anc_tests_prob[start_anc])
          #fixes some issues with country files
          
          
          if(length(Years_anc)>0 ){
            # adjust anctest
            pmtct_1$anc_test[, Years_anc] = pmtct$anc_test[, which(names(pmtct$anc_test) == year_start)]
            # if needed adjust recive/need pmtct, all countries should have anctest but just incase one year is missing.
            pmtct_1$receivepmtct[which(names(pmtct$anc_test) == Years_anc)] = pmtct$receivepmtct[which(names(pmtct$anc_test) == year_start)]
            pmtct_1$needpmtct[which(names(pmtct$anc_test) == Years_anc)] = pmtct$needpmtct[which(names(pmtct$anc_test) == year_start)]
          }
        }
        
        
        # # time to diagnosis
        # simul_tdxF = simul_pool_time_dx_agg_prev(
        #   samp = samp,
        #   mod = make_country[[cnt]]$mod,
        #   fp = make_country[[cnt]]$fp,
        #   hivdemo_proj = hivdemo_proj_list[[cnt]],
        #   pmtct = pmtct_list[[cnt]],
        #   year = 2015:2023,
        #   std = F,
        #   age = c("15-24", "25-34", "35-49", "50-99"),
        #   sex = "female",
        #   parallel = T
        # )
        # print(1)
        # simul_tdxM = simul_pool_time_dx_agg_prev(
        #   samp = samp,
        #   mod = make_country[[cnt]]$mod,
        #   fp = make_country[[cnt]]$fp,
        #   hivdemo_proj = hivdemo_proj_list[[cnt]],
        #   pmtct = pmtct_list[[cnt]],
        #   year = 2015:2023,
        #   std = F,
        #   age = c("15-24", "25-34", "35-49", "50-99"),
        #   sex = "male",
        #   parallel = T
        # )
        # 
        # print(2)
        # 
        # simul_tdxB = simul_pool_time_dx_agg_prev(
        #   samp = samp,
        #   mod = make_country[[cnt]]$mod,
        #   fp = make_country[[cnt]]$fp,
        #   hivdemo_proj = hivdemo_proj_list[[cnt]],
        #   pmtct = pmtct_list[[cnt]],
        #   year = 2015:2023,
        #   std = F,
        #   age = c("15-24", "25-34", "35-49", "50-99"),
        #   sex = c("male", "female"),
        #   parallel = T
        # )
        # print(3)
        # 
        # tdx_agg_simul_male[[cnt]]$out_simul_tdx_all = simul_tdxM
        # tdx_agg_simul_female[[cnt]]$out_simul_tdx_all = simul_tdxF
        # tdx_agg_simul_both[[cnt]]$out_simul_tdx_all = simul_tdxB
        # 
        # make_country[[cnt]]$tdx_male$out_simul_tdx_all = simul_tdxM
        # make_country[[cnt]]$tdx_female$out_simul_tdx_all = simul_tdxF
        # make_country[[cnt]]$tdx_both$out_simul_tdx_all = simul_tdxB
        # 
        # saveRDS(tdx_agg_simul_male,
        #         paste0(here::here("outputs"),
        #                "/male time to dx.rda"))
        # saveRDS(tdx_agg_simul_female,
        #         paste0(here::here("outputs"),
        #                "/female time to dx.rda"))
        # saveRDS(tdx_agg_simul_both,
        #         paste0(here::here("outputs"),
        #                "/both time to dx.rda"))
        # gc()
        # 
        # counterfactual time to diagnosis
        if (!(is.null(samp_1))) {
          simul_tdxF_counter = simul_pool_time_dx_agg_prev(
            samp = samp_1,
            mod = make_country[[cnt]]$mod,
            fp = make_country[[cnt]]$fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_1,
            year = 2015:2023,
            std = F,
            age = c("15-24", "25-34", "35-49", "50-99"),
            sex = "female",
            parallel = T)
          
          print(4)
          
          simul_tdxM_counter = simul_pool_time_dx_agg_prev(
            samp = samp_1,
            mod = make_country[[cnt]]$mod,
            fp = make_country[[cnt]]$fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_1,
            year = 2015:2023,
            std = F,
            age = c("15-24", "25-34", "35-49", "50-99"),
            sex = "male",
            parallel = T)
          
          print(5)
          
          simul_tdxB_counter = simul_pool_time_dx_agg_prev(
            samp = samp_1,
            mod = make_country[[cnt]]$mod,
            fp = make_country[[cnt]]$fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_1,
            year = 2015:2023,
            std = F,
            age = c("15-24", "25-34", "35-49", "50-99"),
            sex = c("male", "female"),
            parallel = T)
          
          print(6)
          
          tdx_agg_simul_male_counter[[cnt]]$out_simul_tdx_all = simul_tdxM_counter
          tdx_agg_simul_female_counter[[cnt]]$out_simul_tdx_all = simul_tdxF_counter
          tdx_agg_simul_both_counter[[cnt]]$out_simul_tdx_all = simul_tdxB_counter
          
          make_country[[cnt]]$tdx_male_counter$out_simul_tdx_all = simul_tdxM_counter
          make_country[[cnt]]$tdx_female_counter$out_simul_tdx_all = simul_tdxF_counter
          make_country[[cnt]]$tdx_both_counter$out_simul_tdx_all = simul_tdxB_counter
          
          saveRDS(
            tdx_agg_simul_male_counter,
            paste0(
              here::here("outputs"),
              "/male time to dx counter.rda"
            )
          )
          saveRDS(
            tdx_agg_simul_female_counter,
            paste0(
              here::here("outputs"),
              "/female time to dx counter.rda"
            )
          )
          saveRDS(
            tdx_agg_simul_both_counter,
            paste0(
              here::here("outputs"),
              "/both time to dx counter.rda"
            )
          )
          gc()
        }
        
        simul_m <-
          simul_aware_agg(
            samp = samp,
            mod = make_country[[cnt]]$mod,
            fp = make_country[[cnt]]$fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = c(2015:2023),
            age = c("15-24", "25-34", "35-49", "50-99"),
            sex = c("male")
          )
        print(7)
        simul_f <-
          simul_aware_agg(
            samp = samp,
            mod = make_country[[cnt]]$mod,
            fp = make_country[[cnt]]$fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = c(2015:2023),
            age = c("15-24", "25-34", "35-49", "50-99"),
            sex = c("female")
          )
        print(8)
        simul_b <-
          simul_aware_agg(
            samp = samp,
            mod = make_country[[cnt]]$mod,
            fp = make_country[[cnt]]$fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = c(2015:2023),
            age = c("15-24", "25-34", "35-49", "50-99"),
            sex = c("male", "female")
          )
        print(9)
        
        aware_agg_simul_male[[cnt]]$out_simul_aware_all = simul_m
        aware_agg_simul_female[[cnt]]$out_simul_aware_all = simul_f
        aware_agg_simul_both[[cnt]]$out_simul_aware_all = simul_b
        
        make_country[[cnt]]$aware_male$out_simul_aware_all = simul_m
        make_country[[cnt]]$aware_female$out_simul_aware_all = simul_f
        make_country[[cnt]]$aware_both$out_simul_aware_all = simul_b
        
        saveRDS(aware_agg_simul_male,
                paste0(here::here("outputs"),
                       "/male aware.rda"))
        saveRDS(aware_agg_simul_female,
                paste0(here::here("outputs"),
                       "/female aware.rda"))
        saveRDS(aware_agg_simul_both,
                paste0(here::here("outputs"),
                       "/both aware.rda"))
        gc()
        
        
        if (!(is.null(samp_1))) {
          simul_m_counter <-
            simul_aware_agg(
              samp = samp_1,
              mod = make_country[[cnt]]$mod,
              fp = make_country[[cnt]]$fp,
              hivdemo_proj = hivdemo_proj_list[[cnt]],
              pmtct = pmtct_1,
              year = c(2015:2023),
              age = c("15-24", "25-34", "35-49", "50-99"),
              sex = c("male")
            )
          
          print(10)
          
          simul_f_counter <-
            simul_aware_agg(
              samp = samp_1,
              mod = make_country[[cnt]]$mod,
              fp = make_country[[cnt]]$fp,
              hivdemo_proj = hivdemo_proj_list[[cnt]],
              pmtct = pmtct_1,
              year = c(2015:2023),
              age = c("15-24", "25-34", "35-49", "50-99"),
              sex = c("female")
            )
          print(11)
          simul_b_counter <-
            simul_aware_agg(
              samp = samp_1,
              mod = make_country[[cnt]]$mod,
              fp = make_country[[cnt]]$fp,
              hivdemo_proj = hivdemo_proj_list[[cnt]],
              pmtct = pmtct_1,
              year = c(2015:2023),
              age = c("15-24", "25-34", "35-49", "50-99"),
              sex = c("male", "female")
            )
          print(12)
          
          aware_agg_simul_male_counter[[cnt]]$out_simul_aware_all = simul_m_counter
          aware_agg_simul_female_counter[[cnt]]$out_simul_aware_all = simul_f_counter
          aware_agg_simul_both_counter[[cnt]]$out_simul_aware_all = simul_b_counter
          
          make_country[[cnt]]$aware_male_counter$out_simul_aware_all = simul_m_counter
          make_country[[cnt]]$aware_female_counter$out_simul_aware_all = simul_f_counter
          make_country[[cnt]]$aware_both_counter$out_simul_aware_all = simul_b_counter
          
          saveRDS(
            aware_agg_simul_male_counter,
            paste0(here::here("outputs"),
                   "/male aware counter.rda")
          )
          saveRDS(
            aware_agg_simul_female_counter,
            paste0(
              here::here("outputs"),
              "/female aware counter.rda"
            )
          )
          saveRDS(
            aware_agg_simul_both_counter,
            paste0(here::here("outputs"),
                   "/both aware counter.rda")
          )
          gc()
        }
        
        
        
        
        
        
      },
      error = function(e) {
        message("Caught an error: ", e$message)
        print(cnt)
      },
      finally = {
        
      }
    )
  }
  
}


# to fill in any missing make country sections
for (i in 1:length(tdx_agg_simul_both)) {
  cnt = names(tdx_agg_simul_both)[i]
  
  make_country[[cnt]]$tdx_both = tdx_agg_simul_both[[cnt]]
  make_country[[cnt]]$tdx_male = tdx_agg_simul_male[[cnt]]
  make_country[[cnt]]$tdx_female = tdx_agg_simul_female[[cnt]]
  
  
  make_country[[cnt]]$aware_both = aware_agg_simul_both[[cnt]]
  make_country[[cnt]]$aware_male = aware_agg_simul_male[[cnt]]
  make_country[[cnt]]$aware_female = aware_agg_simul_female[[cnt]]
  
  make_country[[cnt]]$tdx_both_counter = tdx_agg_simul_both_counter[[cnt]]
  make_country[[cnt]]$tdx_male_counter = tdx_agg_simul_male_counter[[cnt]]
  make_country[[cnt]]$tdx_female_counter = tdx_agg_simul_female_counter[[cnt]]
  
  
  make_country[[cnt]]$aware_both_counter = aware_agg_simul_both_counter[[cnt]]
  make_country[[cnt]]$aware_male_counter = aware_agg_simul_male_counter[[cnt]]
  make_country[[cnt]]$aware_female_counter = aware_agg_simul_female_counter[[cnt]]
  
  
  
  
}

# to fill in any missing aware agg 
for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  
  
  tdx_agg_simul_both[[cnt]]   <- make_country[[cnt]]$tdx_both
  tdx_agg_simul_male[[cnt]]   <- make_country[[cnt]]$tdx_male
  tdx_agg_simul_female[[cnt]] <- make_country[[cnt]]$tdx_female
  
  aware_agg_simul_both[[cnt]]   <- make_country[[cnt]]$aware_both
  aware_agg_simul_male[[cnt]]   <- make_country[[cnt]]$aware_male
  aware_agg_simul_female[[cnt]] <- make_country[[cnt]]$aware_female
  
  if(!is.na(counter_years[counter_years$country == cnt,]$start)){
  
  
  tdx_agg_simul_both_counter[[cnt]]   <- make_country[[cnt]]$tdx_both_counter
  tdx_agg_simul_male_counter[[cnt]]   <- make_country[[cnt]]$tdx_male_counter
  tdx_agg_simul_female_counter[[cnt]] <- make_country[[cnt]]$tdx_female_counter
  
  aware_agg_simul_both_counter[[cnt]]   <- make_country[[cnt]]$aware_both_counter
  aware_agg_simul_male_counter[[cnt]]   <- make_country[[cnt]]$aware_male_counter
  aware_agg_simul_female_counter[[cnt]] <- make_country[[cnt]]$aware_female_counter
  
  
  }else {
    tdx_agg_simul_both_counter[[cnt]]   <- NULL
    tdx_agg_simul_male_counter[[cnt]]   <- NULL
    tdx_agg_simul_female_counter[[cnt]] <- NULL
    
    aware_agg_simul_both_counter[[cnt]]   <- NULL
    aware_agg_simul_male_counter[[cnt]]   <- NULL
    aware_agg_simul_female_counter[[cnt]] <- NULL
    
  }
  
}

# final save
# aware (counter)
saveRDS(aware_agg_simul_male_counter,
        paste0(here::here("outputs"),
               "/male aware counter.rda"))
saveRDS(
  aware_agg_simul_female_counter,
  paste0(here::here("outputs"),
         "/female aware counter.rda")
)
saveRDS(aware_agg_simul_both_counter,
        paste0(here::here("outputs"),
               "/both aware counter.rda"))
# aware (observed)
saveRDS(aware_agg_simul_male,
        paste0(here::here("outputs"),
               "/male aware observed.rda"))
saveRDS(aware_agg_simul_female,
        paste0(here::here("outputs"),
               "/female aware observed.rda"))
saveRDS(aware_agg_simul_both,
        paste0(here::here("outputs"),
               "/both aware observed.rda"))

# tdx (counter)
saveRDS(tdx_agg_simul_male_counter,
        paste0(here::here("outputs"),
               "/male tdx counter.rda"))
saveRDS(tdx_agg_simul_female_counter,
        paste0(here::here("outputs"),
               "/female tdx counter.rda"))
saveRDS(tdx_agg_simul_both_counter,
        paste0(here::here("outputs"),
               "/both tdx counter.rda"))

# tdx (observed)
saveRDS(tdx_agg_simul_male,
        paste0(here::here("outputs"),
               "/male tdx observed.rda"))
saveRDS(tdx_agg_simul_female,
        paste0(here::here("outputs"),
               "/female tdx observed.rda"))
saveRDS(tdx_agg_simul_both,
        paste0(here::here("outputs"),
               "/both tdx observed.rda"))

# make_country save
saveRDS(
  object = make_country,
  file = here::here('anc testing/data/make_country_simul_final.rds')
)
