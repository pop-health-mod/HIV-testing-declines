source(paste0(here::here("anc testing"), "/0.6 time dx functions.R"))
source(paste0(here::here("anc testing"), "/0.5 simul-aware-functions.R"))

library(Matrix)
library(first90)
library(tidyverse)
library(Rcpp)

path_anc <- here::here("anc testing")
make_country = readRDS(paste0(path_anc,"/data/make_country_simul.rds"))

# extract the PMTCT information from the Spectrum files
pmtct_list <- readRDS(paste0(path_anc, "/data/pmtct_list_cnt.rds"))

# extract the HIV/demographic projections
hivdemo_proj_list <- readRDS(paste0(path_anc, "/data/hivdemo_proj_dt_cnt.rds"))


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
cntlist = names(make_country)[-7]

for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  print(cnt)
  if (cnt %in% cntlist) {
    tryCatch(
      expr = {
        
        fp <- make_country[[cnt]]$fp
        
        pgrm_dat <-
          first90::select_prgmdata(make_country[[cnt]]$prgm_dat, 
                                   cnt = cnt)
        
        survey_hts <-
          first90::select_hts(make_country[[cnt]]$survey_hts,
                              cnt = cnt,
                              age_group = c('15-24', '25-34', '35-49'))
        
        # define parameters by country
        samp = make_country[[cnt]]$samp
        opt = make_country[[cnt]]$opt
        mod = make_country[[cnt]]$mod
        
        # we select pmtct data 
        pmtct <- pmtct_list[[cnt]]
        
        
        # we select HIV demo proj
        hivdemo_proj <- hivdemo_proj_list[[cnt]]
        
        
        # time to diagnosis
        if (is.null(make_country[[cnt]]$tdx_both)) {
          
          simul_tdxF = simul_pool_time_dx_agg_prev(
            samp,
            make_country[[cnt]]$mod,
            make_country[[cnt]]$fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = 2015:2023,
            std = F,
            sex = "female",
            parallel = T
          )
          print(1)
          
          simul_tdxM = simul_pool_time_dx_agg_prev(
            samp,
            mod,
            fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = 2015:2023,
            std = F,
            sex = "male",
            parallel = T
          )
          print(2)
          simul_tdxB = simul_pool_time_dx_agg_prev(
            samp = samp,
            mod = mod,
            fp = fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = 2015:2023,
            std = F,
            sex = c("male", "female"),
            parallel = T
          )
          print(3)
          
          tdx_agg_simul_male[[cnt]]$out_simul_tdx_all = simul_tdxM
          tdx_agg_simul_female[[cnt]]$out_simul_tdx_all = simul_tdxF
          tdx_agg_simul_both[[cnt]]$out_simul_tdx_all = simul_tdxB
          
          make_country[[cnt]]$tdx_male$out_simul_tdx_all = simul_tdxM
          make_country[[cnt]]$tdx_female$out_simul_tdx_all = simul_tdxF
          make_country[[cnt]]$tdx_both$out_simul_tdx_all = simul_tdxB
          
          saveRDS(tdx_agg_simul_male,
                  paste0(
                    here::here("outputs"),
                    "/male time to dx final 12 30 2.rda"
                  ))
          saveRDS(
            tdx_agg_simul_female,
            paste0(
              here::here("outputs"),
              "/female time to dx final 12 30 2.rda"
            )
          )
          saveRDS(tdx_agg_simul_both,
                  paste0(
                    here::here("outputs"),
                    "/both time to dx final 12 30 2.rda"
                  ))
          gc()
        }
        
        # counterfactual time to diagnosis
        if (is.null(make_country[[cnt]]$tdx_both_counter) &
            !is.na(counter_years[counter_years$country == cnt, 2])) {
          
          simul_tdxF_counter = simul_pool_time_dx_agg_prev_counter(
            samp,
            make_country[[cnt]]$mod,
            make_country[[cnt]]$fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = 2015:2023,
            std = F,
            sex = "female",
            parallel = T,
            counter_years = counter_years[counter_years$country == cnt,]
          )
          print(4)
          
          simul_tdxM_counter = simul_pool_time_dx_agg_prev_counter(
            samp,
            mod,
            fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = 2015:2023,
            std = F,
            sex = "male",
            parallel = T,
            counter_years = counter_years[counter_years$country == cnt,]
          )
          print(5)
          simul_tdxB_counter = simul_pool_time_dx_agg_prev_counter(
            samp = samp,
            mod = mod,
            fp = fp,
            hivdemo_proj = hivdemo_proj_list[[cnt]],
            pmtct = pmtct_list[[cnt]],
            year = 2015:2023,
            std = F,
            sex = c("male", "female"),
            parallel = T,
            counter_years = counter_years[counter_years$country == cnt,]
          )
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
              "/male time to dx final counter 12 30 2.rda"
            )
          )
          saveRDS(
            tdx_agg_simul_female_counter,
            paste0(
              here::here("outputs"),
              "/female time to dx final counter 12 30 2.rda"
            )
          )
          saveRDS(
            tdx_agg_simul_both_counter,
            paste0(
              here::here("outputs"),
              "/both time to dx final counter 12 30 2.rda"
            )
          )
          gc()
        }
        
        # counterfactual time to diagnosis
        if (is.null(make_country[[cnt]]$aware_both)) {
          
          simul_m <-
            simul_aware_agg(
              samp,
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
              samp,
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
              samp,
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
                         "/male aware final 12 30 2.rda"))
          saveRDS(
            aware_agg_simul_female,
            paste0(here::here("outputs"),
                   "/female aware final 12 30 2.rda")
          )
          saveRDS(aware_agg_simul_both,
                  paste0(here::here("outputs"),
                         "/both aware final 12 30 2.rda"))
          gc()
        }
        
        # counterfactual time to diagnosis
        if (is.null(make_country[[cnt]]$aware_both_counter) &
            !is.na(counter_years[counter_years$country == cnt, 2])) {
          
          simul_m_counter <-
            simul_aware_agg_counter(
              samp,
              mod = make_country[[cnt]]$mod,
              fp = make_country[[cnt]]$fp,
              hivdemo_proj = hivdemo_proj_list[[cnt]],
              pmtct = pmtct_list[[cnt]],
              counter_years = counter_years[counter_years$country == cnt,],
              year = c(2015:2023),
              age = c("15-24", "25-34", "35-49", "50-99"),
              sex = c("male")
            )
          print(10)
          simul_f_counter <-
            simul_aware_agg_counter(
              samp,
              mod = make_country[[cnt]]$mod,
              fp = make_country[[cnt]]$fp,
              hivdemo_proj = hivdemo_proj_list[[cnt]],
              pmtct = pmtct_list[[cnt]],
              counter_years = counter_years[counter_years$country == cnt,],
              year = c(2015:2023),
              age = c("15-24", "25-34", "35-49", "50-99"),
              sex = c("female")
            )
          print(11)
          simul_b_counter <-
            simul_aware_agg_counter(
              samp,
              mod = make_country[[cnt]]$mod,
              fp = make_country[[cnt]]$fp,
              hivdemo_proj = hivdemo_proj_list[[cnt]],
              pmtct = pmtct_list[[cnt]],
              counter_years = counter_years[counter_years$country == cnt,],
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
                   "/male aware final counter 12 30 2.rda")
          )
          saveRDS(
            aware_agg_simul_female_counter,
            paste0(here::here("outputs"),
                   "/female aware final counter 12 30 2.rda")
          )
          saveRDS(
            aware_agg_simul_both_counter,
            paste0(here::here("outputs"),
                   "/both aware final counter 12 30 2 .rda")
          )
          gc()
        }
        
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

# final save
# aware (counter)
saveRDS(aware_agg_simul_male_counter,
        paste0(here::here("outputs"),
               "/male aware final counter.rda"))
saveRDS(aware_agg_simul_female_counter,
        paste0(here::here("outputs"),
               "/female aware final counter.rda"))
saveRDS(aware_agg_simul_both_counter,
        paste0(here::here("outputs"),
               "/both aware final counter.rda"))
# aware (observed)
saveRDS(aware_agg_simul_male,
        paste0(here::here("outputs"),
               "/male aware final observed.rda"))
saveRDS(aware_agg_simul_female,
        paste0(here::here("outputs"),
               "/female aware final observed.rda"))
saveRDS(aware_agg_simul_both,
        paste0(here::here("outputs"),
               "/both aware final observed.rda"))

# tdx (counter)
saveRDS(tdx_agg_simul_male_counter,
        paste0(here::here("outputs"),
               "/male tdx final counter.rda"))
saveRDS(tdx_agg_simul_female_counter,
        paste0(here::here("outputs"),
               "/female tdx final counter.rda"))
saveRDS(tdx_agg_simul_both_counter,
        paste0(here::here("outputs"),
               "/both tdx final counter.rda"))

# tdx (observed)
saveRDS(tdx_agg_simul_male,
        paste0(here::here("outputs"),
               "/male tdx final observed.rda"))
saveRDS(tdx_agg_simul_female,
        paste0(here::here("outputs"),
               "/female tdx final observed.rda"))
saveRDS(tdx_agg_simul_both,
        paste0(here::here("outputs"),
               "/both tdx final observed.rda"))

# make_country save
saveRDS(
  object = make_country,
  file = here::here('anc testing/data/make_country_simul_final.rds')
)
