library(first90)
library(Matrix)
library(Rcpp)
source("anc testing/1.0 simmod.R")
source("anc testing/1.1 tot test out.R")

counter_year <- read_rds("anc testing/data/counter_years.rds")
counter_anc_years <- read_rds("anc testing/data/counter_anc_years.rds")

path_anc <- here::here("anc testing")

# extract the PMTCT information from the Spectrum files
pmtct_list <- readRDS(paste0(path_anc, "/data/pmtct_list_cnt.rds"))

# extract the HIV/demographic projections
hivdemo_proj_list <- readRDS(paste0(path_anc, "/data/hivdemo_proj_dt_cnt.rds"))


# adjust for specific country runs
#remove chad for later run
cntlist = names(make_country)[c(-6)]

for (country in make_country) {
  
  cnt = country$cnt
  print(cnt)
  
  if(cnt %in% cntlist){
    tryCatch(
      expr = {
        #break()
        fp <- make_country[[cnt]]$fp
        pgrm_dat <- first90::select_prgmdata(make_country[[cnt]]$prgm_dat, cnt = cnt)
        survey_hts <- first90::select_hts(make_country[[cnt]]$survey_hts, cnt = cnt, age_group <- c('15-24','25-34','35-49'))
        mod = make_country[[cnt]]$mod
        opt = make_country[[cnt]]$opt
        
        # select country specific pmtct
        pmtct <- pmtct_list[[cnt]]
        
        # select country specific demo proj
        hivdemo_proj <- hivdemo_proj_list[[cnt]]
        
        
        samp = make_country[[cnt]]$samp
        
        counter_years_loop = counter_years[counter_years$country == cnt,]
        
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
            pmtct_1$anc_test[, Years_anc] = pmtct$anc_test[, year_start]
            # if needed adjust recive/need pmtct, all countries should have anctest but just incase one year is missing.
            pmtct_1$receivepmtct[Years_anc] = pmtct$receivepmtct[year_start]
            pmtct_1$needpmtct[Years_anc] = pmtct$needpmtct[year_start]
          }
        }
        fp = create_anc_param(opt$par,fp,pmtct,hivdemo_proj)
        
        
        # identify credible intervals
        counter_simul <- simul.run.anc(samp_1,
                                       fp,
                                       pmtct = pmtct_1,
                                       hivdemo_proj = hivdemo_proj)
        
        make_country[[cnt]]$counter_simul = counter_simul
        
        
          # identify credible intervals
        simul <- simul.run.anc(samp,
                               fp,
                               pmtct = pmtct,
                               hivdemo_proj = hivdemo_proj)
        
        make_country[[cnt]]$simul = simul
        saveRDS(
          object = simul,
          file = paste0(here::here('anc testing/data/simul'),'/make_country_simul_final ',cnt,'.rds')
        )
        
        
        
        #save each version in case of crash
        saveRDS(object = counter_simul, file = paste0(here::here('anc testing/data/counter_simul'),'/make_country_simul_final ',cnt,'.rds'))
        
        
      },
      # allows for running multiple countries at a time even with minor errors present(i.e. unlucky neldermead)
      error = function(e) {
        message("Caught an error: ",cnt, e$message)
        
      },
      finally = {
        
      }
    )
  }
  
}

saveRDS(object = make_country, file = here::here('anc testing/data/make_country_simul_final.rds'))




