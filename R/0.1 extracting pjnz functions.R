## ---- functuons to extract and create make country ----
make_country_FUN = function(pjnz_list){
  
  file_in_zip = first90::file_in_zip
  ##for lists
  if(class(pjnz_list) == "list"){
    
    cnt <- first90::read_country(pjnz_list[[1]])
    print(cnt)
    #issue in create fp for lists, basically assuming 1 length spectrum vector, so need custom create fp using spectrumversion[1]
    fp <- prepare_inputs(pjnz_list)
    fp$t_anc_start = 35L
    
    srv <- list()
    prg <- list()
    
    print("prefor")
    for (i in 1:length(pjnz_list)) {
      print(i)
      dp_file <- file_in_zip(pjnz_list[[i]], ".DP$")
      pjn_file <- file_in_zip(pjnz_list[[i]], ".PJN$")
      
      dp <- first90:::first90_read_csv_character(dp_file)
      
      names_shiny = "<Shiny90SurveyData MV>"
      names_shiny_program = "<Shiny90ProgramData MV>"
      
      
      dp_vector = dp[,1]
      dp_end_vector = which(dp_vector == "<End>")
      dp_survey = which(dp_vector == names_shiny)
      
      #finds the first <end> after it reaches survey and then selects all the data between those 2 points
      survey_extract = dp[c(dp_survey:dp_end_vector[which(dp_end_vector>dp_survey)[1]]-1),c(1:13)]
      
      #adds names to coloumn
      colnames(survey_extract) = c("Tag","Description","surveyid","year","agegr","sex","hivstatus","est","se","ci_l","ci_u","counts")
      
      #factors the age
      survey_extract$agegr = as.factor(survey_extract$agegr)
      levels(survey_extract$agegr)  = c("15-24","25-34","35-49","15-49")
      
      #factors sex
      survey_extract$sex = as.factor(survey_extract$sex)
      levels(survey_extract$sex)  = c("both","male","female")
      
      #factors positivity
      survey_extract$hivstatus = as.factor(survey_extract$hivstatus)
      levels(survey_extract$hivstatus)  = c("All","Negative","Positive")
      
      #shrinks NA around datafram
      survey_extract = survey_extract[-c(1:4),]
      survey_extract = survey_extract[,-c(1,2)]
      
      #assigns NA's to survey HTS and ensures the lenghts match up
      na_vec_S = matrix(NA, nrow = length(survey_extract[,1]), ncol = 1)
      survey_hts <- data.frame(surveyid = na_vec_S,
                               country = na_vec_S,
                               year = na_vec_S,
                               hivstatus = na_vec_S,
                               sex = na_vec_S,
                               agegr = na_vec_S,
                               est = na_vec_S,
                               se = na_vec_S,
                               ci_l = na_vec_S,
                               ci_u = na_vec_S,
                               counts = na_vec_S,
                               outcome = na_vec_S)
      #assign the values
      if(all(lengths(survey_extract)>0)){
        survey_hts <- data.frame(surveyid = survey_extract$surveyid,
                                 country = cnt,
                                 year = as.numeric(survey_extract$year),
                                 hivstatus = tolower(survey_extract$hivstatus),
                                 sex = survey_extract$sex,
                                 agegr = survey_extract$agegr,
                                 est = as.numeric(survey_extract$est)*0.01,
                                 se = as.numeric(survey_extract$se)*0.01,
                                 ci_l = as.numeric(survey_extract$ci_l)*0.01,
                                 ci_u = as.numeric(survey_extract$ci_u)*0.01,
                                 counts = as.numeric(survey_extract$counts),
                                 outcome = "evertest" #use with caution
                                 )
        
        survey_hts = with(survey_hts, survey_hts[order(surveyid,agegr,sex,hivstatus),])
      }
      
      #dim(dp)  
      
      
      #exact same but for program data
      #stop("preprogram")
      dp_program = which(dp_vector == names_shiny_program)
      dp_test = which(dp_vector ==  "<HIVTesting MV>")
      dp_end_vector[which(dp_end_vector>dp_test)[1]]-1
      
      
      dp_end_vector[which(dp_end_vector>dp_program)[1]]-1
      program_extract = dp[c(dp_program:dp_end_vector[which(dp_end_vector>dp_program)[1]]-1),c(1:10)]
      colnames(program_extract) = c("Tag","Description","year","sex","total tests","total positive tests","total hts tests","total positive hts tests","total ANC test","total positive ANC test")
      #print(program_extract)
      program_extract$sex = as.factor(program_extract$sex)
      levels(program_extract$sex)  = c("both","male","female")
      program_extract = program_extract[-c(1:4),]
      program_extract = program_extract[,-c(1,2)]
      
      #converts -9999 to NA
      for (j in 1:length(program_extract[1,])) {
        
        program_extract[which(program_extract[,j]==-9999),j] = NA
      }
      
      country = cnt
      
      #empty data frame prgmdat
      na_vec = matrix(NA, nrow = length(program_extract[,1]), ncol = 1)
      prgm_dat <- data.frame(country = na_vec,
                             year = na_vec,
                             sex = na_vec,
                             agegr = na_vec,
                             tot = na_vec,
                             totpos = na_vec,
                             vct =na_vec,
                             vctpos = na_vec,
                             anc = na_vec,
                             ancpos = na_vec)
      
      #assigns to prgm dat
      prgm_dat = prgm_dat %>% 
        mutate(year = as.numeric(program_extract$year),
               country = cnt,
               sex = program_extract$sex,
               agegr = "15-99",
               tot = as.numeric(program_extract$`total tests`),
               totpos = as.numeric(program_extract$`total positive tests`),
               vct = as.numeric(program_extract$`total hts tests`),
               vctpos = as.numeric(program_extract$`total positive hts tests`),
               anc = as.numeric(program_extract$`total ANC test`),
               ancpos = as.numeric(program_extract$`total positive ANC test`))
      
      
      
      
      
      
      prgm_dat[prgm_dat == 0] = NA
      
      
      
      #stop("end")
      srv[[i]] = survey_hts
      prg[[i]] = prgm_dat
      #print(length(prg))
      
      
    }  
    
    hold = vector()
    for (i in 1:length(prg)) {
      if(any(prg[[i]]$sex!="both")){
        hold = c(hold,i)
      }
    }
    
    # Combine all data frames
    combined_data <- bind_rows(prg[c(1:length(prg))])
    
    # Sum the relevant columns by year
    summed_data_by_year <- combined_data %>%
      group_by(year,agegr,country,sex) %>%
      summarise(
        tot = sum(tot, na.rm = TRUE),
        totpos = sum(totpos, na.rm = TRUE),
        vct = sum(vct, na.rm = TRUE),
        vctpos = sum(vctpos, na.rm = TRUE),
        anc = sum(anc, na.rm = TRUE),
        ancpos = sum(ancpos, na.rm = TRUE)
      )
    prg = as.data.frame(summed_data_by_year)
    prg[prg == 0] = NA
    
    return(list(prgm_dat = prg,survey_hts =  srv,fp = fp, cnt = cnt))
  }
  
  if(class(pjnz_list) != "list"){
    
    pjnz = pjnz_list
    cnt <- first90::read_country(pjnz)
    print(cnt)
    fp <- first90::prepare_inputs(pjnz)
    fp$t_anc_start = 35L
    
    
    
    
    dp_file <- file_in_zip(pjnz, ".DP$")
    pjn_file <- file_in_zip(pjnz, ".PJN$")
    dp <- first90:::first90_read_csv_character(dp_file)
    
    names_shiny = "<Shiny90SurveyData MV>"
    names_shiny_program = "<Shiny90ProgramData MV>"
    
    
    dp_vector = dp[,1]
    dp_end_vector = which(dp_vector == "<End>")
    dp_survey = which(dp_vector == names_shiny)
    
    #finds the first <end> after it reaches survey and then selects all the data between those 2 points
    survey_extract = dp[c(dp_survey:dp_end_vector[which(dp_end_vector>dp_survey)[1]]-1),c(1:13)]
    
    #adds names to coloumn
    colnames(survey_extract) = c("Tag","Description","surveyid","year","agegr","sex","hivstatus","est","se","ci_l","ci_u","counts")
    
    #factors the age
    survey_extract$agegr = as.factor(survey_extract$agegr)
    levels(survey_extract$agegr)  = c("15-24","25-34","35-49","15-49")
    
    #factors sex
    survey_extract$sex = as.factor(survey_extract$sex)
    levels(survey_extract$sex)  = c("both","male","female")
    
    #factors positivity
    survey_extract$hivstatus = as.factor(survey_extract$hivstatus)
    levels(survey_extract$hivstatus)  = c("All","Negative","positive")
    
    #shrinks NA around datafram
    survey_extract = survey_extract[-c(1:4),]
    survey_extract = survey_extract[,-c(1,2)]
    
    #assigns NA's to survey HTS and ensures the lenghts match up
    na_vec_S = matrix(NA, nrow = length(survey_extract[,1]), ncol = 1)
    survey_hts <- data.frame(surveyid = na_vec_S,
                             country = na_vec_S,
                             year = na_vec_S,
                             hivstatus = na_vec_S,
                             sex = na_vec_S,
                             agegr = na_vec_S,
                             est = na_vec_S,
                             se = na_vec_S,
                             ci_l = na_vec_S,
                             ci_u = na_vec_S,
                             counts = na_vec_S,
                             outcome = na_vec_S)
    #assign the values
    if(all(lengths(survey_extract)>0)){
      survey_hts <- data.frame(surveyid = survey_extract$surveyid,
                               country = cnt,
                               year = as.numeric(survey_extract$year),
                               hivstatus = tolower(survey_extract$hivstatus),
                               sex = survey_extract$sex,
                               agegr = survey_extract$agegr,
                               est = as.numeric(survey_extract$est)*0.01,
                               se = as.numeric(survey_extract$se)*0.01,
                               ci_l = as.numeric(survey_extract$ci_l)*0.01,
                               ci_u = as.numeric(survey_extract$ci_u)*0.01,
                               counts = as.numeric(survey_extract$counts),
                               outcome = "evertest" #use with caution
      )
      
      survey_hts = with(survey_hts, survey_hts[order(surveyid,agegr,sex,hivstatus),])
    }
    #dim(dp)  
    
    
    #exact same but for program data
    #stop("preprogram")
    dp_program = which(dp_vector == names_shiny_program)
    dp_test = which(dp_vector ==  "<HIVTesting MV>")
    dp_end_vector[which(dp_end_vector>dp_test)[1]]-1
    
    
    dp_end_vector[which(dp_end_vector>dp_program)[1]]-1
    program_extract = dp[c(dp_program:dp_end_vector[which(dp_end_vector>dp_program)[1]]-1),c(1:10)]
    colnames(program_extract) = c("Tag","Description","year","sex","total tests","total positive tests","total hts tests","total positive hts tests","total ANC test","total positive ANC test")
    #print(program_extract)
    program_extract$sex = as.factor(program_extract$sex)
    levels(program_extract$sex)  = c("both","male","female")
    program_extract = program_extract[-c(1:4),]
    program_extract = program_extract[,-c(1,2)]
    
    #converts -9999 to NA
    for (j in 1:length(program_extract[1,])) {
      
      program_extract[which(program_extract[,j]==-9999),j] = NA
    }
    
    country = cnt
    
    #empty data frame prgmdat
    na_vec = matrix(NA, nrow = length(program_extract[,1]), ncol = 1)
    prgm_dat <- data.frame(country = na_vec,
                           year = na_vec,
                           sex = na_vec,
                           agegr = na_vec,
                           tot = na_vec,
                           totpos = na_vec,
                           vct =na_vec,
                           vctpos = na_vec,
                           anc = na_vec,
                           ancpos = na_vec)
    
    #assigns to prgm dat
    prgm_dat = prgm_dat %>% 
      mutate(year = as.numeric(program_extract$year),
             country = cnt,
             sex = program_extract$sex,
             agegr = "15-99",
             tot = as.numeric(program_extract$`total tests`),
             totpos = as.numeric(program_extract$`total positive tests`),
             vct = as.numeric(program_extract$`total hts tests`),
             vctpos = as.numeric(program_extract$`total positive hts tests`),
             anc = as.numeric(program_extract$`total ANC test`),
             ancpos = as.numeric(program_extract$`total positive ANC test`))
    
    
    
    
    
    
    prgm_dat[prgm_dat == 0] = NA
    
    
    
    return(list(prgm_dat = prgm_dat,survey_hts = survey_hts,fp = fp, cnt = cnt))
  }
  
  
}

sumrowsprgm = function(pgrm,year) {
  pgrm[is.na(pgrm)] = 0
  for (i in 1:length(pgrm$year)) {
    if(pgrm$year[i] >= year && i+1 <= length(pgrm$year)){
      # print(i)
      pgrm[c(i),c(5:10)] = colSums(pgrm[c(i,i+1),c(5:10)])
      pgrm = pgrm[-c(i+1),]
      pgrm[i,3] = "both"
    }
    # print(i)  
  }
  return(pgrm)
  
  
}


create_fp <- function(projp,
                      demp,
                      hiv_steps_per_year = 10L,
                      proj_start = projp$yr_start,
                      proj_end = projp$yr_end,
                      AGE_START = 15L,
                      popadjust = TRUE,
                      artelig200adj=TRUE,
                      who34percelig=0,
                      projection_period = NULL,
                      art_dropout_recover_cd4 = NULL) {
  
  
  ## ########################## ##
  ##  Define model state space  ##
  ## ########################## ##
  
  ## Parameters defining the model projection period and state-space
  ss <- list(proj_start = proj_start,
             PROJ_YEARS = as.integer(proj_end - proj_start + 1L),
             AGE_START  = as.integer(AGE_START),
             hiv_steps_per_year = as.integer(hiv_steps_per_year))
  
  ## populuation projection state-space
  ss$NG <- 2
  ss$pDS <- 2               # Disease stratification for population projection (HIV-, and HIV+)
  
  ## macros
  ss$m.idx <- 1
  ss$f.idx <- 2
  
  ss$hivn.idx <- 1
  ss$hivp.idx <- 2
  
  ss$pAG <- 81 - AGE_START
  ss$ag.rate <- 1
  ss$p.fert.idx <- 16:50 - AGE_START
  ss$p.age15to49.idx <- 16:50 - AGE_START
  ss$p.age15plus.idx <- (16-AGE_START):ss$pAG
  
  
  ## HIV model state-space
  ss$h.ag.span <- as.integer(c(2,3, rep(5, 6), 31))   # Number of population age groups spanned by each HIV age group [sum(h.ag.span) = pAG]
  ss$hAG <- length(ss$h.ag.span)          # Number of age groups
  ss$hDS <- 7                             # Number of CD4 stages (Disease Stages)
  ss$hTS <- 3                             # number of treatment stages (including untreated)
  
  ss$ag.idx <- rep(1:ss$hAG, ss$h.ag.span)
  ss$agfirst.idx <- which(!duplicated(ss$ag.idx))
  ss$aglast.idx <- which(!duplicated(ss$ag.idx, fromLast=TRUE))
  
  
  ss$h.fert.idx <- which((AGE_START-1 + cumsum(ss$h.ag.span)) %in% 15:49)
  ss$h.age15to49.idx <- which((AGE_START-1 + cumsum(ss$h.ag.span)) %in% 15:49)
  ss$h.age15plus.idx <- which((AGE_START-1 + cumsum(ss$h.ag.span)) >= 15)
  
  invisible(list2env(ss, environment())) # put ss variables in environment for convenience
  
  fp <- list(ss=ss)
  fp$SIM_YEARS <- ss$PROJ_YEARS
  fp$proj.steps <- proj_start + 0.5 + 0:(ss$hiv_steps_per_year * (fp$SIM_YEARS-1)) / ss$hiv_steps_per_year
  
  ## Replace 6,14 with 6.14 for files saved with french locale
  projp$spectrum_version <- sub("^([0-9]+),(.*)$", "\\1.\\2", projp$spectrum_version)
  
  if (!grepl("^[4-6]\\.[0-9]", projp$spectrum_version[1])) {
    stop(paste0("Spectrum version not recognized: ", projp$spectrum_version))
  }
  
  if (is.null(projection_period)) {
    fp$projection_period <- if (projp$spectrum_version[1] >= "6.2") {"calendar"} else {"midyear"}
  } else {
    stopifnot(projection_period %in% c("calendar", "midyear"))
    fp$projection_period <- projection_period
  }
  
  
  ## ######################## ##
  ##  Demographic parameters  ##
  ## ######################## ##
  
  fp$basepop <- demp$basepop[(AGE_START+1):81, , as.character(proj_start)]
  fp$Sx <- demp$Sx[(AGE_START+1):81,,as.character(proj_start:proj_end)]
  
  fp$asfr <- demp$asfr[,as.character(proj_start:proj_end)] # NOTE: assumes 15-49 is within projection age range
  ## Note: Spectrum averages ASFRs from the UPD file over 5-year age groups.
  ##       Prefer to use single-year of age ASFRs as provided. The below line will
  ##       convert to 5-year average ASFRs to exactly match Spectrum.
  ## fp$asfr <- apply(apply(fp$asfr, 2, tapply, rep(3:9*5, each=5), mean), 2, rep, each=5)
  
  fp$srb <- sapply(demp$srb[as.character(proj_start:proj_end)], function(x) c(x,100)/(x+100))
  
  netmigr.adj <- demp$netmigr
  
  if (fp$projection_period == "midyear") {
    
    ## Spectrum mid-year projection (v5.19 and earlier) adjusts net-migration to occur
    ## half in current age group and half in next age group
    
    netmigr.adj[-1,,] <- (demp$netmigr[-1,,] + demp$netmigr[-81,,])/2
    netmigr.adj[1,,] <- demp$netmigr[1,,]/2
    netmigr.adj[81,,] <- netmigr.adj[81,,] + demp$netmigr[81,,]/2
  }
  
  fp$netmigr <- netmigr.adj[(AGE_START+1):81,,as.character(proj_start:proj_end)]
  
  fp$entrantpop <- projp$totpop[AGE_START,,as.character(proj_start:proj_end)]
  
  ## set population adjustment
  fp$popadjust <- popadjust
  
  hivnpop <- projp$totpop - projp$hivpop
  fp$target_hivn_pop <- hivnpop[(AGE_START+1):81,,as.character(proj_start:proj_end)]
  fp$target_hivp_pop <- projp$hivpop[(AGE_START+1):81,,as.character(proj_start:proj_end)]
  
  fp$target_hivn_ha <- apply(fp$target_hivn_pop, 2:3, fastmatch::ctapply, fp$ss$ag.idx, sum)
  fp$target_artpop_ha <- apply(projp$artpop[(AGE_START+1):81,,as.character(proj_start:proj_end)],
                               2:3, fastmatch::ctapply, fp$ss$ag.idx, sum)
  hivp_ha <- apply(fp$target_hivp_pop, 2:3, fastmatch::ctapply, fp$ss$ag.idx, sum)
  fp$target_hivpop_ha <- hivp_ha - fp$target_artpop_ha
  
  
  ## ###################### ##
  ##  HIV model parameters  ##
  ## ###################### ##
  
  projp.h.ag <- findInterval(AGE_START + cumsum(h.ag.span) - h.ag.span, c(15, 25, 35, 45))  # NOTE: Will not handle AGE_START < 15 presently
  fp$cd4_initdist <- projp$cd4_initdist[,projp.h.ag,]
  fp$cd4_prog <- (1-exp(-projp$cd4_prog[,projp.h.ag,] / hiv_steps_per_year)) * hiv_steps_per_year
  fp$cd4_mort <- projp$cd4_mort[,projp.h.ag,]
  fp$art_mort <- projp$art_mort[,,projp.h.ag,]
  fp$artmx_timerr <- projp$artmx_timerr
  
  frr_agecat <- as.integer(rownames(projp$fert_rat))
  frr_agecat[frr_agecat == 18] <- 17
  fert_rat.h.ag <- findInterval(AGE_START + cumsum(h.ag.span[h.fert.idx]) - h.ag.span[h.fert.idx], frr_agecat)
  
  fp$frr_cd4 <- array(1, c(hDS, length(h.fert.idx), PROJ_YEARS))
  fp$frr_cd4[,,] <- rep(projp$fert_rat[fert_rat.h.ag, as.character(proj_start:proj_end)], each=hDS)
  fp$frr_cd4 <- sweep(fp$frr_cd4, 1, projp$cd4fert_rat, "*")
  fp$frr_cd4 <- fp$frr_cd4 * projp$frr_scalar
  
  fp$frr_art <- array(1.0, c(hTS, hDS, length(h.fert.idx), PROJ_YEARS))
  fp$frr_art[1,,,] <- fp$frr_cd4 # 0-6 months
  fp$frr_art[2:3, , , ] <- sweep(fp$frr_art[2:3, , , ], 3, projp$frr_art6mos[fert_rat.h.ag] * projp$frr_scalar, "*") # 6-12mos, >1 years
  
  ## ART eligibility and numbers on treatment
  
  fp$art15plus_num <- projp$art15plus_num[,as.character(proj_start:proj_end)]
  fp$art15plus_isperc <- projp$art15plus_numperc[, as.character(proj_start:proj_end)] == 1
  
  ## convert percentage to proportion
  fp$art15plus_num[fp$art15plus_isperc] <- fp$art15plus_num[fp$art15plus_isperc] / 100
  
  ## eligibility starts in projection year idx
  fp$specpop_percelig <- rowSums(with(projp$artelig_specpop[-1,], mapply(function(elig, percent, year) rep(c(0, percent*as.numeric(elig)), c(year - proj_start, proj_end - year + 1)), elig, percent, year)))
  fp$artcd4elig_idx <- findInterval(-projp$art15plus_eligthresh[as.character(proj_start:proj_end)], -c(999, 500, 350, 250, 200, 100, 50))
  
  ## Update eligibility threshold from CD4 <200 to <250 to account for additional
  ## proportion eligible with WHO Stage 3/4.
  if(artelig200adj)
    fp$artcd4elig_idx <- replace(fp$artcd4elig_idx, fp$artcd4elig_idx==5L, 4L)
  
  fp$pw_artelig <- with(projp$artelig_specpop["PW",], rep(c(0, elig), c(year - proj_start, proj_end - year + 1)))  # are pregnant women eligible (0/1)
  
  ## percentage of those with CD4 <350 who are based on WHO Stage III/IV infection
  fp$who34percelig <- who34percelig
  
  if (is.null(art_dropout_recover_cd4)) {
    fp$art_dropout_recover_cd4 <- projp$spectrum_version[1] >= "6.14"
  } else {
    stopifnot(is.logical(art_dropout_recover_cd4))
    fp$art_dropout_recover_cd4 <- art_dropout_recover_cd4
  }
  
  
  fp$art_dropout <- projp$art_dropout[as.character(proj_start:proj_end)]/100
  fp$median_cd4init <- projp$median_cd4init[as.character(proj_start:proj_end)]
  fp$med_cd4init_input <- as.integer(fp$median_cd4init > 0)
  fp$med_cd4init_cat <- replace(findInterval(-fp$median_cd4init, - c(1000, 500, 350, 250, 200, 100, 50)),
                                !fp$med_cd4init_input, 0L)
  
  fp$tARTstart <- min(unlist(apply(fp$art15plus_num > 0, 1, which)), PROJ_YEARS)
  
  ## New ART patient allocation options
  fp$art_alloc_method <- projp$art_alloc_method
  fp$art_alloc_mxweight <- projp$art_prop_alloc[1]
  
  ## Scale mortality among untreated population by ART coverage
  fp$scale_cd4_mort <- projp$scale_cd4_mort
  
  ## HIV prevalence and ART coverage among age 15 entrants
  hivpop14 <- projp$age14hivpop[,,,as.character(proj_start:(proj_end-1))]
  pop14 <- projp$age14totpop[ , as.character(proj_start:(proj_end-1))]
  hiv14 <- colSums(hivpop14,,2)
  art14 <- colSums(hivpop14[5:7,,,],,2)
  
  fp$entrantprev <- cbind(0, hiv14/pop14) # 1 year offset because age 15 population is age 14 in previous year
  fp$entrantartcov <- cbind(0, art14/hiv14)
  fp$entrantartcov[is.na(fp$entrantartcov)] <- 0
  colnames(fp$entrantprev) <- colnames(fp$entrantartcov) <- as.character(proj_start:proj_end)
  
  hiv_noart14 <- colSums(hivpop14[1:4,,,])
  artpop14 <- hivpop14[5:7,,,]
  
  fp$paedsurv_cd4dist <- array(0, c(hDS, NG, PROJ_YEARS))
  fp$paedsurv_artcd4dist <- array(0, c(hTS, hDS, NG, PROJ_YEARS))
  
  cd4convert <- rbind(c(1, 0, 0, 0, 0, 0, 0),
                      c(1, 0, 0, 0, 0, 0, 0),
                      c(1, 0, 0, 0, 0, 0, 0),
                      c(0, 1, 0, 0, 0, 0, 0),
                      c(0, 0, 0.67, 0.33, 0, 0, 0),
                      c(0, 0, 0, 0, 0.35, 0.21, 0.44))
  
  ## Convert age 5-14 CD4 distribution to adult CD4 distribution and normalize to
  ## sum to 1 in each sex and year.
  for(g in 1:NG)
    for(i in 2:PROJ_YEARS){
      
      if((hiv14[g,i-1] - art14[g,i-1]) > 0) {
        fp$paedsurv_cd4dist[,g,i] <- hiv_noart14[,g,i-1] %*% cd4convert / (hiv14[g,i-1] - art14[g,i-1])
      }
      if(art14[g,i-1]) {
        fp$paedsurv_artcd4dist[,,g,i] <- artpop14[,,g,i-1] %*% cd4convert / art14[g,i-1]
        
        ## if age 14 has ART population in CD4 above adult eligibilty, assign to highest adult
        ## ART eligibility category.
        idx <- fp$artcd4elig_idx[i]
        if(idx > 1) {
          fp$paedsurv_artcd4dist[,idx,g,i] <- fp$paedsurv_artcd4dist[,idx,g,i] + rowSums(fp$paedsurv_artcd4dist[,1:(idx-1),g,i, drop=FALSE])
          fp$paedsurv_artcd4dist[,1:(idx-1),g,i] <- 0
        }
      }
    }
  
  class(fp) <- "specfp"
  
  return(fp)
}

prepare_inputs = function (pjnzlist) 
{
  pjnz_in <- lapply(pjnzlist, first90::extract_pjnz)
  prepare_inputs_from_extracts(pjnz_in)
}

prepare_inputs_from_extracts = function (pjnz_in) 
{
  pjnz_aggr <- first90::combine_inputs(pjnz_in)
  demp <- list(basepop = pjnz_aggr$totpop, Sx = pjnz_aggr$Sx, 
               asfr = pjnz_aggr$asfr, srb = pjnz_aggr$srb, netmigr = pjnz_aggr$netmigr)
  projp <- pjnz_aggr
  projp$age14totpop <- projp$totpop["14", , ]
  dimnames(projp$fert_rat)[[1]] <- gsub("(.*)-.*", "\\1", dimnames(projp$fert_rat)[[1]])
  fp <- create_fp(projp, demp)
  fp$popadjust <- pjnz_aggr$popadjust
  fp$infections <- pjnz_aggr$infections[fp$ss$AGE_START + fp$ss$p.age15plus.idx, 
                                        , ]
  fp$eppmod <- "directinfections_hts"
  fp$hts_rate <- array(0, c(fp$ss$hAG, fp$ss$NG, fp$ss$pDS, 
                            fp$ss$PROJ_YEARS))
  fp$diagn_rate <- array(0, c(fp$ss$hDS, fp$ss$hAG, fp$ss$NG, 
                              4, fp$ss$PROJ_YEARS))
  fp$t_hts_start <- fp$ss$PROJ_YEARS + 1L
  fp
}




