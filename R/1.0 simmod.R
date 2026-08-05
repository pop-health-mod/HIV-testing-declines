add_ss_indices <- function(dat, ss) {
  df <- dat
  agegr_tmp <- sub("15\\+", "15-99", df$agegr)
  df$agestart <- type.convert(sub("([0-9]+)-([0-9]+)", "\\1",
                                  agegr_tmp), as.is = FALSE)
  ageend <- type.convert(sub("([0-9]+)-([0-9]+)", "\\2", agegr_tmp), as.is = FALSE) +
    1L
  df$aidx <- df$agestart - ss$AGE_START + 1L
  df$agspan <- ageend - df$agestart
  hag_breaks <- ss$agfirst.idx + ss$AGE_START - 1L
  df$haidx <- match(df$agestart, hag_breaks)
  end_haidx <- match(ageend, hag_breaks)
  end_haidx[ageend >= 65] <- length(hag_breaks) + 1L
  df$hagspan <- end_haidx - df$haidx
  df$haidx[df$agestart >= max(hag_breaks) & ageend >= max(hag_breaks)] <- length(hag_breaks)
  df$hagspan[df$agestart >= max(hag_breaks) & ageend >= max(hag_breaks)] <- 1L
  df$sidx <- match(df$sex, c("both", "male", "female")) - 1L
  df$hvidx <- match(df$hivstatus, c("all", "negative", "positive")) - 1L
  df$yidx <- df$year - ss$proj_start + 1L
  if (any(is.na(df$hagspan)))
    warning(paste(sum(is.na(df$hagspan)), "records with non-matching HIV age groups"))
  df
}

simmod_anc_t <- function(fp, anc_true = TRUE, VERSION = "C") {
    ## Attach state space variables
    invisible(list2env(fp$ss, environment())) # put ss variables in environment for convenience
    if (anc_true == FALSE | is.null(fp$anc_rate)) {
      fp$anc_diagn_rate <- array(0, c(hDS, hAG, NG, 2, PROJ_YEARS))
      fp$anc_rate <- array(0, c(hAG, NG, 2, PROJ_YEARS))
    }
  
    if(VERSION != "R") {
      fp$eppmodInt <- match(fp$eppmod, c("rtrend", "directincid", "directinfections", "directinfections_hts"), nomatch = 0) # 0: r-spline;
  
      ## projection_period codes:
      ## 0: mid-year (<= Spectrum 5.19)
      ## 1: calendar year (>= Spectrum 5.2)
      fp$projection_period_int <- match(fp$projection_period, c("midyear", "calendar")) - 1L # -1 for 0-based indexing
      
      sourceCpp(paste0(here::here(src), "/eppasmancretestC.cpp"))
      
      mod <- eppasmancC(fp)
      attr(mod, "hivtests") <- attr(mod,"vcttests") + attr(mod,"anctests")
      class(mod) <- "spec"
      return(mod)
    }
  
    #'#################################################################################
  
    if (requireNamespace("fastmatch", quietly = TRUE)) {
      ctapply <- fastmatch::ctapply
    } else { ctapply <- tapply }
  
  
  fp$ss$DT <- DT <- 1 / fp$ss$hiv_steps_per_year
  anc <- 2
  
  ## initialize projection
  pop <- array(0, c(pAG, NG, pDS, PROJ_YEARS))
  pop[, , 1, 1] <- fp$basepop
  grad_tn_list <- list()
  grad_diagn_list <- list()
  gradART_list <- list()
  hivpop <- array(0, c(hDS, hAG, NG, PROJ_YEARS))
  diagnpop <- array(0, c(hDS, hAG, NG, anc, PROJ_YEARS))
  diagnpop_anc <- array(0, c(hDS, hAG, NG, PROJ_YEARS))
  artpop <- array(0, c(hTS, hDS, hAG, NG, anc, PROJ_YEARS))
  testnegpop <- array(0, c(hAG, NG, pDS, anc, PROJ_YEARS))
  testnegpop_anc <- array(0, c(hAG, NG, pDS, PROJ_YEARS))
  
  ## initialize output
  prev15to49 <- numeric(PROJ_YEARS)
  incid15to49 <- numeric(PROJ_YEARS)
  sexinc15to49out <- array(NA, c(NG, PROJ_YEARS))
  paedsurvout <- rep(NA, PROJ_YEARS)
  
  infections <- array(0, c(pAG, NG, PROJ_YEARS))
  hivdeaths <- array(0, c(pAG, NG, PROJ_YEARS))
  natdeaths <- array(0, c(pAG, NG, PROJ_YEARS))
  
  hivpopdeaths <- array(0, c(hDS, hAG, NG, PROJ_YEARS))
  artpopdeaths <- array(0, c(hTS, hDS, hAG, NG, PROJ_YEARS))
  artdropouts <- array(0, c(hTS, hDS, hAG, NG, anc, PROJ_YEARS))
  
  anctests <- array(0, c(hAG, NG, 6, PROJ_YEARS))
  vcttests <- array(0, c(hAG, NG, 6, PROJ_YEARS))
  hivtests <- array(0, c(hAG, NG, 6, PROJ_YEARS))
  
  diagnoses <- array(0, c(hDS, hAG, NG, anc, PROJ_YEARS))
  
  diagnoses_non_anc <- array(0, c(hDS, hAG, NG, PROJ_YEARS))
  diagnoses_anc <- array(0, c(hDS, hAG, NG, PROJ_YEARS))
  ancdiagnosis <- array(0, c(hDS, hAG, NG, PROJ_YEARS))
  
  late_diagnoses <- array(0, c(hDS, hAG, NG, anc, PROJ_YEARS))
  artinits <- array(0, c(hDS, hAG, NG, anc, PROJ_YEARS))
  popadj.prob <- array(0, c(pAG, NG, PROJ_YEARS))
  entrant_prev_out <- numeric(PROJ_YEARS)
  hivp_entrants_out <- array(0, c(NG, PROJ_YEARS))
  
  if(fp$eppmod != "directincid") {
    ## outputs by timestep
    incrate15to49.ts.out <- rep(NA, length(fp$rvec))
    rvec <- if(fp$eppmod == "rtrend") rep(NA, length(fp$proj.steps)) else fp$rvec
    
    prev15to49.ts.out <- rep(NA, length(fp$rvec))
  }
  
  ## store last prevalence value (for r-trend model)
  prevlast <- 0
  
  for(i in 2:fp$SIM_YEARS){
    ## ################################### ##
    ##  Single-year population projection  ##
    ## ################################### ##
    
    ## age the population
    pop[-c(1, pAG), , , i] <- pop[-(pAG - 1:0), , , i - 1]
    pop[pAG, , , i] <- pop[pAG, , , i - 1] + pop[pAG - 1, , , i - 1] # open age group
    
    ## Add lagged births into youngest age group
    hivn_entrants <- fp$entrantpop[, i - 1] * (1 - fp$entrantprev[ , i])
    hivp_entrants <- fp$entrantpop[, i - 1] * fp$entrantprev[ , i]
    
    entrant_prev_out[i] <- sum(hivp_entrants) / sum(hivn_entrants + hivp_entrants)
    hivp_entrants_out[,i] <- sum(hivp_entrants)
    
    pop[1,,hivn.idx,i] <- hivn_entrants
    pop[1,,hivp.idx,i] <- hivp_entrants
    
    hiv.ag.prob <- pop[aglast.idx,,hivp.idx,i-1] / apply(pop[,,hivp.idx,i-1], 2, ctapply, ag.idx, sum)
    hiv.ag.prob[is.nan(hiv.ag.prob)] <- 0
    hiv.ag.prob[hAG,] <- 0
    
    hivpop[,,,i] <- hivpop[,,,i-1]
    hivpop[,-hAG,,i] <- hivpop[,-hAG,,i] - sweep(hivpop[,-hAG,,i-1], 2:3, hiv.ag.prob[-hAG,], "*")
    hivpop[,-1,,i] <- hivpop[,-1,,i] + sweep(hivpop[, -hAG,,i-1], 2:3, hiv.ag.prob[-hAG,], "*")
    hivpop[,1,,i] <- hivpop[,1,,i] + sweep(fp$paedsurv_cd4dist[,,i], 2, hivp_entrants * (1-fp$entrantartcov[,i]), "*")
    
    if (i > fp$t_hts_start) {
      #after hts starts
      #aging the population
      hivn.ag.prob <- pop[aglast.idx,,hivn.idx,i-1] / apply(pop[,,hivn.idx,i-1], 2, ctapply, ag.idx, sum)
      hivn.ag.prob[is.nan(hivn.ag.prob)] <- 0
      
      testnegpop[,,,,i] <- testnegpop[,,,,i-1]
      
      #same aging prop applies to anc and non anc
      testnegpop[-hAG,,hivn.idx,1,i] <- testnegpop[-hAG,,hivn.idx,1,i] - testnegpop[-hAG,,hivn.idx,1,i-1] * hivn.ag.prob[-hAG,]
      testnegpop[-1,,hivn.idx,1,i] <- testnegpop[-1,,hivn.idx,1,i] + testnegpop[-hAG,,hivn.idx,1,i-1] * hivn.ag.prob[-hAG,]
      
      testnegpop[-hAG,,hivn.idx,anc,i] <- testnegpop[-hAG,,hivn.idx,anc,i] - testnegpop[-hAG,,hivn.idx,anc,i-1] * hivn.ag.prob[-hAG,]
      testnegpop[-1,,hivn.idx,anc,i] <- testnegpop[-1,,hivn.idx,anc,i] + testnegpop[-hAG,,hivn.idx,anc,i-1] * hivn.ag.prob[-hAG,]
      
      #hiv postive
      testnegpop[-hAG,,hivp.idx,1,i] <- testnegpop[-hAG,,hivp.idx,1,i] - testnegpop[-hAG,,hivp.idx,1,i-1] * hiv.ag.prob[-hAG,]
      testnegpop[-1,,hivp.idx,1,i] <- testnegpop[-1,,hivp.idx,1,i] + testnegpop[-hAG,,hivp.idx,1,i-1] * hiv.ag.prob[-hAG,]
      
      testnegpop[-hAG,,hivp.idx,anc,i] <- testnegpop[-hAG,,hivp.idx,anc,i] - testnegpop[-hAG,,hivp.idx,anc,i-1] * hiv.ag.prob[-hAG,]
      testnegpop[-1,,hivp.idx,anc,i] <- testnegpop[-1,,hivp.idx,anc,i] + testnegpop[-hAG,,hivp.idx,anc,i-1] * hiv.ag.prob[-hAG,]
      
      
      ## ageing diagnosed population
      diagnpop[,,,,i] <- diagnpop[,,,,i-1]
      #non-anc
      diagnpop[,-hAG,,1,i] <- diagnpop[,-hAG,,1,i] - sweep(diagnpop[,-hAG,,1,i-1], 2:3, hiv.ag.prob[-hAG,], "*")
      diagnpop[,-1,,1,i] <- diagnpop[,-1,,1,i] + sweep(diagnpop[,-hAG,,1,i-1], 2:3, hiv.ag.prob[-hAG,], "*")
      #anc
      diagnpop[,-hAG,,anc,i] <- diagnpop[,-hAG,,anc,i] - sweep(diagnpop[,-hAG,,anc,i-1], 2:3, hiv.ag.prob[-hAG,], "*")
      diagnpop[,-1,,anc,i] <- diagnpop[,-1,,anc,i] + sweep(diagnpop[,-hAG,,anc,i-1], 2:3, hiv.ag.prob[-hAG,], "*")
      
      ## !! Currently assume that age 15 entrants are not previously tested negative or diagnosed and untreated
      
    }
    
    if (i > fp$tARTstart) {
      
      artpop[,,,,,i] <- artpop[,,,,,i-1]
      
      #non-anc
      artpop[,,-hAG,,1,i] <- artpop[,,-hAG,,1,i] - sweep(artpop[,,-hAG,,1,i-1], 3:4, hiv.ag.prob[-hAG,], "*")
      artpop[,,-1,,1,i] <- artpop[,,-1,,1,i] + sweep(artpop[,,-hAG,,1,i-1], 3:4, hiv.ag.prob[-hAG,], "*")
      #anc
      artpop[,,-hAG,,anc,i] <- artpop[,,-hAG,,anc,i] - sweep(artpop[,,-hAG,,anc,i-1], 3:4, hiv.ag.prob[-hAG,], "*")
      artpop[,,-1,,anc,i] <- artpop[,,-1,,anc,i] + sweep(artpop[,,-hAG,,anc,i-1], 3:4, hiv.ag.prob[-hAG,], "*")
      
      
      #assume all art entrants were not tested at anc
      artpop[,,1,,1,i] <- artpop[,,1,,1,i] + sweep(fp$paedsurv_artcd4dist[,,,i], 3, hivp_entrants * fp$entrantartcov[,i], "*")
    }
    
    ## survive the population
    deaths <- sweep(pop[,,,i], 1:2, (1-fp$Sx[,,i]), "*")
    hiv.sx.prob <- 1-apply(deaths[,,hivp.idx], 2, ctapply, ag.idx, sum) / apply(pop[,,hivp.idx,i], 2, ctapply, ag.idx, sum)
    hiv.sx.prob[is.nan(hiv.sx.prob)] <- 0
    
    if (i > fp$t_hts_start) {
      hivn.sx.prob <- 1-apply(deaths[,,hivn.idx], 2, ctapply, ag.idx, sum) / apply(pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum)
      hivn.sx.prob[is.nan(hivn.sx.prob)] <- 0
    }
    
    pop[,,,i] <- pop[,,,i] - deaths
    natdeaths[,,i] <- rowSums(deaths,,2)
    
    hivpop[,,,i] <- sweep(hivpop[,,,i], 2:3, hiv.sx.prob, "*")
    
    if (i > fp$t_hts_start) {
      #non-anc
      testnegpop[,, hivn.idx,1,i] <- testnegpop[,,hivn.idx,1,i] * hivn.sx.prob
      testnegpop[,, hivp.idx,1,i] <- testnegpop[,,hivp.idx,1,i] * hiv.sx.prob
      
      #anc
      testnegpop[,, hivn.idx,anc,i] <- testnegpop[,,hivn.idx,anc,i] * hivn.sx.prob
      testnegpop[,, hivp.idx,anc,i] <- testnegpop[,,hivp.idx,anc,i] * hiv.sx.prob
      
      diagnpop[,,,,i] <- sweep(diagnpop[,,,,i], 2:3, hiv.sx.prob, "*")
      
    }
    
    if (i > fp$tARTstart){
      artpop[,,,,,i] <- sweep(artpop[,,,,,i], 3:4, hiv.sx.prob, "*")
    }
    
    #assumed to no longer be relevant
    if (fp$projection_period == "midyear") {
      
      ## net migration
      netmigsurv <- fp$netmigr[, , i] * (1 + fp$Sx[, , i]) / 2
      mr.prob <- 1 + netmigsurv / rowSums(pop[, , , i], , 2)
      hiv.mr.prob <- apply(mr.prob * pop[, , hivp.idx, i], 2, ctapply, ag.idx, sum) / apply(pop[, , hivp.idx, i], 2, ctapply, ag.idx, sum)
      hiv.mr.prob[is.nan(hiv.mr.prob)] <- 0
      
      if(i > fp$t_hts_start) {
        hivn.mr.prob <- apply(mr.prob * pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum) /  apply(pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum)
        hivn.mr.prob[is.nan(hivn.mr.prob)] <- 0
      }
      
      pop[,,,i] <- sweep(pop[,,,i], 1:2, mr.prob, "*")
      
      hivpop[,,,i] <- sweep(hivpop[,,,i], 2:3, hiv.mr.prob, "*")
      if(i > fp$t_hts_start) {
        testnegpop[,, hivn.idx,1,i] <- testnegpop[,,hivn.idx,1,i] * hivn.mr.prob
        testnegpop[,, hivn.idx,anc,i] <- testnegpop[,,hivn.idx,anc,i] * hivn.mr.prob
        
        testnegpop[,, hivp.idx,1,i] <- testnegpop[,,hivp.idx,1,i] * hiv.mr.prob
        testnegpop[,, hivp.idx,anc,i] <- testnegpop[,,hivp.idx,anc,i] * hiv.mr.prob
        
        diagnpop[,,,,i] <- sweep(diagnpop[,,,,i], 2:3, hiv.mr.prob, "*")
      }
      if(i > fp$tARTstart)
        artpop[,,,,,i] <- sweep(artpop[,,,,,i], 3:4, hiv.mr.prob, "*")
    }
    
    
    ## fertility
    births.by.age <- rowSums(pop[p.fert.idx, f.idx,,i-1:0])/2 * fp$asfr[,i]
    births.by.h.age <- ctapply(births.by.age, ag.idx[p.fert.idx], sum)
    births <- fp$srb[, i] * sum(births.by.h.age)
    
    ## ################################ ##
    # ----  Disease model simulation  ---# 
    ## ################################ ##
    
    for(ii in seq_len(hiv_steps_per_year)) {
      grad <- array(0, c(hDS, hAG, NG))
      
      ## disease progression and mortality
      grad[-hDS,,] <- grad[-hDS,,] - fp$cd4_prog * hivpop[-hDS,,,i]  # remove cd4 stage progression (untreated)
      grad[-1,,] <- grad[-1,,] + fp$cd4_prog * hivpop[-hDS,,,i]      # add cd4 stage progression (untreated)
      
      if(fp$scale_cd4_mort == 1) {
        cd4mx_scale <- hivpop[,,,i] / (hivpop[,,,i] + apply(artpop[,,,,,i],2:4,sum))
        cd4mx_scale[!is.finite(cd4mx_scale)] <- 1.0
        cd4_mort_ts <- fp$cd4_mort * cd4mx_scale
      } else
        cd4_mort_ts <- fp$cd4_mort
      
      
      grad <- grad - cd4_mort_ts * hivpop[,,,i] # HIV mortality, untreated
      hivdeaths.ts <- cd4_mort_ts * hivpop[,,,i]
      hivdeaths_hAG.ts <- colSums(hivdeaths.ts)
      
      
      #'# ---- Distributing new infections in disease model ----
      if (fp$eppmod == "directinfections_hts") {
        
        ## Calculate annualised new infections by HIV age groups
        infections_ha_sim <- apply(fp$infections[,, i], 2, ctapply, ag.idx, sum)
        grad <- grad + sweep(fp$cd4_initdist, 2:3, infections_ha_sim, "*")
        
        ## move new infections per DT from negative to positive compartment
        pop[,, hivn.idx, i] <- pop[,, hivn.idx, i] - DT * fp$infections[,, i]
        pop[,, hivp.idx, i] <- pop[,, hivp.idx, i] + DT * fp$infections[,, i]
        infections[,,i] <- infections[,,i] + DT * fp$infections[,, i]
      }
      
      ## HIV testing and diagnosis
      if (i >= fp$t_hts_start) {
        
        grad_tn <- array(0, c(hAG, NG, pDS, anc))
        grad_diagn <- array(0, c(hDS, hAG, NG,anc))
        
        ## Number of tests among never tested HIV negative persons
        hivn_pop_ha <- apply(pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum)
        
        vcttests[,,1,i] <- vcttests[,,1,i] + DT * (fp$hts_rate[ , , 1, i] * (hivn_pop_ha - apply(testnegpop[ , , hivn.idx , , i],1:2,sum)))
        anctests[,,1,i] <- anctests[,,1,i] + DT * (fp$anc_rate[ , , 1, i] * (hivn_pop_ha - apply(testnegpop[ , , hivn.idx , , i],1:2,sum)))
        
        #tests amoung those who have ever been tested(negative)
        
        # affects both equally, anc and non anc
        vcttests[,,2,i] <- vcttests[,,2,i] + DT * fp$hts_rate[ , , 2, i] * apply(testnegpop[ , , hivn.idx , , i],1:2,sum)
        
        #' comprises of retesting anc rate among anc 
        #' and the first time anc testing rate specific for ever tested among non anc
        anctests[,,2,i] <-  anctests[,,2,i] +
          DT * fp$anc_rate[ , , 2, i] * apply(testnegpop[ , , hivn.idx ,anc, i],1:2,sum) +
          DT * fp$anc_rate[ , , 1, i] * apply(testnegpop[ , , hivn.idx , 1, i],1:2,sum)
        
        
        # testing for HIV outside anc(new) applies to both
        grad_tn[ , , hivn.idx, 1] <- grad_tn[ , , hivn.idx, 1] + fp$hts_rate[ , , 1, i] * (hivn_pop_ha - apply(testnegpop[ , , hivn.idx , , i],1:2,sum))
        
        grad_tn[ , , hivn.idx, anc] <- grad_tn[ , , hivn.idx, anc] + fp$anc_rate[ , , 1, i] * (hivn_pop_ha - apply(testnegpop[ , , hivn.idx , , i],1:2,sum))
        
        # Add new infections to testneg population
        if (fp$eppmod == "directinfections_hts") {
          
          infections_ha <- apply(fp$infections[,, i], 2, ctapply, ag.idx, sum)
          
          ## number of new infections among never tested (proportional to the HIV-neg pop) and to anc/vct testing
          hivn_pop_ha <- apply(pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum)
          testneg_infections_ha <- infections_ha * (apply(testnegpop[,,hivn.idx,,i],1:2,sum) / hivn_pop_ha)
          
          num <- testnegpop[,,hivn.idx,1,i]
          den <- testnegpop[,,hivn.idx,1,i] + testnegpop[,,hivn.idx,anc,i]
          
          anc_ratio <- ifelse(den > .Machine$double.eps, num/den, 1)   # <-- route 0-den to non-ANC
          anc_ratio[!is.finite(anc_ratio)] <- 1
          anc_ratio <- pmin(pmax(anc_ratio, 0), 1)
          
          alloc_nonanc <- testneg_infections_ha * anc_ratio
          alloc_anc    <- testneg_infections_ha - alloc_nonanc   # residual
          
          # Apply updates
          grad_tn[,,hivn.idx,1]  <- grad_tn[,,hivn.idx,1]  - alloc_nonanc
          grad_tn[,,hivp.idx,1]  <- grad_tn[,,hivp.idx,1]  + alloc_nonanc
          grad_tn[,,hivn.idx,anc] <- grad_tn[,,hivn.idx,anc] - alloc_anc
          grad_tn[,,hivp.idx,anc] <- grad_tn[,,hivp.idx,anc] + alloc_anc
        }
        
        ## Do new diagnoses
        
        ## Remove HIV deaths among tested negative pop
        prop_tn_hivp <- apply(testnegpop[,,hivp.idx,,i],1:2,sum) / apply(hivpop[,,,i],2:3,sum)
        prop_tn_hivp[!is.finite(prop_tn_hivp)] <- 0.0
        
        anc_ratio = testnegpop[,,hivp.idx,1,i]/(testnegpop[,,hivp.idx,1,i] + testnegpop[,,hivp.idx,2,i])
        anc_ratio[is.nan(anc_ratio)] <- 1
        
        grad_tn[ , ,hivp.idx ,1 ] <- grad_tn[ , ,hivp.idx ,1 ] - hivdeaths_hAG.ts * prop_tn_hivp * anc_ratio
        grad_tn[ , ,hivp.idx ,anc ] <- grad_tn[ , ,hivp.idx ,anc ] - hivdeaths_hAG.ts * prop_tn_hivp*(1-anc_ratio)
        
        undiagnosed_i <- pmax(hivpop[,,,i] - apply(diagnpop[,,,,i],1:3,sum),0)
        
        undiag_ha_tot <- apply(undiagnosed_i, 2:3, sum)                 # [hAG, NG]
        
        tn_hivp_ha_tot <- apply(testnegpop[,,hivp.idx,,i], c(1,2,3), sum)    # [hAG, NG]
        
        eps <- .Machine$double.eps
        prop_testneg_ha <- pmin(1, pmax(0, tn_hivp_ha_tot[,,1] / pmax(undiag_ha_tot, eps)))   # [hAG, NG]
        prop_testneg_anc_ha <- pmin(1, pmax(0, tn_hivp_ha_tot[,,anc] / pmax(undiag_ha_tot, eps)))   # [hAG, NG]
        
        prop_testneg <- array(prop_testneg_ha, dim = dim(testnegpop[,,hivp.idx,1,i]))
        
        prop_testneg_anc <- array(prop_testneg_anc_ha, dim = dim(testnegpop[,,hivp.idx,anc,i]))
        
        ## Annualized new diagnoses among never tested population apply to both anc(shouldnt be present)
        diagn_naive <- sweep(sweep(undiagnosed_i, 2:3, 1 - apply(prop_testneg+prop_testneg_anc,1:2,sum), "*"),1:3, fp$diagn_rate[,,,1,i],"*")
        
        diagn_naive_anc <- sweep(sweep(undiagnosed_i, 2:3, 1 - apply(prop_testneg+prop_testneg_anc,1:2,sum), "*"),1:3, fp$anc_diagn_rate[,,,1,i],"*")
        
        ## Annualized new diagnoses among previously tested negative population
        diagn_testneg <- array(data = 0,dim = c(hDS,hAG,NG,anc))
        
        diagn_testneg[,,,1] <- sweep(sweep(undiagnosed_i, 2:3, prop_testneg, "*"),1:3,fp$diagn_rate[,,,2,i],"*")
        
        diagn_testneg[,,,anc] <- sweep(sweep(undiagnosed_i, 2:3, prop_testneg_anc, "*"),1:3,fp$diagn_rate[,,,2,i],"*")
        
        # first test is naive rate among those tested outside anc, 
        # while anc tested have second rate
        diagn_testneg_anc <- array(data = 0,dim = c(hDS,hAG,NG,anc))
        
        diagn_testneg_anc[,,,1] <- fp$anc_diagn_rate[,,,1,i] * sweep(undiagnosed_i, 2:3, prop_testneg, "*")
        diagn_testneg_anc[,,,anc] <- fp$anc_diagn_rate[,,,2,i] * sweep(undiagnosed_i, 2:3, prop_testneg_anc, "*")
        
        # naive
        vcttests[,,3,i] <- vcttests[,,3,i] + DT * apply(diagn_naive,2:3,sum)
        anctests[,,3,i] <- anctests[,,3,i] + DT * apply(diagn_naive_anc,2:3,sum)
        
        #prior tested negative
        vcttests[,,4,i] <- vcttests[,,4,i] + DT * apply(diagn_testneg,2:3,sum)
        anctests[,,4,i] <- anctests[,,4,i] + DT * apply(diagn_testneg_anc,2:3,sum)
        
        # different among those retested normally vs anc
        #diagnosed retested
        vcttests[,,5,i] <- vcttests[,,5,i] +
          colSums(DT * fp$diagn_rate[,,,3,i] * diagnpop[,,,1,i]) +
          colSums(DT * fp$diagn_rate[,,,3,i] * diagnpop[,,,anc,i])
        
        anctests[,,5,i] <- anctests[,,5,i] +  
          colSums(DT * fp$anc_diagn_rate[,,,3,i] * diagnpop[,,,1,i]) +
          colSums(DT * fp$anc_diagn_rate[,,,4,i] * diagnpop[,,,anc,i])
        
        
        # different anc rate anyway captures anc modifications
        #art retested
        vcttests[,,6,i] <- vcttests[,,6,i] +
          colSums(DT * fp$diagn_rate[,,,4,i] * colSums(artpop[,,,,1,i])) +
          colSums(DT * fp$diagn_rate[,,,4,i] * colSums(artpop[,,,,anc,i]))
        
        # same for both as they should have proof of art treatment
        anctests[,,6,i] <- anctests[,,6,i] +   
          colSums(DT * fp$anc_diagn_rate[,,,5,i] * colSums(artpop[,,,,1,i])) +
          colSums(DT * fp$anc_diagn_rate[,,,5,i] * colSums(artpop[,,,,anc,i]))
        
        grad_tn[ , , hivp.idx, 1 ] <- grad_tn[ , , hivp.idx, 1] - apply(diagn_testneg[,,,1],2:3,sum) - apply(diagn_testneg_anc[,,,1],2:3,sum)
        grad_tn[ , , hivp.idx, anc] <- grad_tn[ , , hivp.idx, anc] - apply(diagn_testneg_anc[,,,anc],2:3,sum) - apply(diagn_testneg[,,,anc],2:3,sum)
        
        grad_diagn[ , , , 1] <- grad_diagn[ , , , 1] + 
          apply(diagn_naive,1:3,sum) +
          apply(diagn_testneg[,,,1],1:3,sum)
        
        grad_diagn[ , , , anc] <- grad_diagn[ , , , anc] + 
          apply(diagn_naive_anc,1:3,sum) + 
          apply(diagn_testneg_anc,1:3,sum) + 
          apply(diagn_testneg[,,,anc],1:3,sum)
        
        ## Disease progression and mortality among diagnosed (untreated) population
        
        # non-ANC channel
        grad_diagn[ , , , 1] <- grad_diagn[, , , 1] - fp$cd4_mort * diagnpop[ , , , 1, i]
        
        grad_diagn[-hDS, , , 1] <- grad_diagn[-hDS, , , 1] - fp$cd4_prog * diagnpop[-hDS, , , 1, i]
        grad_diagn[-1 , , , 1] <- grad_diagn[-1, , , 1] + fp$cd4_prog * diagnpop[-hDS,,,1,i]
        
        # ANC channel
        grad_diagn[,,,anc] <- grad_diagn[,,,anc] - fp$cd4_mort * diagnpop[,,,anc,i]
        
        grad_diagn[-hDS,,,anc] <- grad_diagn[-hDS,,,anc] - fp$cd4_prog * diagnpop[-hDS,,,anc,i]
        grad_diagn[-1,,,anc] <- grad_diagn[-1,,,anc] + fp$cd4_prog * diagnpop[-hDS,,,anc,i]
        
        
        if (anc_true == TRUE) {
          hivtests[,,,i] <- vcttests[,,,i] + anctests[,,,i]
          diagnoses[,,,1,i] <- diagnoses[,,,1,i] + DT * (diagn_naive + diagn_testneg[,,,1] +  diagn_testneg[,,,anc])
          diagnoses[,,,anc,i] <- diagnoses[,,,anc,i] + DT * (diagn_testneg_anc[,,,1] + diagn_naive_anc + diagn_testneg_anc[,,,anc])
          diagnpop_anc[,,,i] <- diagnpop_anc[,,,i] + DT * grad_diagn[,,,anc]
          testnegpop_anc[ , , , i] <- testnegpop_anc[ , , , i] + DT * grad_tn[,,,anc ]
          testnegpop[ , , , , i] <- testnegpop[ , , , , i] + DT * (grad_tn[,,,])
          diagnpop[,,,,i] <- diagnpop[,,,,i] + DT * (grad_diagn[,,,])
        } else {
          hivtests[,,,i] <- vcttests[,,,i]
          diagnoses_non_anc[,,,i] <- DT * (diagn_naive + diagn_testneg)
          diagnoses[,,,i] <- diagnoses[,,,i] + diagnoses_non_anc[,,,i]
          testnegpop[ , , , i] <- testnegpop[ , , , i] + DT * grad_tn[,,,]
          diagnpop[,,,i] <- diagnpop[,,,i] + DT * grad_diagn[,,,]
        }
        grad_tn_list[[paste0(i,".",ii)]] = grad_tn[,,,]
        grad_diagn_list[[paste0(i,".",ii)]] = grad_diagn[,,,]
      }
      
      hivpop[,,,i] <- hivpop[,,,i] + DT*grad
      hivpopdeaths[,,, i] <- hivpopdeaths[,,, i] + DT * hivdeaths.ts
      
      ## ART population
      if(i >= fp$tARTstart) {
        gradART <- array(0, c(hTS, hDS, hAG, NG,anc))
        
        ## progression and mortality
        gradART[1:2,,,,1] <- gradART[1:2,,,,1] - 2.0 * artpop[1:2,,,,1, i]      # remove ART duration progression (HARD CODED 6 months duration)
        gradART[2:3,,,,1] <- gradART[2:3,,,,1] + 2.0 * artpop[1:2,,,,1, i]      # add ART duration progression (HARD CODED 6 months duration)
        
        ## progression and mortality
        gradART[1:2,,,,anc] <- gradART[1:2,,,,anc] - 2.0 * artpop[1:2,,,,anc, i]      # remove ART duration progression (HARD CODED 6 months duration)
        gradART[2:3,,,,anc] <- gradART[2:3,,,,anc] + 2.0 * artpop[1:2,,,,anc, i]      # add ART duration progression (HARD CODED 6 months duration)
        
        artdeaths.ts <- sweep(artpop[,,,,,i],1:4,fp$art_mort * fp$artmx_timerr[ , i],"*")
        gradART <- gradART - artdeaths.ts                  # ART mortality
        
        hivdeaths_hAG.ts <- hivdeaths_hAG.ts + apply(artdeaths.ts,3:4,sum)
        
        artpop[,,,,, i] <- artpop[,,,,, i] + DT * gradART
        artpopdeaths[,,,, i] <- artpopdeaths[,,,, i] + DT * apply(artdeaths.ts,1:4,sum)
        
        ## ART dropout
        ## remove proportion from all adult ART groups back to untreated pop
        art_dropout_ii <- fp$art_dropout[i] * colSums(artpop[1:2,,,,,i])
        
        if (fp$art_dropout_recover_cd4) {
          
          art_dropout_ii[1,,,] <- art_dropout_ii[1,,,] +
            fp$art_dropout[i] * artpop[3:fp$ss$hTS,1,,,,i]
          
          art_dropout_ii[-fp$ss$hDS,,,] <- art_dropout_ii[-fp$ss$hDS,,,] +
            fp$art_dropout[i] * artpop[3:fp$ss$hTS,-1,,,,i]
          
        } else {
          
          art_dropout_ii <- art_dropout_ii +
            fp$art_dropout[i] * artpop[3:fp$ss$hTS,,,,,i]
        }
        
        hivpop[,,,i] <- hivpop[,,,i] + DT * apply(art_dropout_ii,1:3,sum)
        
        
        if(i >= fp$t_hts_start) {
          diagnpop[,,,,i] <- diagnpop[,,,,i] + DT * art_dropout_ii
        }
        artdropouts[,,,,,i] <- artdropouts[,,,,,i] + DT * fp$art_dropout[i]*artpop[,,,,,i]
        
        artpop[,,,,,i] <- artpop[,,,,,i] - DT * fp$art_dropout[i]*artpop[,,,,,i]
        
        ## calculate number eligible for ART
        artcd4_percelig <- 1 - (1-rep(0:1, times = c(fp$artcd4elig_idx[i]-1, hDS - fp$artcd4elig_idx[i]+1))) *
          (1-rep(c(0, fp$who34percelig), c(2, hDS-2))) *
          (1-rep(fp$specpop_percelig[i], hDS))
        
        art15plus.elig <- sweep(hivpop[,h.age15plus.idx,,i], 1, artcd4_percelig, "*")
        
        ## calculate pregnant women
        if(fp$pw_artelig[i]) {
          births.dist <- sweep(
            sweep(hivpop[,h.fert.idx,f.idx,i],1:2, fp$frr_cd4[,,i], "*"), 2,
            births.by.h.age / (ctapply(pop[p.fert.idx, f.idx, hivn.idx, i], ag.idx[p.fert.idx], sum) +
                                 colSums(sweep(hivpop[,h.fert.idx,f.idx,i],1:2, fp$frr_cd4[,,i],"*")) +
                                 apply(sweep(artpop[ ,,h.fert.idx,f.idx,,i],1:3,fp$frr_art[,,,i],"*"),3,sum)),"*")
          
          
          
          if(fp$artcd4elig_idx[i] > 1){
            art15plus.elig[1:(fp$artcd4elig_idx[i]-1),h.fert.idx-min(h.age15plus.idx)+1,f.idx ] <- 
              art15plus.elig[1:(fp$artcd4elig_idx[i]-1),h.fert.idx-min(h.age15plus.idx)+1,f.idx] + 
              births.dist[1:(fp$artcd4elig_idx[i]-1),] # multiply by DT to account for proportion of annual births occurring during this time step
            
          }
        }
        ## calculate number to initiate ART based on number or percentage
        
        artnum.ii <- c(0, 0) # number on ART this ts
        if (fp$projection_period == "midyear" && DT*ii < 0.5) {
          for(g in 1:2) {
            if(!any(fp$art15plus_isperc[g,i-2:1])) {  # both number
              artnum.ii[g] <- c(fp$art15plus_num[g,i-2:1] %*% c(1-(DT*ii+0.5), DT*ii+0.5))
            } else if(all(fp$art15plus_isperc[g,i-2:1])) {  # both percentage
              artcov.ii <- c(fp$art15plus_num[g,i-2:1] %*% c(1-(DT*ii+0.5), DT*ii+0.5))
              artnum.ii[g] <- artcov.ii * (sum(art15plus.elig[,,g]) + sum(artpop[,,h.age15plus.idx,g,i]))
            } else if(!fp$art15plus_isperc[g,i-2] & fp$art15plus_isperc[g,i-1]) { # transition number to percentage
              curr_coverage <- sum(artpop[,,h.age15plus.idx,g,i]) / (sum(art15plus.elig[,,g]) + sum(artpop[,,h.age15plus.idx,g,i]))
              artcov.ii <- curr_coverage + (fp$art15plus_num[g,i-1] - curr_coverage) * DT/(0.5-DT*(ii-1))
              artnum.ii[g] <- artcov.ii * (sum(art15plus.elig[,,g]) + sum(artpop[,,h.age15plus.idx,g,,i]))
            }
          }
        } else {
          
          for(g in 1:2){
            
            #calculate the number of PLHIV on ART this year
            art_interp_w <- DT*ii
            if (fp$projection_period == "midyear") {
              art_interp_w <- art_interp_w - 0.5
            }
            if(!any(fp$art15plus_isperc[g,i-1:0])) {  # both number
              artnum.ii[g] <- c(fp$art15plus_num[g,i-1:0] %*% c(1-art_interp_w, art_interp_w))
            } else if(all(fp$art15plus_isperc[g,i-1:0])) {  # both percentage
              artcov.ii <- c(fp$art15plus_num[g,i-1:0] %*% c(1-art_interp_w, art_interp_w))
              artnum.ii[g] <- artcov.ii * (sum(art15plus.elig[,,g]) + sum(artpop[,,h.age15plus.idx,g,,i]))
            } else if(!fp$art15plus_isperc[g,i-1] & fp$art15plus_isperc[g,i]) {  # transition number to percentage
              curr_coverage <- sum(artpop[,,h.age15plus.idx,g,,i]) / (sum(art15plus.elig[,,g]) + sum(artpop[,,h.age15plus.idx,g,,i]))
              artcov.ii <- curr_coverage + (fp$art15plus_num[g,i] - curr_coverage) * DT/(1.0 - art_interp_w + DT)
              artnum.ii[g] <- artcov.ii * (sum(art15plus.elig[,,g]) + sum(artpop[,,h.age15plus.idx,g,,i]))
            }
          }
        }
        #calculate the number of PLHIV that will initiate ART this year
        art15plus.inits <- pmax(artnum.ii - apply(artpop[,,h.age15plus.idx,,,i],4,sum), 0)
        
        ## calculate ART initiation distribution
        if(!fp$med_cd4init_input[i]) {
          
          if(fp$art_alloc_method == 4L) { ## by lowest CD4
            
            ## Calculate proportion to be initiated in each CD4 category
            artinit <- array(0, dim(art15plus.elig))
            remain_artalloc <- art15plus.inits
            for(m in hDS:1) {
              elig_hm <- colSums(art15plus.elig[m,,,],,2)
              init_prop <- ifelse(elig_hm == 0, elig_hm, pmin(1.0, remain_artalloc / elig_hm, na.rm=TRUE))
              artinit[m , , ,] <- sweep(art15plus.elig[m,,,], 2, init_prop, "*")
              remain_artalloc <- remain_artalloc - init_prop * elig_hm
            }
            
          } else {
            
            expect.mort.weight = array(0,dim(art15plus.elig))
            expect.mort.weight[,,] <- sweep(fp$cd4_mort[, h.age15plus.idx,], 3,
                                            apply(art15plus.elig[,,] * fp$cd4_mort[, h.age15plus.idx,],3,sum), "/")
            
            expect.mort.weight[is.nan(expect.mort.weight)] = 0;expect.mort.weight[is.infinite(expect.mort.weight)] = 0
            
            artinit.weight <- sweep(fp$art_alloc_mxweight * expect.mort.weight, 3, 
                                    (1 - fp$art_alloc_mxweight)/apply(art15plus.elig,3,sum), "+")
            artinit.weight[is.nan(artinit.weight)] = 0;artinit.weight[is.infinite(artinit.weight)] = 0
            
            ## apply stratification based on current diagnpop levels
            diag_tot  <- apply(diagnpop[ , , , , i], 1:3, sum)               # [hAG, NG]
            art_anc_ratio = array(0,dim(apply(diagnpop[ , , , , i], 1:4, sum)  ))
            art_anc_ratio[,,,1] <- sweep(apply(diagnpop[ , , , 1, i], 1:3, sum), 1:3, pmax(diag_tot, .Machine$double.eps), "/")  
            # very small numerator/denominator combos cause issues when rounding occurs, 
            # this biases art inits into vct when there is very little diagnpop in either
            art_anc_ratio[,,,1][diag_tot< 1e-9] <- 1
            art_anc_ratio[!is.finite(art_anc_ratio)] <- 0; art_anc_ratio = pmin(pmax(art_anc_ratio,0),1)
            
            art_anc_ratio[,,,2] <- 1-art_anc_ratio[,,,1]
            
            artinit <- array(data = 0,dim = c(dim(art15plus.elig),anc))
            preartinit <- pmin(sweep(artinit.weight * art15plus.elig, 3, art15plus.inits, "*"),
                               art15plus.elig, na.rm=TRUE)
            
            artinit[,,,1] = sweep(preartinit,1:3,art_anc_ratio[,,,1],"*")
            
            artinit[,,,2] = sweep(preartinit,1:3,art_anc_ratio[,,,anc],"*")
            
          }
          
        } else {
          
          CD4_LOW_LIM <- c(500, 350, 250, 200, 100, 50, 0)
          CD4_UPP_LIM <- c(1000, 500, 350, 250, 200, 100, 50)
          
          medcd4_idx <- fp$med_cd4init_cat[i]
          
          medcat_propbelow <- (fp$median_cd4init[i] - CD4_LOW_LIM[medcd4_idx]) / (CD4_UPP_LIM[medcd4_idx] - CD4_LOW_LIM[medcd4_idx])
          
          elig_below <- colSums(art15plus.elig[medcd4_idx,,,,drop=FALSE],,3) * medcat_propbelow
          if(medcd4_idx < hDS)
            elig_below <- elig_below + colSums(art15plus.elig[(medcd4_idx+1):hDS,,,,drop=FALSE],,3)
          
          elig_above <- colSums(art15plus.elig[medcd4_idx,,,,drop=FALSE],,3) * (1.0-medcat_propbelow)
          if(medcd4_idx > 1)
            elig_above <- elig_above + colSums(art15plus.elig[1:(medcd4_idx-1),,,,drop=FALSE],,3)
          
          initprob_below <- pmin(art15plus.inits * 0.5 / elig_below, 1.0, na.rm=TRUE)
          initprob_above <- pmin(art15plus.inits * 0.5 / elig_above, 1.0, na.rm=TRUE)
          initprob_medcat <- initprob_below * medcat_propbelow + initprob_above * (1-medcat_propbelow)
          
          artinit <- array(0, dim = c(hDS, hAG, NG,anc))
          
          if(medcd4_idx < hDS)
            artinit[(medcd4_idx + 1):hDS,,,] <- sweep(art15plus.elig[(medcd4_idx+1):hDS,,,,drop=FALSE], 4, initprob_below, "*")
          
          artinit[medcd4_idx,,,] <- sweep(art15plus.elig[medcd4_idx,,,,drop=FALSE], 4, initprob_medcat, "*")
          
          if(medcd4_idx > 0)
            artinit[1:(medcd4_idx-1),,,] <- sweep(art15plus.elig[1:(medcd4_idx-1),,,,drop=FALSE], 4, initprob_above, "*")
        }
        
        if (i >= fp$t_hts_start) {
          
          ## 'newdiagn' is the number of new diagnoses are in deficit in
          ## the diagnosed population to match ART initiations
          newdiagn <- pmax(artinit - diagnpop[,,,,i], 0)
          diagn_surplus <- pmax(diagnpop[,,,,i] - artinit, 0)
          
          # broadcast over when multiplying diagn_surplus
          eps <- .Machine$double.eps
          # remove ANC dimension
          num <- apply(newdiagn,2:4, sum)                 
          den <- pmax(apply(diagn_surplus, 2:4, sum), eps)      
          frac_exc_pool <- num / den
          frac_exc_pool[!is.finite(frac_exc_pool)] <- 0
          frac_exc_pool[frac_exc_pool > 1] <- 1
          
          # same total as legacy, but split by ANC/VCT according to available surplus
          to_put_back <- sweep(apply(diagn_surplus,1:4,sum), 2:4, frac_exc_pool, "*")
          
          ratio_anc = apply(diagn_surplus[,,,1],2:3,sum)/(apply(diagn_surplus[,,,],2:3,sum))
          ratio_anc[is.nan(ratio_anc)] <-0
          ratio_anc[!is.finite(ratio_anc)] <- 0; ratio_anc[ratio_anc > 1] <- 1
          
          ## 'prop_testneg' calculates the proportion of the undiagnosed HIV+
          ## population who have previously tested negative
          prop_testneg <- apply(testnegpop[ , , hivp.idx, , i],1:2,sum) / colSums(hivpop[,,,i] - apply(diagnpop[,,,,i],1:3,sum))
          prop_testneg[is.na(prop_testneg) | prop_testneg > 1 | prop_testneg < 0] <- 0
          
          late_diagnoses[,,,,i] <- late_diagnoses[,,,,i] + newdiagn[,,,] - to_put_back[,,,]
          diagnoses[,,,,i] <- diagnoses[,,,,i] + newdiagn - to_put_back
          
          # Apply the change
          diagnpop_new <- diagnpop[,,,,i] -  (artinit - newdiagn + to_put_back)
          
          # Identify where we have dont have enough PLHIV to return and swaps them to opposite channel
          negatives <- pmin(diagnpop_new, 0)  # captures only negative values
          diagnpop_new <- pmax(diagnpop_new, 0)  # set negatives to 0
          
          # Transfer the deficit to the other stratum
          # If anc (dim 4, index 1) is negative, take from vct (index 2) and vice versa
          diagnpop_new[,,,1] <- diagnpop_new[,,,1] + negatives[,,,2]  
          diagnpop_new[,,,2] <- diagnpop_new[,,,2] + negatives[,,,1]  
          
          diagnpop_new <- pmax(diagnpop_new, 0)
          
          diagnpop[,,,,i] <- diagnpop_new
          
          ## here, the 'testnegpop' now becomes aware according to their relative proportion.
          newdiagn_ha <- newdiagn - to_put_back
          
          vcttests[ , , 3, i] <- vcttests[ , , 3, i] + apply(sweep(newdiagn_ha[,,,1],2:3,(1 - prop_testneg),"*"),2:3,sum)+ apply(sweep(newdiagn_ha[,,,2],2:3,(1 - prop_testneg),"*"),2:3,sum)
          vcttests[ , , 4, i] <- vcttests[ , , 4, i] + apply(sweep(newdiagn_ha[,,,1],2:3,(prop_testneg),"*"),2:3,sum)+ apply(sweep(newdiagn_ha[,,,2],2:3,(prop_testneg),"*"),2:3,sum)
          # assume all late testing is through VCT
          
          #remove from testnegpop proportionally
          testnegpop_new <- testnegpop[ , , hivp.idx, , i] -  apply(sweep(newdiagn_ha[,,,],2:3,prop_testneg,"*"),2:4,sum)
          
          # Identify where we have negatives
          testnegpop_negatives <- pmin(testnegpop_new, 0)  # captures only negative values
          testnegpop_new <- pmax(testnegpop_new, 0)  # set negatives to 0
          
          # Transfer the deficit to the other stratum
          # If anc (dim 4, index 1) is negative, take from vct (index 2) and vice versa
          testnegpop_new[,,1] <- testnegpop_new[,,1] + testnegpop_negatives[,,2]  # anc gets vct's deficit
          testnegpop_new[,,2] <- testnegpop_new[,,2] + testnegpop_negatives[,,1]  # vct gets anc's deficit
          
          testnegpop[ , , hivp.idx, , i] <-  testnegpop_new
          
        }
        
        hivpop[, h.age15plus.idx,, i] <- hivpop[, h.age15plus.idx,,  i] - apply(artinit,1:3,sum)
        artpop[1,, h.age15plus.idx,,, i] <- artpop[1,, h.age15plus.idx,,, i] + artinit
        artinits[,,,,i] <- artinits[,,,,i] + artinit
        
        gradART_list[[paste0(i,".",ii)]] = gradART
      }
      ## Remove hivdeaths from pop
      
      calc.agdist <- function(x) { d <- x / rep(ctapply(x, ag.idx, sum), h.ag.span); d[is.na(d)] <- 0; d }
      
      hivdeaths_p.ts <- apply(DT * hivdeaths_hAG.ts, 2, rep, h.ag.span) * apply(pop[,,hivp.idx,i], 2, calc.agdist)  # HIV deaths by single-year age
      
      pop[,,hivp.idx,i] <- pop[,,hivp.idx,i] - hivdeaths_p.ts
      hivdeaths[,,i] <- hivdeaths[,,i] + hivdeaths_p.ts
      
    }
    
    #' ---- End Disease Model ----
    
    ## ## Code for calculating new infections once per year to match prevalence (like Spectrum)
    ## ## incidence
    ## prev.i <- sum(pop[p.age15to49.idx,,2,i]) / sum(pop[p.age15to49.idx,,,i]) # prevalence age 15 to 49
    ## incrate15to49.i <- (fp$prev15to49[i] - prev.i)/(1-prev.i)
    
    ## Direct incidence input: unlikely to be relevant
    if(fp$eppmod %in% c("directincid", "directinfections")) {
      if(fp$eppmod == "directincid") {
        if(fp$incidpopage == 0L) # incidence for 15-49 population
          p.incidpop.idx <- p.age15to49.idx
        else if(fp$incidpopage == 1L) # incidence for 15+ population
          p.incidpop.idx <- p.age15plus.idx
        incrate.i <- fp$incidinput[i]
        
        sexinc <- incrate.i*c(1, fp$incrr_sex[i])*sum(pop[p.incidpop.idx,,hivn.idx,i-1])/(sum(pop[p.incidpop.idx,m.idx,hivn.idx,i-1]) + fp$incrr_sex[i]*sum(pop[p.incidpop.idx, f.idx,hivn.idx,i-1]))
        agesex.inc <- sweep(fp$incrr_age[,,i], 2, sexinc/(colSums(pop[p.incidpop.idx,,hivn.idx,i-1] * fp$incrr_age[p.incidpop.idx,,i])/colSums(pop[p.incidpop.idx,,hivn.idx,i-1])), "*")
        infections[,,i] <- agesex.inc * pop[,,hivn.idx,i-1]
      } else if(fp$eppmod == "directinfections") {
        infections[,,i] <- fp$infections[,,i]
      }
      
      infections_ha <- apply(infections[,,i], 2, ctapply, ag.idx, sum)
      
      if(i >= fp$t_hts_start) {
        hivn_pop_ha <- apply(pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum)
        testneg_infections_ha <- infections_ha * testnegpop[,,hivn.idx,1,i] / hivn_pop_ha
        testneg_infections_ha_anc <- infections_ha * testnegpop[,,hivn.idx,anc,i] / hivn_pop_ha
        
        
        testnegpop[ , , hivn.idx, 1, i] <- testnegpop[ , , hivn.idx, 1, i] - testneg_infections_ha
        testnegpop[ , , hivp.idx, 1, i] <- testnegpop[ , , hivp.idx, 1, i] + testneg_infections_ha
        
        testnegpop[ , , hivn.idx, anc, i] <- testnegpop[ , , hivn.idx, anc, i] - testneg_infections_ha_anc
        testnegpop[ , , hivp.idx, anc, i] <- testnegpop[ , , hivp.idx, anc, i] + testneg_infections_ha_anc
        
        
      }
      
      pop[,,hivn.idx,i] <- pop[,,hivn.idx,i] - infections[,,i]
      pop[,,hivp.idx,i] <- pop[,,hivp.idx,i] + infections[,,i]
      
      hivpop[,,,i] <- hivpop[,,,i] + sweep(fp$cd4_initdist, 2:3, infections_ha, "*")
    } # if(fp$eppmod %in% c("directincid", "directinfections"))
    
    if (fp$projection_period == "calendar") {
      ## net migration
      netmigsurv <- fp$netmigr[,,i]
      mr.prob <- 1+netmigsurv / rowSums(pop[,,,i],,2)
      hiv.mr.prob <- apply(mr.prob * pop[,,hivp.idx,i], 2, ctapply, ag.idx, sum) /  apply(pop[,,hivp.idx,i], 2, ctapply, ag.idx, sum)
      hiv.mr.prob[is.nan(hiv.mr.prob)] <- 0
      
      if(i > fp$t_hts_start) {
        hivn.mr.prob <- apply(mr.prob * pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum) /  apply(pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum)
        hivn.mr.prob[is.nan(hivn.mr.prob)] <- 0
      }
      pop[,,,i] <- sweep(pop[,,,i], 1:2, mr.prob, "*")
      
      hivpop[,,,i] <- sweep(hivpop[,,,i], 2:3, hiv.mr.prob, "*")
      if(i > fp$t_hts_start) {
        #non-anc
        testnegpop[,, hivn.idx,1,i] <- testnegpop[,,hivn.idx,1,i] * hivn.mr.prob
        testnegpop[,, hivp.idx,1,i] <- testnegpop[,,hivp.idx,1,i] * hiv.mr.prob
        
        #anc
        testnegpop[,, hivn.idx,anc,i] <- testnegpop[,,hivn.idx,anc,i] * hivn.mr.prob
        testnegpop[,, hivp.idx,anc,i] <- testnegpop[,,hivp.idx,anc,i] * hiv.mr.prob
        
        diagnpop[,,,,i] <- sweep(diagnpop[,,,,i], 2:3, hiv.mr.prob, "*")
        
      }
      if(i >= fp$tARTstart)
        artpop[,,,,,i] <- sweep(artpop[,,,,,i], 3:4, hiv.mr.prob, "*")
    }
    
    # adjust HIV population to match target population size
    if(fp$popadjust) {
      hivpop_ha <- apply(hivpop[,,,i],2:3,sum)
      hivpop_ha_adj <- fp$target_hivpop_ha[,,i] / hivpop_ha
      hivpop_ha_adj[!is.finite(hivpop_ha_adj)] <- 1.0
      hivpop[,,,i] <- sweep(hivpop[,,,i], 2:3, hivpop_ha_adj, "*")
      
      if(i >= fp$t_hts_start) {
        
        hivn_ha <- apply(pop[,,hivn.idx,i], 2, ctapply, ag.idx, sum)
        hivn_ha_adj <- fp$target_hivn_ha[,,i] / hivn_ha
        hivn_ha_adj[!is.finite(hivn_ha_adj)] <- 1.0
        
        #non-anc        
        testnegpop[,,hivn.idx,1,i] <- testnegpop[,,hivn.idx,1,i] * hivn_ha_adj
        testnegpop[,,hivp.idx,1,i] <- testnegpop[,,hivp.idx,1,i] * hivpop_ha_adj
        #anc
        testnegpop[,,hivn.idx,anc,i] <- testnegpop[,,hivn.idx,anc,i] * hivn_ha_adj
        testnegpop[,,hivp.idx,anc,i] <- testnegpop[,,hivp.idx,anc,i] * hivpop_ha_adj
        
        diagnpop[,,,,i] <- sweep(diagnpop[,,,,i], 2:3, hivpop_ha_adj, "*")
      }
      
      if(i >= fp$tARTstart) {
        artpop_ha <- apply(artpop[,,,,,i],c(3:4),sum)
        artpop_ha_adj <- fp$target_artpop_ha[,,i] / artpop_ha
        
        artpop_ha_adj[!is.finite(artpop_ha_adj)] <- 1.0
        
        
        artpop[,,,,,i] <- sweep(artpop[,,,,,i], 3:4, artpop_ha_adj, "*")
      }
      
      pop[,,hivn.idx,i] <- fp$target_hivn_pop[,,i]
      pop[,,hivp.idx,i] <- fp$target_hivp_pop[,,i]
    }
    
    ## prevalence and incidence 15 to 49
    prev15to49[i] <- sum(pop[p.age15to49.idx,,hivp.idx,i]) / sum(pop[p.age15to49.idx,,,i])
    
    if (fp$projection_period == "calendar") {
      ## incidence: interpolated denominator
      incid15to49_denom <- 0.5 * (sum(pop[p.age15to49.idx,,hivn.idx,i-1]) + sum(pop[p.age15to49.idx,,hivn.idx,i]))      
    } else {
      incid15to49_denom <- sum(pop[p.age15to49.idx,,hivn.idx,i-1])
    }
    incid15to49[i] <- sum(infections[p.age15to49.idx,,i]) / incid15to49_denom
    
  }
  
    
    
    attr(pop, "prev15to49") <- prev15to49
    attr(pop, "incid15to49") <- incid15to49
    #attr(pop, "sexinc") <- sexinc15to49out
    attr(pop, "hivpop") <- hivpop
    attr(pop, "diagnpop") <- diagnpop
    attr(pop, "artpop") <- artpop
    attr(pop, "testnegpop") <- testnegpop
    attr(pop, "infections") <- infections
    attr(pop, "hivdeaths") <- hivdeaths
    attr(pop, "natdeaths") <- natdeaths
    attr(pop, "hivpopdeaths") <- hivpopdeaths
    attr(pop, "artpopdeaths") <- artpopdeaths
    attr(pop, "hivtests") <- vcttests+anctests
    attr(pop, "diagnoses") <- diagnoses
    attr(pop, "late_diagnoses") <- late_diagnoses
    attr(pop, "artinits") <- artinits
    attr(pop, "popadjust") <- popadj.prob
    attr(pop, "vcttests") <- vcttests
    attr(pop, "artdropouts") <- artdropouts

    attr(pop, "gradtnlist") = grad_tn_list
    attr(pop, "gradartlist") = gradART_list
    
    anc_true <- TRUE
    if (anc_true == TRUE) {
      attr(pop, "diagnpop_anc") <- diagnpop_anc
      attr(pop, "diagnoses_non_anc") <- diagnoses_non_anc
      attr(pop, "diagnoses_anc") <- diagnoses_anc
      attr(pop, "anctests") <- anctests
      attr(pop, "testnegpop_anc") <- testnegpop_anc
    }
    if(fp$eppmod != "directincid") {
      attr(pop, "incrate15to49_ts") <- incrate15to49.ts.out
      attr(pop, "prev15to49_ts") <- prev15to49.ts.out
    }
    attr(pop, "entrantprev") <- entrant_prev_out
    #attr(pop, "hivp_entrants") <- hivp_entrants_out
    class(pop) <- "spec"
    return(pop)
  
  }
create_anc_param <- function(theta, fp, pmtct, hivdemo_proj, subvar = list(), verbose = FALSE) {
    invisible(list2env(fp$ss, environment()))
    ###defines length based on theta length
    theta_fx <- c(0, 0, 0.27, 0.27, rep(0.9, 3))
    
    if (any(is.na(theta))) { stop("error: you have NAs in theta") }
    
    max_knot_year <- if (length(theta) == 44) {
      2019
    } else if (length(theta) == 46) {
      2020
    } else if (length(theta) == 48) {
      2021
    } else if (length(theta) == 50) {
      2022
    } else if (length(theta) == 52) {
      2023
    } else if (length(theta) == 54) {
      2024
    } else {
      stop("Unexpected length of parameter vector.")
    }
    
    if (verbose) { print(paste0("according to your initial parameters, you are modeling up to: ", max_knot_year)) }
    
    knots <- c(1995, 2000:max_knot_year) - fp$ss$proj_start + 1L
    knots_rr_dxunt <- c(2010:max_knot_year) - fp$ss$proj_start + 1L
    n_k1 <- length(knots) - 1
    n_k2 <- n_k1 + length(knots_rr_dxunt)
    # new - these are the anc-related parameters (n_k3 correspond to the first parameter for anc fitting)
    n_k3 <- n_k2 + 12
    # baseline rates for women
    rate_f <- exp(theta[1:n_k1])
    rr_dxunt <- stats::plogis(theta[(n_k1 + 1):n_k2]) * 8
    
    #rr for men, both inflection points (notice change in prior/transformation)
    rr_m <- 0.05 + stats::plogis(theta[(n_k2 + 1):(n_k2 + 2)]) * (10 - 0.05)
    rr_test <- 0.95 + stats::plogis(theta[(n_k2 + 3):(n_k2 + 4)]) * 7.05
    rr_plhiv <- 0.05 + stats::plogis(theta[n_k2 + 5]) * (1.95 -  0.05)
    rr_dxart <- stats::plogis(theta[n_k2 + 6])
    rr_25m <- 0.1 + stats::plogis(theta[n_k2 + 7]) * (6 - 0.1)
    rr_35m <- 0.1 + stats::plogis(theta[n_k2 + 8]) * (6 - 0.1)
    rr_25f <- 0.1 + stats::plogis(theta[n_k2 + 9]) * (6 - 0.1)
    rr_35f <- 0.1 + stats::plogis(theta[n_k2 + 10]) * (6 - 0.1)
    pr_oidx <- 0.25 + (stats::plogis(theta[n_k2 + 11]) * (1.75 -0.25))
    
    # age effect for males and females
    agefn_m <- c(rep(1, 3), rep(rr_25m, 2), rep(rr_35m, 3), rr_35m * 0.8112)
    agefn_f <- c(rep(1, 3), rep(rr_25f, 2), rep(rr_35f, 3), rr_35f * 0.8183)
    
    # time trends in (non-anc) testing rates
    fp$t_hts_start <- as.integer(1995 - fp$ss$proj_start + 1L)
    
    # adds the extra years of no testing to the female rate
    base_rate_f <- stats::approx(knots, c(0, rate_f), seq_len(fp$ss$PROJ_YEARS), rule = 2)$y
    #defines knots (i.e, we still allow the male-to-female ratio to vary with time, 2 inflection points)
    knots_rr_m <- c(2005, 2012) - fp$ss$proj_start + 1L
    
    # assigns knot locations
    rr_m <- stats::approx(knots_rr_m, rr_m, seq_len(fp$ss$PROJ_YEARS), rule = 2)$y
    # male rate
    base_rate_m <- base_rate_f * rr_m
    
    # same for re-testing
    knots_rr_test <- c(2005, 2010, 2015) - fp$ss$proj_start +  1L
    rate_rr_test <- exp(stats::approx(knots_rr_test, 
                                      log(c(1, rr_test)), seq_len(fp$ss$PROJ_YEARS), rule = 2)$y)
    
    # testing rate for hiv- population
    # 1: never tested
    # 2: previously tested negative
    # sets a empty(1) array up 
    hts_rate <- array(1.0, c(fp$ss$hAG, fp$ss$NG, fp$ss$pDS, fp$ss$PROJ_YEARS))
    
    # defines rates for male and female same values for all age and hiv groups
    hts_rate_m <- sweep(hts_rate[, 1, , , drop = FALSE], 4, base_rate_m, "*")
    hts_rate_f <- sweep(hts_rate[, 2, , , drop = FALSE], 4, base_rate_f, "*")
    # assigns
    hts_rate[, 1, , ] <- hts_rate_m
    hts_rate[, 2, , ] <- hts_rate_f
    
    #age groups, sweeps on 1 so probably
    hts_rate_am <- sweep(hts_rate_m[, , , , drop = FALSE], 1, agefn_m, "*")
    hts_rate_af <- sweep(hts_rate_f[, , , , drop = FALSE], 1, agefn_f, "*")
    hts_rate[, 1, , ] <- hts_rate_am
    hts_rate[, 2, , ] <- hts_rate_af
    
    # HIV negative re-testing (i.e., 2 = previously tested negative)
    hts_rate[, , 2, ] <- sweep(hts_rate[, , 2, , drop = FALSE], 4, rate_rr_test, "*")
    
    # testing rate for hiv+ population
    # 1: never tested
    # 2: previously tested neg
    # 3: diagnosed
    # 4: on art
    # empty array
    diagn_rate <- array(1.0, c(fp$ss$hDS, fp$ss$hAG, fp$ss$NG, 4, fp$ss$PROJ_YEARS))
    
    # base rate then by age
    diagn_rate_m <- sweep(diagn_rate[, , 1, , , drop = FALSE], 5, base_rate_m, "*")
    diagn_rate_f <- sweep(diagn_rate[, , 2, , , drop = FALSE], 5, base_rate_f, "*")
    diagn_rate[, , 1, , ] <- diagn_rate_m
    diagn_rate[, , 2, , ] <- diagn_rate_f
    # we multiply by the age rate ratio
    diagn_rate_am <- sweep(diagn_rate_m[, , , , , drop = FALSE], 2, agefn_m, "*")
    diagn_rate_af <- sweep(diagn_rate_f[, , , , , drop = FALSE], 2, agefn_f, "*")
    diagn_rate[, , 1, , ] <- diagn_rate_am
    diagn_rate[, , 2, , ] <- diagn_rate_af
    
    # testing rates due to the incidence of OI (in never tested and previously tested neg)
    mod_hts <- simmod_anc_t(fp, anc_true = TRUE)
    
    art_m <- colSums(attr(mod_hts, "artpop")[, , 1:8, 1, , , drop = FALSE], , 5)
    hiv_m <- colSums(attr(mod_hts, "hivpop")[, 1:8, 1, , drop = FALSE], , 3)
    # finds proportion of art over total hiv+ (art+hiv, or 0.95, which ever is lower)
    oi_m <- pmin(art_m / (art_m + hiv_m) * pr_oidx, 0.95)
    # na's become 0
    oi_m[is.na(oi_m)] <- 0
    #same for female
    art_f <- colSums(attr(mod_hts, "artpop")[, , 1:8, 2, , , drop = FALSE], , 5)
    hiv_f <- colSums(attr(mod_hts, "hivpop")[, 1:8, 2, , drop = FALSE], , 3)
    oi_f <- pmin(art_f / (art_f + hiv_f) * pr_oidx, 0.95)
    oi_f[is.na(oi_f)] <- 0
    
    # defines another array
    diagn_rate_oi <- array(1.0, c(fp$ss$hDS, fp$ss$hAG, fp$ss$NG, 4, fp$ss$PROJ_YEARS))
    #proportion of diagnosed assigned
    diagn_rate_oi[, , 1, , ] <- sweep(diagn_rate_oi[, , 1, , , drop = FALSE], 5, oi_m, "*")
    diagn_rate_oi[, , 2, , ] <- sweep(diagn_rate_oi[, , 2, , , drop = FALSE], 5, oi_f, "*")
    # we multiply by theta_fx (i.e., incidence of OI)
    diagn_rate_oi <- sweep(diagn_rate_oi[, , , , ], 1, theta_fx, "*")
    
    # testing among hiv+ never tested (unaware)
    diagn_rate[, , , 1, ] <- diagn_rate[, , , 1, ] * rr_plhiv + diagn_rate_oi[, , , 1, ]
    # testing among plhiv previously tested negative
    diagn_rate[, , , 2, ] <- sweep(diagn_rate[, , , 2, , drop = FALSE], 5, rate_rr_test, "*")
    diagn_rate[, , , 2, ] <- diagn_rate[, , , 2, ] * rr_plhiv + diagn_rate_oi[, , , 2, ]
    # re-testing among plhiv aware (not on ART)
    rate_dxunt <- stats::approx(knots_rr_dxunt, rr_dxunt, seq_len(fp$ss$PROJ_YEARS), rule = 2)$y
    diagn_rate_dxunt <- sweep(diagn_rate[, , , 3, , drop = FALSE], 5, rate_dxunt, "*")
    diagn_rate[, , , 3, ] <- diagn_rate_dxunt
    diagn_rate[, , , 3, ] <- diagn_rate[, , , 3, ] + diagn_rate_oi[, , , 3, ]
    # relative testing rate among plhiv already on art
    diagn_rate_dxart <- sweep(diagn_rate[, , , 4, , drop = FALSE], 5, rate_dxunt, "*")
    diagn_rate[, , , 4, ] <- diagn_rate_dxart * rr_dxart
    
    #' anc testing as a separate modality (i.e., with its own rate)
    
    # some women are not tested at clinicsbased on ART status, set between 0 to  0.75 proprtion of those on art tested
    anc_known_pos <- 0.05 + stats::plogis(theta[n_k3]) * (0.75 - 0.05)
    
    
    
    # we first use the annual spectrum asfr (only for ages 15:49)
    asfr <- hivdemo_proj$asfr
    
    group_indices_asfr <- rep(seq_along(fp$ss$h.ag.span[1:8]), fp$ss$h.ag.span[1:8])  # Expands to match asfr rows
    
    # Compute grouped means efficiently using rowsum()
    asfr <- rowsum(asfr, group_indices_asfr) / as.numeric(table(group_indices_asfr))
    
    # Append zero row
    asfr <- rbind(asfr, rep(0, ncol(asfr)))  # Ensuring correct dimensions
    
    #' *create array for hiv status and fertility
    #' * HIV age group (hAG: 1-9)
    #' * (1 = wlhiv not on art, 2 = wlhiv on art)
    rr_fert <- array(1.0, c(fp$ss$hAG, 2))
    # assign hiv+ art and non [age groups, year(time in variant)]
    rr_fert[c(1, 3:8), 1] <- hivdemo_proj$`fert_notart[hiv+]`[1:7, fp$ss$PROJ_YEARS]
    #linear mid point between 15 and 20 is for ag 17)
    rr_fert[c(2), 1] <- sum(hivdemo_proj$`fert_notart[hiv+]`[1:2, fp$ss$PROJ_YEARS]) / 2L
    #assign old age for >45
    rr_fert[c(9), 1] <- hivdemo_proj$`fert_notart[hiv+]`[7, fp$ss$PROJ_YEARS]
    
    # repeat for on art (lower fertility: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3606959/ makes sense according to this)
    rr_fert[c(1,3:8), 2] <- hivdemo_proj$`fert_onart[hiv+]`[1:7, fp$ss$PROJ_YEARS]
    # linear mid point between 15 and 20 is for ag 17(slightly off change? 17.5))
    rr_fert[2, 2] <- sum(hivdemo_proj$`fert_onart[hiv+]`[1:2, fp$ss$PROJ_YEARS]) / 2L
    # assign old age for >45
    rr_fert[9, 2] <- hivdemo_proj$`fert_onart[hiv+]`[7, fp$ss$PROJ_YEARS]
    
    # cd4count fertility rate effects
    rr_cd4 <- hivdemo_proj$cd4fert_rat
    
    # assign knot, nk3
    # num_tests_max_knot_year = 1 + stats::plogis(theta[n_k3])*4
    # anc testing starts in 2005 (generally)
    proj_end <- (fp$ss$PROJ_YEARS + fp$ss$proj_start - 1L)
    years_since_anc <- proj_end - 2002
    
    # find the number of years which have data, using testing data
    anc_indx_years <- colnames(pmtct$anc_test)[which(!is.na(pmtct$anc_test[2, ]))]
    anc_data_years <- as.integer(names(pmtct$anc_test)[!(is.na(pmtct$anc_test[2, ])|pmtct$anc_test[2, ] == 0)])
    anc_data_idx <- fp$ss$PROJ_YEARS - (proj_end - anc_data_years)
    
    # get ANC clients and subtract those who are known positive from the tests performed
    births <- pmtct$births 
    
    # Find valid columns where row 2 is not NA
    valid_cols <- which(!(is.na(pmtct$anc_test[2, ])|pmtct$anc_test[2, ] == 0))  # Faster than repeated indexing
    
    # anc tested and tested anc pos
    if(length(valid_cols)>1|length(valid_cols) == 0){
      anc_tested <- as.integer(colSums(pmtct$anc_test[c(2,5), valid_cols],na.rm = T))
      
    } else if(length(valid_cols) == 1){
      anc_tested <- as.integer(sum((pmtct$anc_test[c(2,5), valid_cols]),na.rm = T))
      
    }
    
    anc_pos <- as.integer(pmtct$anc_test[3, valid_cols])
    
    # Extract birth indices 
    births_idx <- colnames(pmtct$anc_test)[valid_cols]
    
    # Compute ANC coverage,  nb women tested at anc1 / births
    
    anc_cov <- anc_tested / births[births_idx]
    
    # Compute ANC positive proportion
    anc_pos_prop <- anc_pos / anc_tested
    
    anc_tests_prob = as.numeric(pmtct$receivepmtct)/as.numeric(pmtct$needpmtct) 
    anc_tests_prob[is.na(anc_tests_prob)] <- 0
    names(anc_tests_prob) <- fp$ss$proj_start:(fp$ss$proj_start + fp$ss$PROJ_YEARS - 1)
    
    #if (any(anc_tests_prob > 1)) { print("check spectrum's pmtct inputs, prob of being tested at anc >100%") }
    anc_tests_prob[anc_tests_prob > 1] <- 0.999 # or we could put a default of 95% (to discuss)
    
    
    
    # linear interpolation to remove any missing data
    if (is.null(anc_cov)) { stop("need to use births from the Spectrum pmtct module") }
    if(length(anc_cov)!=0){
      # no longer necissary since recive/need is better than linear interpolation
      # anc_cov2 <- stats::approx(anc_data_idx, anc_cov, anc_data_idx[1]:anc_data_idx[length(anc_data_idx)], rule = 2)$y
      # print(anc_cov2 == anc_cov)
      # # make sure interpolation is applied properly across years
      if(length(anc_data_idx) != length(anc_cov) ){stop("anccov and ancidx differ")}
      
      # apply known data
      anc_tests_prob[anc_data_idx] <- anc_cov
      
      # predict using artcov method past the date (for projection)
      anc_tests_prob[max(anc_data_idx):fp$ss$PROJ_YEARS] <- anc_tests_prob[max(anc_data_idx):fp$ss$PROJ_YEARS]
      
      
    }
    #earliest start is 2004
    anc_tests_prob[1:34] = 0
    
    # retesting due to parity and other mechanisms of retesting
    or_anc <-  1 + plogis(theta[n_k3+2]) * (10-1)
    
    
    # Compensate for under or over reporting of ANC tests, ranging from 0.2 to 0.98 times the number of expected tests
    ancover <-  plogis(theta[n_k3 + 1]) * 2
    
    #exponential adjustment
    ANC1_overcounting = exp(-1 * ancover * anc_tests_prob)
    # sets a empty(0) array up 
    #' * HIV age group (hAG: 1-9)
    #' * CD4 stage (hDS: 1-7)
    #' * HIV+/- and art+/- status ( 1 = HIV+ART-, 2 = HIV+ART+)
    
    #set 0 as that is baseline proportion
    anc_rate <- array(1.0, c(fp$ss$hAG,fp$ss$NG, 2, fp$ss$PROJ_YEARS))
    #HIV+ array
    anc_diagn_rate <- array(1.0, c(fp$ss$hDS, fp$ss$hAG ,fp$ss$NG, 5, fp$ss$PROJ_YEARS))
    
    # multiply by fertility for hiv-
    anc_rate[, 2, , ] <- sweep(anc_rate[, 2, , , drop = FALSE], c(1,4), asfr, "*")
    birth <- anc_rate
    # multiply by fertility for hiv+
    anc_diagn_rate[, , 2, , ] <- sweep(anc_diagn_rate[, , 2, , , drop = FALSE], c(2, 5), asfr, "*")
    # multiply by cd4
    anc_diagn_rate[, , 2, , ]  <- anc_diagn_rate[, , 2, , , drop = FALSE] * rr_cd4
    # multiply by hiv (not on art) and fertility
    anc_diagn_rate[, , 2, 1:4, ]  <- sweep(anc_diagn_rate[, , 2, 1:4, , drop = FALSE], c(2), rr_fert[, 1], "*")
    # multiply by art and fertility
    anc_diagn_rate[, , 2, 5, ]  <- sweep(anc_diagn_rate[, , 2, 5, , drop = FALSE], c(2), rr_fert[, 2], "*")
    
    positive_birth <- anc_diagn_rate
    
    anc_tests_prob = pmin(anc_tests_prob,1)
    # multiply by the probability that a pregnant women will be tested at anc (C_t)
    anc_rate[, 2, , ] <- sweep(anc_rate[, 2, , , drop = FALSE], 4, anc_tests_prob, "*")
    anc_diagn_rate[, , 2, , ] <- sweep(anc_diagn_rate[, , 2, , , drop = FALSE], 5, anc_tests_prob, "*")
    
    uncorrected_anc_rate = anc_rate
    # multiply by parity/overestimation
    #anc_rate[, 2, 1, ] <- sweep(anc_rate[, 2, 2,, drop = FALSE], 4, anc_first_test, "*")
    anc_rate[, 2, 2, ] <- sweep(anc_rate[, 2, 2,, drop = FALSE], 4, or_anc, "*")
    
    # multiply by over estimation of tests
    anc_rate[, 2, , ] <- sweep(anc_rate[, 2, , , drop = FALSE], 4, ANC1_overcounting, "*")
    
    # multiply by probability of being tested at anc if already on art
    anc_diagn_rate[, , 2, 5, ] <- sweep(anc_diagn_rate[, , 2, 2, , drop = FALSE], 5, anc_known_pos, "*")
    
    anc_rate[, 1, , ] <- 0
    anc_diagn_rate[, , 1, , ] <- 0
    fp$hts_rate <- hts_rate
    fp$diagn_rate <- diagn_rate
    fp$anc_rate <- anc_rate
    fp$anc_diagn_rate <- anc_diagn_rate
    
    subvar[["anc_tests_prob"]] <- anc_tests_prob
    subvar[["Q_ia"]] <- rr_fert
    subvar[["R_i"]] <- asfr
    subvar[["H_s"]] <- rr_cd4
    subvar[["birth"]] <- birth 
    subvar[["uncorrected ANC"]] <- uncorrected_anc_rate
    subvar[["positive_birth"]] <- positive_birth
    subvar[["adjustment"]] <- or_anc
    subvar[["anc_cov"]] <- anc_cov
    subvar[["diagn_rate_oi"]] <- diagn_rate_oi
    subvar[["ANC1_overcounting"]] <- ANC1_overcounting
    
    fp$subvar <- subvar
    
    return(fp)
  }
  

prepare_anc_likdat <- function(dat_evertest, dat_prg, fp) {
  
  # survey data on hiv testing histories
  dat_evertest$est <- ifelse(is.na(dat_evertest$est), NA, dat_evertest$est)
  dat_evertest <- dat_evertest[stats::complete.cases(dat_evertest$est), ]
  dat_evertest$l_est <- stats::qlogis(dat_evertest$est)
  dat_evertest$l_est_se <- dat_evertest$se/(dat_evertest$est * (1 - dat_evertest$est))
  dat_evertest$neff <- (dat_evertest$est * (1 - dat_evertest$est))/dat_evertest$se^2
  dat_evertest$neff <- ifelse(dat_evertest$est < 1e-05, dat_evertest$counts, dat_evertest$neff)
  dat_evertest$neff <- ifelse(dat_evertest$est > 0.999, dat_evertest$counts, dat_evertest$neff)
  dat_evertest$neff <- ifelse(dat_evertest$neff == Inf, dat_evertest$counts, dat_evertest$neff)
  dat_evertest$nsuccess <- round(dat_evertest$est * dat_evertest$neff, 0)
  dat_evertest$neff <- ifelse(round(dat_evertest$neff, 0) < 1, 1, round(dat_evertest$neff, 0))
  dat_evertest <- add_ss_indices(dat_evertest, fp$ss)
  
  # hts program data
  if (any(dat_prg$agegr != "15-99")) {
    stop(print("Error, HTS program data should be for the 15+ age group only other age-grouping not supported at the moment"))
  }
  dat_prg_pos <- dat_prg
  dat_prg_pos <- dat_prg_pos[stats::complete.cases(dat_prg_pos$totpos), ]
  dat_prg_pos_sex <- subset(dat_prg_pos, sex != "both")
  yr_sex <- unique(dat_prg_pos_sex$year)
  dat_prg_pos1 <- subset(dat_prg_pos, sex == "both" & !(year %in% yr_sex))
  dat_prg_pos2 <- subset(dat_prg_pos, sex != "both" & (year %in% yr_sex))
  dat_prg_pos <- rbind(dat_prg_pos1, dat_prg_pos2)
  dat_prg_pos$tot <- dat_prg_pos$totpos
  dat_prg_sex <- subset(dat_prg, sex != "both")
  yr_sex <- unique(dat_prg_sex$year)
  dat_prg1 <- subset(dat_prg, sex == "both" & !(year %in% yr_sex))
  dat_prg2 <- subset(dat_prg, sex != "both" & (year %in% yr_sex))
  dat_prg <- rbind(dat_prg1, dat_prg2)
  dat_prg <- dat_prg[stats::complete.cases(dat_prg$tot), ]
  
  # we separate into overall, vct, and anc
  # if both anc or vct is missing, we use overall
  idx <- which((is.na(dat_prg$vct) & is.na(dat_prg$anc)) &
                 (dat_prg$sex == "both" | dat_prg$sex == "female"))
  if (length(idx) == 0) {
    dat_all <- NULL
    dat_all_pos <- NULL
    dat_anc <- dat_prg
    dat_anc$tot <- dat_anc$anc
    dat_anc_pos <- dat_prg
    dat_anc_pos$tot <- dat_anc_pos$ancpos
    # dat_anc$sex <- "female"   # we need to override the default sex for anc since it is only for female
    # dat_anc_pos$sex <- "female"
    dat_vct <- dat_prg
    dat_vct$tot <- dat_vct$vct
    dat_vct_pos <- dat_prg
    dat_vct_pos$tot <- dat_vct_pos$vctpos
  } else {
    dat_all <- dat_prg[idx, ]
    dat_all_pos <- dat_all
    dat_all_pos$tot <- dat_all_pos$totpos
    dat_anc <- dat_prg[-idx, ]
    dat_anc$tot <- dat_anc$anc
    dat_anc_pos <- dat_prg[-idx, ]
    dat_anc_pos$tot <- dat_anc_pos$ancpos
    if(dim(dat_anc)[1]>0){
      dat_anc$sex <- "female"   # we need to override the default sex for anc since it is only for female
      dat_anc_pos$sex <- "female"
    }
    dat_vct <- dat_prg[-idx, ]
    dat_vct$tot <- dat_vct$vct
    dat_vct_pos <- dat_prg[-idx, ]
    dat_vct_pos$tot <- dat_vct_pos$vctpos    
  }
  
  # se within +/- 5% for total tests, and se within +/- 10% for positive tests
  if (dim(dat_prg)[1] > 0) {
    dat_prg$l_est_se <- ifelse(dat_prg$sex == "both", dat_prg$tot * 0.05, 
                               dat_prg$tot * 0.05 * sqrt(1 + 1 + 2 * 1))
    dat_prg$hivstatus <- "all"
    dat_prg <- add_ss_indices(dat_prg, fp$ss)
  } else { dat_prg <- NULL }
  if (dim(dat_prg_pos)[1] > 0) {
    dat_prg_pos$l_est_se <- ifelse(dat_prg_pos$sex == "both", dat_prg_pos$tot * 0.1, 
                                   dat_prg_pos$tot * 0.1 * sqrt(1 + 1 + 2 * 1))
    dat_prg_pos$hivstatus <- "positive"
    dat_prg_pos <- add_ss_indices(dat_prg_pos, fp$ss)
  } else { dat_prg_pos <- NULL }
  
  if (dim(dat_anc)[1] > 0) {
    dat_anc$l_est_se <- dat_anc$tot * 0.05
    dat_anc$hivstatus <- "all"
    dat_anc$modality <- "anc"
    dat_anc <- add_ss_indices(dat_anc, fp$ss)
  } else { dat_anc <- NULL }
  if (dim(dat_anc_pos)[1] > 0) {
    dat_anc_pos$l_est_se <- dat_anc_pos$tot * 0.1
    dat_anc_pos$hivstatus <- "positive"
    dat_anc_pos$modality <- "anc"
    dat_anc_pos <- add_ss_indices(dat_anc_pos, fp$ss)
  } else { dat_anc_pos <- NULL }
  
  if (dim(dat_vct)[1] > 0) {
    dat_vct$l_est_se <- ifelse(dat_vct$sex == "both", dat_vct$tot * 0.05, 
                               dat_vct$tot * 0.05 * sqrt(1 + 1 + 2 * 1))
    dat_vct$hivstatus <- "all"
    dat_vct$modality <- "vct"
    dat_vct <- add_ss_indices(dat_vct, fp$ss)
  } else { dat_vct <- NULL }
  if (dim(dat_vct_pos)[1] > 0) {
    dat_vct_pos$l_est_se <- ifelse(dat_vct_pos$sex == "both", dat_vct_pos$tot * 0.1, 
                                   dat_vct_pos$tot * 0.1 * sqrt(1 + 1 + 2 * 1))
    dat_vct_pos$hivstatus <- "positive"
    dat_vct_pos$modality <- "vct"
    dat_vct_pos <- add_ss_indices(dat_vct_pos, fp$ss)
  } else { dat_vct_pos <- NULL }
  
  if (!is.null(dat_all)) {
    dat_all$l_est_se <- ifelse(dat_all$sex == "both", dat_all$tot * 0.05, 
                               dat_all$tot * 0.05 * sqrt(1 + 1 + 2 * 1))
    dat_all$hivstatus <- "all"
    dat_all$modality <- "vct+anc"
    dat_all <- add_ss_indices(dat_all, fp$ss)
  } else { dat_all <- NULL }
  if (!is.null(dat_all_pos)) {
    dat_all_pos$l_est_se <- ifelse(dat_all_pos$sex == "both", dat_all_pos$tot * 0.1, 
                                   dat_all_pos$tot * 0.1 * sqrt(1 + 1 + 2 * 1))
    dat_all_pos$hivstatus <- "positive"
    dat_all_pos$modality <- "vct+anc"
    dat_all_pos <- add_ss_indices(dat_all_pos, fp$ss)
  } else { dat_all_pos <- NULL }
  
  return(list(evertest = dat_evertest, 
              hts = dat_prg, hts_pos = dat_prg_pos, 
              all = dat_all, all_pos = dat_all_pos,
              anc = dat_anc, anc_pos = dat_anc_pos,
              vct = dat_vct, vct_pos = dat_vct_pos))
}

#theta = opt$par
ll_hts_anc<- function (theta, fp, likdat, pmtct, hivdemo_proj, verboise = FALSE, anc = TRUE) {
    
    #set the ANC value to false if all data for ANC fitting is missing
    if (all(is.na(likdat$anc)) | all(is.na(likdat$anc_pos))) { anc = FALSE }
    
    fp <- create_anc_param(theta, fp, pmtct, hivdemo_proj)
    
    mod <- simmod_anc_t(fp, anc_true = T)
    
    if (anc == TRUE) {
      if (verboise == TRUE) {
        #print(apply(attr(mod, "anctests"), 4, sum))
      }
      val1 <- ll_evertest(mod, fp, dat = likdat$evertest)
      
      if (!is.null(likdat$vct)) {
        val2 <- ll_prgdat_anc(mod, fp, dat = likdat$vct)
      } else { val2 <- 0 }
      
      if (!is.null(likdat$vct_pos)) {
        val3 <- ll_prgdat_anc(mod, fp, dat = likdat$vct_pos)
      } else { val3 <- 0}
      
      if (!is.null(likdat$anc)) {
        val4 <- ll_prgdat_anc(mod, fp, dat = likdat$anc)
      } else { val4 <- 0 }
      
      if (!is.null(likdat$anc_pos)) {
        val5 <- ll_prgdat_anc(mod, fp, dat = likdat$anc_pos)
      } else { val5 <- 0 }
      
      if (!is.null(likdat$all)) {
        val6 <- ll_prgdat_anc(mod, fp, dat = likdat$all)
      } else { val6 <- 0 }
      
      if (!is.null(likdat$all_pos)) {
        val7 <- ll_prgdat_anc(mod, fp, dat = likdat$all_pos)
      } else { val7 <- 0 }
      
      val_prior <- lprior_anc(theta, mod, fp)
      
      
      
      val <- val1 + val2 + val3 + val4 + val5 + val6 + val7 + val_prior
      
      if (verboise == TRUE) {
        VERBOSE = data.frame(
          names = c("evertest","non-anc","non-ancpos","anc","ancpos","unstratified","unstratifiedpos","prior","full logliklyhood"),
          values = (c(val1, val2, val3, val4, val5, val6, val7,val_prior,val))
        )
        
      }
    } else if (anc == FALSE) {
      val1 <- ll_evertest(mod, fp, likdat$evertest)
      if (!is.null(likdat$hts)) {
        val2 <- first90::ll_prgdat(mod, fp, likdat$hts)
      } else { val2 <- 0 }
      if (!is.null(likdat$hts_pos)) {
        val3 <- first90::ll_prgdat(mod, fp, likdat$hts_pos)
      } else { val3 <- 0 }
      val_prior <- first90:::lprior_hts(theta, mod, fp)
      val <- val1 + val2 + val3 + val_prior
    }
    if(verboise){
      return(VERBOSE)
    }else{
      return(val)
    }
  }
  
ll_evertest = function (mod, fp, dat) 
  {
    mu <- evertest(mod, fp, df = dat,VERSION = "R")

    if (fp$projection_period == "calendar") {
      dat_last <- dat
      dat_last$yidx <- dat_last$yidx - 1L
      mu_last <- evertest(mod, fp, dat_last)
      mu <- 0.5 * (mu + mu_last)
    }
    if (any(is.na(mu)) || any(mu < 0) || any(mu > 1)) {
      llk <- log(0)
    } else {
      llk <- sum(stats::dbinom(x = dat$nsuccess, size = dat$neff,
                               prob = mu, log = TRUE))
    }
    return(llk)
}
  
ll_prgdat_anc <- function (mod, fp, dat,verboise = F) {
    
    dat <- dat[!is.na(dat$tot), ]
    
    if ((as.character(dat$hivstatus[1]) == "all" & as.character(dat$modality[1]) == "vct+anc")) {
      mu <- total_tests_anc(mod, df = add_ss_indices(dat, fp$ss))
      llk <- sum(stats::dnorm(x = dat$tot, mean = mu, sd = dat$l_est_se, log = TRUE))
      if(verboise){
        print(paste0(as.character(dat$modality[1])," ", as.character(dat$hivstatus[1])," : LL", round(llk,2), " mu "))
        print(round(mu,0))
        print(round( dat$tot,0))
      }
        
      
      #print(dat$modality)
    }
    if (as.character(dat$hivstatus[1]) == "positive" & as.character(dat$modality[1]) == "vct+anc") {
      mu <- total_tests_anc(mod, df = add_ss_indices(dat, fp$ss))
      llk <- sum(stats::dnorm(x = dat$tot, mean = mu, sd = dat$l_est_se, log = TRUE))
      #print(dat$modality)
      if(verboise){
        print(paste0(as.character(dat$modality[1])," ", as.character(dat$hivstatus[1])," : LL", round(llk,2), " mu "))
        print(round(mu,0))
        print(round( dat$tot,0))
      }
      
    }
    
    if ((as.character(dat$hivstatus[1]) == "all" & as.character(dat$modality[1]) == "vct")) {
      mu <- total_tests_anc(mod, df = add_ss_indices(dat, fp$ss))
      llk <- sum(stats::dnorm(x = dat$tot, mean = mu, sd = dat$l_est_se, log = TRUE))
      #print(dat$modality)
      if(verboise){
        print(paste0(as.character(dat$modality[1])," ", as.character(dat$hivstatus[1])," : LL", round(llk,2), " mu "))
        print(round(mu,0))
        print(round( dat$tot,0))
      }
      
    }
    if (as.character(dat$hivstatus[1]) == "positive" & as.character(dat$modality[1]) == "vct") {
      mu <- total_tests_anc(mod, df = add_ss_indices(dat, fp$ss))
      llk <- sum(stats::dnorm(x = dat$tot, mean = mu, sd = dat$l_est_se, log = TRUE))
      #print(dat$modality)
      if(verboise){
        print(paste0(as.character(dat$modality[1])," ", as.character(dat$hivstatus[1])," : LL", round(llk,2), " mu "))
        print(round(mu,0))
        print(round( dat$tot,0))
      }
      
    }
    
    if ((as.character(dat$hivstatus[1]) == "all" & as.character(dat$modality[1]) == "anc")) {
      mu <- total_tests_anc(mod, df = add_ss_indices(dat, fp$ss))
      llk <- sum(stats::dnorm(x = dat$tot, mean = mu, sd = dat$l_est_se, log = TRUE))
      #print(dat$modality)
      if(verboise){
        print(paste0(as.character(dat$modality[1])," ", as.character(dat$hivstatus[1])," : LL", round(llk,2), " mu "))
        print(round(mu,0))
        print(round( dat$tot,0))
      }
      
    }
    if (as.character(dat$hivstatus[1]) == "positive" & as.character(dat$modality[1]) == "anc") {
      mu <- total_tests_anc(mod, df = add_ss_indices(dat, fp$ss))
      llk <- sum(stats::dnorm(x = dat$tot, mean = mu, sd = dat$l_est_se, log = TRUE))
      #print(dat$modality)
      if(verboise){
        print(paste0(as.character(dat$modality[1])," ", as.character(dat$hivstatus[1])," : LL", round(llk,2), " mu "))
        print(round(mu,0))
        print(round( dat$tot,0))
      }
      
    }
    
    if (!(as.character(dat$hivstatus[1]) %in% c("all", "positive"))) {
      print("Error - HIV status is incorrect")
      return()
    }
    if (!(as.character(dat$modality[1]) %in% c("anc", "vct", "vct+anc"))) {
      print("Error - modality is incorrect, are you modeling ANC data? if no try first90::ll_hts")
      return()
    }
    if(any(is.nan(mu))){ print(paste0("NANs in", as.character(dat$modality[1])))}
       
    return(llk)
  }
  
  
  lprior_anc <- function(theta, mod, fp) {
    ## Penalty to smooth testing rates among females aged 15-24 (reference group)
    ## We calculate penalty for RR of males on the log(rate) scale (and use same SD as for females)
    ## -- UPDATE HERE --
    ## * Extend knots by 1 year to current year
    max_knot_year <- if (length(theta) == 44) {
      2019
    } else if (length(theta) == 46) {
      2020
    } else if (length(theta) == 48) {
      2021
    } else if (length(theta) == 50) {
      2022
    } else if (length(theta) == 52) {
      2023
    } else if (length(theta) == 54) {
      2024
    } else {
      stop("Unexpected length of parameter vector.")
    }
    ## -- UPDATE ABOVE --
    
    knots <- c(1995, 2000:max_knot_year) - fp$ss$proj_start + 1L
    knots_rr_dxunt <- c(2010:max_knot_year) - fp$ss$proj_start + 1L
    n_k1 <- length(knots) - 1
    n_k2 <- n_k1 + length(knots_rr_dxunt)
    n_k3 <- n_k2 + 12
    
    penalty_f <- theta[1:n_k1][-1] - theta[1:n_k1][-n_k1]
    penalty_rr_dxunt <- theta[(n_k1 + 1):n_k2][-1] - theta[(n_k1 + 1):n_k2][-length(theta[(n_k1 + 1):n_k2])]
    penalty_rr_m <- log(stats::plogis(theta[n_k2 + 2]) * 10) - log(stats::plogis(theta[n_k2 + 1]) * 10)
    penalty_rr_test <- theta[n_k2 + 4] - theta[n_k2 + 3]
    
    lprior <-
      ## Prior for first baseline rate for females # exp(log(0.001) + 1.96*0.25)
      stats::dnorm(x = theta[1], mean = log(0.005), sd = 0.25, log = TRUE) +
      ## Relative testing among PLHIV diagnosed, untreated. 1.50 (95%CI: 0.14-6.00) # stats::plogis(stats::qlogis(1.5/8) + c(-1, 1) * 1.96*1.31)*8
      stats::dnorm(x = theta[n_k1 + 1], mean = stats::qlogis(1.5 / 8), sd = 1.31, log = TRUE) +
      ## Prior for male RR 5 (95%CI: 0.19-9.8) # 0.05 + stats::plogis(stats::qlogis((5.05-0.05)/(10-0.05)) + c(-1, 1) * 1.96*2) * (10-0.05)
      sum(stats::dnorm(x = theta[n_k2 + 1], mean = stats::qlogis(5 / 10), sd = 2, log = TRUE)) + 
      ## Relative increase among previously tested. 1.93 (95%CI: 1.08-5.00) # 0.95 + stats::plogis(stats::qlogis((1.93 - 0.95)/7.05) + stats::qnorm(0.975)*1.084)*7.05
      stats::dnorm(x = theta[n_k2 + 3], mean = stats::qlogis((1.93 - 0.95) / 7.05), sd = 1.084, log = TRUE) +
      ## Relative factor for re-testing among PLHIV unaware. 1.00 (95%CI: 0.10-1.90) # 0.05 + stats::plogis(stats::qlogis(0.95 / 1.90) + c(-1, 1) * 1.96 * 1.85) * (1.95 - 0.05)
      stats::dnorm(x = theta[n_k2 + 5], mean = stats::qlogis(0.95 / 1.90), sd = 1.85, log = TRUE) +
      ## Relative testing among PLHIV already on ART (95%CI: 0.01-0.90) # stats::plogis(stats::qlogis(0.25) + c(-1, 1) * 1.96*1.68)
      stats::dnorm(x = theta[n_k2 + 6], mean = stats::qlogis(0.25), sd = 1.68, log = TRUE) +
      ## Prior for age (95% CI is 0.14-5.0)# 0.1 + invlogit(logit(0.9/5.9) + c(-1, 1) * 1.96*1.5) * (6 - 0.1)
      sum(stats::dnorm(x = theta[c((n_k2 + 7):(n_k2 + 10))], mean = stats::qlogis(0.9 / 5.9), sd = 1.685, log = TRUE)) +
      ## RR OI diagnosed for HIV relative to ART coverage 1.0 (0.3-1.7) # 0.25 + stats::plogis(stats::qlogis(0.5) + c(-1, 1) * 1.96*1.75) * (1.75 - 0.25)
      stats::dnorm(x = theta[n_k2 + 11], mean = stats::qlogis(0.5), sd = 1.75, log = TRUE) +
      ## Probability of re-testing at ANC when WLHIV are on ART #  0.29 (0.09 to 0.57) 0.05 + stats::plogis(stats::qlogis((0.25 - 0.05) / (0.75 - 0.05)) + c(-1, 1) * 1.96 * 1) * (0.75-0.05)
      stats::dnorm(x = theta[n_k3], mean = stats::qlogis((0.25-0.05) / (0.75-0.05)), sd = 1, log = TRUE) +
      ## RR for under/over estimation of ANC tests, 0.85 ( 0.25- 1.75) # exp(-1 * stats::plogis(stats::qlogis((0.4)/1.5) + c(-1,0, 1) * 1.96 * 2) * 2 * 1)
      stats::dnorm(x = theta[n_k3 + 1], mean = stats::qlogis((0.4/2 )),  sd = 2, log = TRUE) +
      ## RR retesting at ANC , 1.5 ( 1.1 - 2.5) # 1 + stats::plogis(stats::qlogis((2.5-1)/(10-1)) + c(-1, 1) * 1.96 * 1.5) * (10-1)
      stats::dnorm(x = theta[n_k3 + 2], mean = stats::qlogis((1.5)/(10 - 1)),  sd = 1.5, log = TRUE) 
    
    
    prior_sd_f <- 0.205 # exp(log(0.25) + c(-1, 1)*1.96*0.205); previous of 0.35 when double-counting
    prior_sd_rr_m <- 0.25 # exp(log(1) + c(-1, 1) * 1.96 * 0.20)
    prior_sd_rr_dxunt <- 0.25
    prior_sd_rr_test <- 0.25
    
    hyperprior <-
      ## Penalty for 1st degree difference (female baseline rate)
      sum(stats::dnorm(x = penalty_f, mean = 0, sd = prior_sd_f, log = TRUE)) +
      ## Penalty for 1st degree difference (male RR)
      sum(stats::dnorm(x = penalty_rr_m, mean = 0, sd = prior_sd_rr_m, log = TRUE)) +
      ## Penalty for 1st degree difference (RR_dxunt)
      sum(stats::dnorm(x = penalty_rr_dxunt, mean = 0, sd = prior_sd_rr_dxunt, log = TRUE)) +
      ## Penalty for 1st degree difference (RR_test)
      sum(stats::dnorm(x = penalty_rr_test, mean = 0, sd = prior_sd_rr_test, log = TRUE))
    
    return(lprior + hyperprior) 
  }
  
  total_tests_anc <- function(mod, df) {
    
    all_diag <- NA
    length(all_diag) <- length(df$haidx)
    
    names(attributes(mod))
    
    if (df$modality[1] == "vct") {
      for (i in seq_along(df$haidx)) {
        hivdx <- switch(df$hivstatus[i], all = 1:6, negative = 1:2, positive = 3:6)
        haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
        sidx <- if (df$sidx[i] == 0) { 1:2
        } else { df$sidx[i] }
        all_diag[i] <- colSums(attr(mod, "vcttests")[haidx, sidx, hivdx, df$yidx[i], drop = FALSE], , 3)
        
      }
    }
    
    if (df$modality[1] == "anc") {
      for (i in seq_along(df$haidx)) {
        hivdx <- switch(df$hivstatus[i], all = 1:6, negative = 1:2, positive = 3:6)
        haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
        sidx <- if (df$sidx[i] == 0) { 1:2
        } else { df$sidx[i] }
        all_diag[i] <- colSums(attr(mod, "anctests")[haidx, sidx, hivdx, df$yidx[i], drop = FALSE], , 3)
        
      }
    }
    
    if (df$modality[1] == "vct+anc") {
      for (i in seq_along(df$haidx)) {
        hivdx <- switch(df$hivstatus[i], all = 1:6, negative = 1:2, positive = 3:6)
        haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
        sidx <- if (df$sidx[i] == 0) { 1:2
        } else { df$sidx[i] }
        all_diag[i] <- colSums(attr(mod, "anctests")[haidx, sidx, hivdx, df$yidx[i], drop = FALSE], , 3)+colSums(attr(mod, "vcttests")[haidx, sidx, hivdx, df$yidx[i], drop = FALSE], , 3)
        
      }
    }  
    return(all_diag)
  }
  
  
  
simul.run.anc <- function(samp, fp, pmtct, hivdemo_proj, progress = NULL) {
    # Define the proper categories over which CI are required
    end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
    diagno <- expand.grid(year = 2000:end_date,
                          outcome = 'aware',
                          agegr = c("15-24", "25-34", '35-49', '15-49', '15+'),
                          sex = c("both", "female", "male"),
                          hivstatus = 'positive')
    unaware <- expand.grid(year = 2000:end_date,
                          outcome = 'unaware',
                          agegr = c("15-24", "25-34", '35-49', '15-49', '15+'),
                          sex = c("both", "female", "male"),
                          hivstatus = 'positive')
    out_art <- expand.grid(year = 2000:end_date,
                           outcome = "artcov",
                           agegr = c("15-24", "25-34", '35-49', '15-49', '15+'),
                           sex = c("both", "female", "male"),
                           hivstatus = "positive")
    out_evertest <- expand.grid(year = 2000:end_date,
                                outcome = "evertest",
                                agegr = c("15-24", "25-34", '35-49', '15-49', '15+'),
                                sex = c("both", "female", "male"),
                                hivstatus = c("all", "negative", "positive"))
    out_hiv <- expand.grid(year = 2000:end_date,
                           outcome = "hivtests",
                           agegrp = "15+",
                           sex = c("both", "female", "male"),
                           hivstatus = 'all',
                           modality = "hiv")
    out_hiv_pos <- expand.grid(year = 2000:end_date,
                               outcome = "hivtests",
                               agegrp = "15+",
                               sex = c("both", "female", "male"),
                               hivstatus = 'positive',
                               modality = "hiv")
    
    out_vct <- expand.grid(year = 2000:end_date,
                           outcome = "vcttests",
                           agegrp = "15+",
                           sex = c("both", "female", "male"),
                           hivstatus = 'all',
                           modality = "vct")
    out_vct_pos <- expand.grid(year = 2000:end_date,
                               outcome = "vcttests",
                               agegrp = "15+",
                               sex = c("both", "female", "male"),
                               hivstatus = 'positive',
                               modality = "vct")
    out_anc <- expand.grid(year = 2000:end_date,
                           outcome = "anctests",
                           agegrp = "15+",
                           sex = c("both", "female", "male"),
                           hivstatus = 'all',
                           modality = "anc")
    out_anc_pos <- expand.grid(year = 2000:end_date,
                               outcome = "anctests",
                               agegrp = "15+",
                               sex = c("both", "female", "male"),
                               hivstatus = 'positive',
                               modality = "anc")
    
    diagno_ss <- add_ss_indices(diagno, fp$ss)
    evertest_ss <- add_ss_indices(out_evertest, fp$ss)
    unaware_ss <- add_ss_indices(unaware,fp$ss)
    
    vct_ss <- add_ss_indices(out_vct, fp$ss)
    vctpos_ss <- add_ss_indices(out_vct_pos, fp$ss)
    vct_ss$hivstatus <- as.character(vct_ss$hivstatus)
    vctpos_ss$hivstatus <- as.character(vctpos_ss$hivstatus)
    
    anc_ss <- add_ss_indices(out_anc, fp$ss)
    ancpos_ss <- add_ss_indices(out_anc_pos, fp$ss)
    anc_ss$hivstatus <- as.character(anc_ss$hivstatus)
    ancpos_ss$hivstatus <- as.character(ancpos_ss$hivstatus)
    # Create parameters (proper scale, etc.), and simulate model
    for (i in 1:nrow(samp)) {
      fp <- create_anc_param(samp[i, ], fp,pmtct = pmtct, hivdemo_proj = hivdemo_proj)
      mod <- simmod_anc_t(fp)
      diagno[, ncol(diagno) + 1] <- diagnosed(mod, fp, diagno_ss)
      out_evertest[, (ncol(out_evertest) + 1)] <- evertest(mod, fp, evertest_ss)
      unaware[, ncol(unaware) + 1] <- Undiagnosed(mod, fp, unaware_ss)
      
      out_anc[, (ncol(out_anc) + 1)] <- total_tests_anc(mod, anc_ss)
      out_anc_pos[, (ncol(out_anc_pos) + 1)] <- total_tests_anc(mod, ancpos_ss)
      
      # vct and anc are mutually exclusive, here we add vct+anc to get total tests
      out_vct_only <- total_tests_anc(mod, vct_ss)
      out_vctpos_only <- total_tests_anc(mod, vctpos_ss)
      out_hiv[, (ncol(out_hiv) + 1)] <- out_vct_only + out_anc[, ncol(out_anc)]
      out_hiv_pos[, (ncol(out_hiv_pos) + 1)] <- out_vctpos_only + out_anc_pos[, ncol(out_anc_pos)]
      
      out_vct[, (ncol(out_vct) + 1)] <- out_vct_only 
      out_vct_pos[, (ncol(out_vct_pos) + 1)] <- out_vctpos_only 
      
      
      if (!is.null(progress)) { progress() }
    }
    
    # Store results in a list of data frames
    val <- list(diagnoses = diagno, evertest = out_evertest,
                anctests = out_anc, hivtests = out_hiv, anctestspos = out_anc_pos, 
                hivtestspos = out_hiv_pos, param = samp,vcttests = out_vct,
                vcttestspos = out_vct_pos,unaware = unaware)
    return(val)
  }
  
  
simul.test.anc <- function(opt, fp, pmtct, hivdemo_proj, sim = 3000, likdat = NULL, samp = NULL) {
    
    if( is.null(samp)){
      samp <- first90::simul.sample(opt$hessian, opt$par, fp, sim = sim, 
                                    likdat = likdat)
    }
    val <- simul.run.anc(samp, fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
    
    return(list(simul = val, samp = samp))
  }
  
getci <- function(df) {
    start_col <- which(names(df) == "modality") + 1
    if (length(start_col) == 0) { start_col <- 6 }
    n_col <- ncol(df)
    # Function for the confidence interval of the estimates
    ci <- apply(X = df[, (start_col:n_col)], MARGIN = 1,
                FUN = stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE)
    lower <- ci[1, ]
    upper <- ci[2, ]
    df_ci <- data.frame(df[c(1:(start_col - 1))], lower, upper)
    return(df_ci)
  }
  

