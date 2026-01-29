time_to_dx_cr <- function(fp, year = c(2000:2023), age = "15-24", sex = "male", test_ever = "never", dt = 0.20, version = "C") {
  
  if (version == "C") {
    
    val <- time_to_dx_cr_cpp(fp, year = year, age = age, sex = sex, test_ever = test_ever, dt = dt)
  
    } 
  if (version == "R") {
    n_year <- length(year)
    val <- data.frame(year = year,
                      age = rep(paste(age, collapse = ", "), n_year),
                      sex = rep(paste(sex, collapse = ", "), n_year),
                      time_dx_avg = NA,
                      time_dx_med = NA,
                      prb6mo = NA,
                      prb1yr = NA,
                      prb2yr = NA,
                      prb5yr = NA,
                      prb500cd4 = NA,
                      prb350cd4 = NA,
                      propdx = NA)
    
    if (age == "15-24") { ind_age <- 1 }
    if (age == "25-34") { ind_age <- 4 }
    if (age == "35-49") { ind_age <- 6 }
    if (age == "50-99") { ind_age <- 9 }
    
    if (sex == "male") { ind_sex <- 1
    } else {
      ind_sex = 2 }
    
    if (test_ever == "never") { 
      ind_th <- 1 
      ind_anc_th <- 1
    }
    if (test_ever == "ever")  { 
      ind_th <- 2 
      ind_anc_th <- 1
    }
    if (test_ever == "ever_anc")  { 
      ind_th <- 2 
      ind_anc_th <- 2
    }

    # CD4 progression rates
    cd4_prg <- fp$cd4_prog[, ind_age, ind_sex]
    
    # HIV-related death rates
    mort_rate <- fp$cd4_mort[, ind_age, ind_sex]
    
    # Death rates by CD4 category
    mort_rate15 <- fp$cd4_mort[, 1, ind_sex]
    mort_rate25 <- fp$cd4_mort[, 4, ind_sex]
    mort_rate35 <- fp$cd4_mort[, 6, ind_sex]
    mort_rate50 <- fp$cd4_mort[, 9, ind_sex]
    
    # For probability of being Dx before certain time (expressed in dt)
    tind6mo <- round(0.5 / dt, 0)
    tind1yr <- round(1 / dt, 0)
    tind2yr <- round(2 / dt, 0)
    tind5yr <- round(5 / dt, 0)
    
    for (n in 1:n_year) {
      # which group are we calculating this for
      ind_yr <- year[n] - fp$ss$proj_start + 1
      
      # what is the average age of incident HIV infection
      avg_age15 <- 14 + weighted.mean(1:10, w = fp$infections[1:10, ind_sex, ind_yr])
      avg_age25 <- 14 + weighted.mean(11:20, w = fp$infections[11:20, ind_sex, ind_yr])
      avg_age35 <- 14 + weighted.mean(21:35, w = fp$infections[21:35, ind_sex, ind_yr])
      
      # Testing rates by CD4 category
      if(sex == "male"){
        test_rate15 <- fp$diagn_rate[, 1, ind_sex, ind_th, ind_yr]
        test_rate25 <- fp$diagn_rate[, 4, ind_sex, ind_th, ind_yr]
        test_rate35 <- fp$diagn_rate[, 6, ind_sex, ind_th, ind_yr]
        test_rate50 <- fp$diagn_rate[, 9, ind_sex, ind_th, ind_yr]
      }
      
      if(sex == "female"){
        test_rate15 <- fp$diagn_rate[, 1, ind_sex, ind_th, ind_yr]+fp$anc_diagn_rate[, 1, ind_sex, ind_anc_th, ind_yr]
        test_rate25 <- fp$diagn_rate[, 4, ind_sex, ind_th, ind_yr]+fp$anc_diagn_rate[, 4, ind_sex, ind_anc_th, ind_yr]
        test_rate35 <- fp$diagn_rate[, 6, ind_sex, ind_th, ind_yr]+fp$anc_diagn_rate[, 6, ind_sex, ind_anc_th, ind_yr]
        test_rate50 <- fp$diagn_rate[, 9, ind_sex, ind_th, ind_yr]+fp$anc_diagn_rate[, 9, ind_sex, ind_anc_th, ind_yr]
      }
      
      # Creating vector for time and testing rates or death rates
      time_int <- seq(0, 50, by = dt)[-1]
      vec_l <- length(time_int)
      
      if (age == '15-24') {
        ind15 <- round((25 - avg_age15) / dt, 0)
        ind25 <- ind15 + 10 / dt
        ind35 <- ind25 + 15 / dt
        test_rate_t <- rbind( do.call("rbind", replicate(ind15, test_rate15, simplify = FALSE)),
                              do.call("rbind", replicate((ind25 - ind15), test_rate25, simplify = FALSE)),
                              do.call("rbind", replicate((ind35 - ind25), test_rate35, simplify = FALSE)),
                              do.call("rbind", replicate((vec_l - ind35), test_rate50, simplify = FALSE))) 
        mort_rate_t <- rbind( do.call("rbind", replicate(ind15, mort_rate15, simplify = FALSE)),
                              do.call("rbind", replicate((ind25 - ind15), mort_rate25, simplify = FALSE)),
                              do.call("rbind", replicate((ind35 - ind25), mort_rate35, simplify = FALSE)),
                              do.call("rbind", replicate((vec_l - ind35), mort_rate50, simplify = FALSE))) 
      }
      if (age == '25-34') {
        ind25 <- round((35 - avg_age25) / dt, 0)
        ind35 <- ind25 + 15 / dt
        test_rate_t <- rbind( do.call("rbind", replicate(ind25, test_rate25, simplify = FALSE)),
                              do.call("rbind", replicate((ind35 - ind25), test_rate35, simplify = FALSE)),
                              do.call("rbind", replicate((vec_l - ind35), test_rate50, simplify = FALSE))) 
        mort_rate_t <- rbind( do.call("rbind", replicate(ind25, mort_rate25, simplify = FALSE)),
                              do.call("rbind", replicate((ind35 - ind25), mort_rate35, simplify = FALSE)),
                              do.call("rbind", replicate((vec_l - ind35), mort_rate50, simplify = FALSE))) 
      }
      if (age == '35-49') {
        ind35 <- round((50 - avg_age35) / dt, 0)
        test_rate_t <- rbind( do.call("rbind", replicate(ind35, test_rate35, simplify = FALSE)),
                              do.call("rbind", replicate((vec_l - ind35), test_rate50, simplify = FALSE))) 
        mort_rate_t <- rbind( do.call("rbind", replicate(ind35, mort_rate35, simplify = FALSE)),
                              do.call("rbind", replicate((vec_l - ind35), mort_rate50, simplify = FALSE))) 
      }
      if (age == '50-99') {
        test_rate_t <- do.call("rbind", replicate(vec_l, test_rate50, simplify = FALSE))
        mort_rate_t <- do.call("rbind", replicate(vec_l, mort_rate50, simplify = FALSE))
      }
      
      # Initialize life table
      N <- 1000
      X <- array(data = 0, dim = c(vec_l, 7))
      X[1, ] <- N * fp$cd4_initdist[, ind_age, ind_sex]
      Xt <- X[1, ]
      # For mean or median time between infection and diagnosis or HIV-death
      for (i in 2:vec_l) {
        X[i, 1] <- Xt[1] + (-(cd4_prg[1] + mort_rate_t[i, 1] + test_rate_t[i, 1]) * Xt[1]) * dt
        X[i, 2] <- Xt[2] + (cd4_prg[1] * Xt[1] - (cd4_prg[2] + mort_rate_t[i, 2] + test_rate_t[i, 2]) * Xt[2]) * dt
        X[i, 3] <- Xt[3] + (cd4_prg[2] * Xt[2] - (cd4_prg[3] + mort_rate_t[i, 3] + test_rate_t[i, 3]) * Xt[3]) * dt
        X[i, 4] <- Xt[4] + (cd4_prg[3] * Xt[3] - (cd4_prg[4] + mort_rate_t[i, 4] + test_rate_t[i, 4]) * Xt[4]) * dt
        X[i, 5] <- Xt[5] + (cd4_prg[4] * Xt[4] - (cd4_prg[5] + mort_rate_t[i, 5] + test_rate_t[i, 5]) * Xt[5]) * dt
        X[i, 6] <- Xt[6] + (cd4_prg[5] * Xt[5] - (cd4_prg[6] + mort_rate_t[i, 6] + test_rate_t[i, 6]) * Xt[6]) * dt
        X[i, 7] <- Xt[7] + (cd4_prg[6] * Xt[6] - (mort_rate_t[i, 7] + test_rate_t[i, 7]) * Xt[7]) * dt
        Xt <- X[i,]
        
        nb_dx[i, ] <- (test_rate_t[i, ] * X[i - 1, ]) * dt
        nb_death[i] <- sum(mort_rate_t[i, ] * X[i - 1, ]) * dt
        person_time[i - 1] <- (sum(Xt) + (sum(X[i - 1, ]) - sum(X[i, ])) / 2) * dt
      }
      
      # Life expectancy at HIV infection = mean time between HIV infection and testing or HIV death
      val$time_dx_avg[val$year == year[n]] <- sum(person_time) / N
      val$time_dx_med[val$year == year[n]] <- time_int[which.min(abs(rowSums(X) - (0.5 * N)))]
      
      # Probability of being diagnosed (among those not dying)
      val$prb6mo[val$year == year[n]] <- sum(nb_dx[1:tind6mo, ]) / N
      val$prb1yr[val$year == year[n]] <- sum(nb_dx[1:tind1yr, ]) / N
      val$prb2yr[val$year == year[n]] <- sum(nb_dx[1:tind2yr, ]) / N
      val$prb5yr[val$year == year[n]] <- sum(nb_dx[1:tind5yr, ]) / N
      val$prb500cd4[val$year == year[n]] <- sum(nb_dx[ , 1]) / N
      val$prb350cd4[val$year == year[n]] <- sum(nb_dx[ , 1:2]) / N
      val$propdx[val$year == year[n]] <- sum(nb_dx) / N
      
    }
  
}

return(val)
}


denom_lifetable <- function(mod, fp, df_ind) {
  
  ever <- never <- ever_anc <- numeric(length(df_ind$haidx))
  
  for(i in seq_along(df_ind$haidx)) {
    
    haidx <- df_ind$haidx[i] + 1:df_ind$hagspan[i] - 1
    sidx <- if(df_ind$sidx[i] == 0) 1:2 else df_ind$sidx[i]
    
    paidx <- fp$ss$agfirst.idx[df_ind$haidx[i]] + 1:sum(fp$ss$h.ag.span[haidx]) - 1
    
    if(df_ind$hvidx[i] %in% c(0, 1)){ # testing among HIV-
      
      pop_hivn_ha <- apply(mod[paidx, sidx, fp$ss$hivn.idx, df_ind$yidx[i],
                               drop = FALSE],
                           2:3, fastmatch::ctapply, fp$ss$ag.idx[paidx], sum)
      
      tested_ha <- attr(mod, "testnegpop")[haidx, sidx, fp$ss$hivn.idx,1,
                                           df_ind$yidx[i], drop = FALSE]
      
      tested_anc_ha <- attr(mod, "testnegpop")[haidx, sidx, fp$ss$hivn.idx,2,
                                           df_ind$yidx[i], drop = FALSE]
      
      
      never[i] <- sum(c(pop_hivn_ha) - c(tested_ha) - c(tested_anc_ha))  # among untested HIV- population
      ever[i] <- sum(tested_ha)    # among previously tested HIV- population
      ever_anc[i] <- sum(tested_anc_ha)    # among previously tested HIV- population
    
      }
    val <- data.frame(df_ind[, !(colnames(df_ind) %in% c("agestart","aidx","agspan","haidx","hagspan","sidx","hvidx","yidx"))], never, ever,ever_anc)
    
  }
  return(val)
}

pool_time_dx_cr <- function(mod, fp, year = c(2000:2020), age = c("15-24", "25-34", "35-49", "50-99"),
                            sex = c("male", "female")) {
  
  n_year <- length(year)
  val <- data.frame(year = year, age = rep(paste(age, collapse = "+"), n_year),
                    sex = rep(paste(sex, collapse = "+"), n_year),
                    time_dx_avg = NA,
                    time_dx_med = NA,
                    prb6mo = NA,
                    prb1yr = NA,
                    prb2yr = NA,
                    prb5yr = NA,
                    prb500cd4 = NA,
                    prb350cd4 = NA,
                    propdx = NA)
  
  for (n in 1:n_year) {
    ind_yr <- year[n] - fp$ss$proj_start + 1
    
    time_mat <- expand.grid(year = year[n], age = c("15-24", "25-34", "35-49", "50-99"),
                            sex = c("male", "female"), test_ever = c("never", "ever"))
    time_mat <- rbind(time_mat,expand.grid(year = year[n], age = c("15-24", "25-34", "35-49", "50-99"),
                            sex = c("female"), test_ever = c("ever_anc")))
    
    
    time_mat$time_avg <- NA
    time_mat$time_med <- NA
    time_mat$prb6 <- NA
    time_mat$prb1 <- NA
    time_mat$prb2 <- NA
    time_mat$prb5 <- NA
    time_mat$prb500 <- NA
    time_mat$prb350 <- NA
    time_mat$prop <- NA
    time_mat$w <- NA
    index_age <- data.frame(x = c(1, 11, 21, 36),
                            y = c(10, 20, 35, 66))
    
    info_need <- expand.grid(year = year[n],
                             agegr = c("15-24", "25-34", "35-49", "50-99"),
                             sex = c("female", "male"),
                             hivstatus = "negative")
    denom <- denom_lifetable(mod, fp, df_ind = add_ss_indices(info_need, fp$ss))
    denom$w_n <- denom$never / (denom$never + denom$ever + denom$ever_anc)
    denom$w_e <- denom$ever / (denom$never + denom$ever + denom$ever_anc)
    denom$w_ae <- denom$ever_anc / (denom$never + denom$ever + denom$ever_anc)
    
    for (i in 1:20) {
      if (time_mat$age[i] == "15-24") { index_agei <- index_age[1, ] }
      if (time_mat$age[i] == "25-34") { index_agei <- index_age[2, ] }
      if (time_mat$age[i] == "35-49") { index_agei <- index_age[3, ] }
      if (time_mat$age[i] == "50-99") { index_agei <- index_age[4, ] }
      if (time_mat$sex[i] == "male") { index_sexi <- 1 }
      if (time_mat$sex[i] == "female") { index_sexi <- 2 }
      
      
      result_i <- time_to_dx_cr(fp, year = year[n], age = as.character(time_mat$age[i]),
                                sex = as.character(time_mat$sex[i]), 
                                test_ever = as.character(time_mat$test_ever[i]))
      
      time_mat$time_avg[i] <- result_i$time_dx_avg
      time_mat$time_med[i] <- result_i$time_dx_med
      time_mat$prb6[i] <- result_i$prb6mo
      time_mat$prb1[i] <- result_i$prb1yr
      time_mat$prb2[i] <- result_i$prb2yr
      time_mat$prb5[i] <- result_i$prb5yr
      time_mat$prb500[i] <- result_i$prb500cd4
      time_mat$prb350[i] <- result_i$prb350cd4
      time_mat$prop[i] <- result_i$propdx
      
      
      if (time_mat$test_ever[i] == 'never') {
        time_mat$w[i] <- sum(fp$infections[index_agei$x:index_agei$y, index_sexi, ind_yr]) *
          denom$w_n[denom$agegr == time_mat$age[i] & denom$sex == time_mat$sex[i]]
      } else if(time_mat$test_ever[i] == 'ever'){
        time_mat$w[i] <- sum(fp$infections[index_agei$x:index_agei$y, index_sexi, ind_yr]) *
          denom$w_e[denom$agegr == time_mat$age[i] & denom$sex == time_mat$sex[i]]
      } else if(time_mat$test_ever[i] == 'ever_anc'){
        time_mat$w[i] <- sum(fp$infections[index_agei$x:index_agei$y, index_sexi, ind_yr]) *
          denom$w_ae[denom$agegr == time_mat$age[i] & denom$sex == time_mat$sex[i]]
      } 
      }
    time_mat_sel <- time_mat[time_mat$age %in% age & time_mat$sex %in% sex, ]
    val$time_dx_avg[val$year == year[n]] <- weighted.mean(time_mat_sel$time_avg, w = time_mat_sel$w)
    val$time_dx_med[val$year == year[n]] <- weighted.mean(time_mat_sel$time_med, w = time_mat_sel$w)
    val$prb6mo[val$year == year[n]] <- weighted.mean(time_mat_sel$prb6, w = time_mat_sel$w)    
    val$prb1yr[val$year == year[n]] <- weighted.mean(time_mat_sel$prb1, w = time_mat_sel$w)    
    val$prb2yr[val$year == year[n]] <- weighted.mean(time_mat_sel$prb2, w = time_mat_sel$w)  
    val$prb5yr[val$year == year[n]] <- weighted.mean(time_mat_sel$prb5, w = time_mat_sel$w)
    val$prb500cd4[val$year == year[n]] <- weighted.mean(time_mat_sel$prb500, w = time_mat_sel$w)  
    val$prb350cd4[val$year == year[n]] <- weighted.mean(time_mat_sel$prb350, w = time_mat_sel$w)  
    val$propdx[val$year == year[n]] <- weighted.mean(time_mat_sel$prop, w = time_mat_sel$w)
  }
  return(val)
}

simul_time_dx_cr <- function(samp, mod, fp, pmtct, hivdemo_proj, year = c(2018:2023),
                             age = "15-24", sex = "male", test_ever = "never", dt = 0.20) {
  
  print("Careful, evertested among women does not count retests amoung anc")
  n_year <- length(year)
  val <- data.frame(year = year, age = rep(paste(age, collapse = "+"), n_year),
                    sex = rep(paste(sex, collapse = "+"), n_year),
                    median = NA, lci = NA, uci = NA,
                    
                    prb1yr = NA, prb1yr_lci = NA, prb1yr_uci = NA,
                    prb2yr = NA, prb2yr_lci = NA, prb2yr_uci = NA,
                    prb5yr = NA, prb5yr_lci = NA, prb5yr_uci = NA,
                    prb500cd4 = NA, prb500cd4_lci = NA, prb500cd4_uci = NA,
                    prb350cd4 = NA, prb350cd4_lci = NA, prb350cd4_uci = NA,
                    
                    prb1yrt = NA, prb1yrt_lci = NA, prb1yrt_uci = NA,
                    prb2yrt = NA, prb2yrt_lci = NA, prb2yrt_uci = NA,
                    prb5yrt = NA, prb5yrt_lci = NA, prb5yrt_uci = NA,
                    prb500cd4t = NA, prb500cd4t_lci = NA, prb500cd4t_uci = NA,
                    prb350cd4t = NA, prb350cd4t_lci = NA, prb350cd4t_uci = NA,
                    
                    prb1yrd = NA, prb1yrd_lci = NA, prb1yrd_uci = NA,
                    prb2yrd = NA, prb2yrd_lci = NA, prb2yrd_uci = NA,
                    prb5yrd = NA, prb5yrd_lci = NA, prb5yrd_uci = NA,
                    prb500cd4d = NA, prb500cd4d_lci = NA, prb500cd4d_uci = NA,
                    prb350cd4d = NA, prb350cd4d_lci = NA, prb350cd4d_uci = NA,
                    
                    propdx = NA, propdx_lci = NA, propdx_uci = NA)
  
  for (n in 1:n_year) {
    avg_time_dx <- NULL
    
    avg_prb1yr <- NULL
    avg_prb2yr <- NULL
    avg_prb5yr <- NULL
    avg_prb500cd4 <- NULL
    avg_prb350cd4 <- NULL
    
    avg_prb1yrt <- NULL
    avg_prb2yrt <- NULL
    avg_prb5yrt <- NULL
    avg_prb500cd4t <- NULL
    avg_prb350cd4t <- NULL
    
    avg_prb1yrd <- NULL
    avg_prb2yrd <- NULL
    avg_prb5yrd <- NULL
    avg_prb500cd4d <- NULL
    avg_prb350cd4d <- NULL
    
    avg_propdx <- NULL
    
    # Create parameters (proper scale, etc.), and simulate model
    for (i in 1:nrow(samp)) {
      fp <- create_anc_param(samp[i,], fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
      
      if(sex == "male"){
        result_i <- time_to_dx_cr_cpp(fp, year = year[n], age = age, sex = sex, test_ever = test_ever, dt = dt)
        print(paste0("iter",i))
      }
      if(sex == "female"){
        result_i <- time_to_dx_cr(fp, year = year[n], age = age, sex = sex, test_ever = test_ever, dt = dt)
        print(paste0("iter",i))
      }
      
       avg_time_dx <- c(avg_time_dx, result_i$time_dx_avg)
      
      avg_prb1yr <- c(avg_prb1yr, result_i$prb1yr)
      avg_prb2yr <- c(avg_prb2yr, result_i$prb2yr)
      avg_prb5yr <- c(avg_prb5yr, result_i$prb5yr)
      avg_prb500cd4 <- c(avg_prb500cd4, result_i$prb500cd4)
      avg_prb350cd4 <- c(avg_prb350cd4, result_i$prb350cd4)
      
      avg_prb1yrt <- c(avg_prb1yrt, result_i$prb1yrt)
      avg_prb2yrt <- c(avg_prb2yrt, result_i$prb2yrt)
      avg_prb5yrt <- c(avg_prb5yrt, result_i$prb5yrt)
      avg_prb500cd4t <- c(avg_prb500cd4t, result_i$prb500cd4t)
      avg_prb350cd4t <- c(avg_prb350cd4t, result_i$prb350cd4t)
      
      avg_prb1yrd <- c(avg_prb1yrd, result_i$prb1yrd)
      avg_prb2yrd <- c(avg_prb2yrd, result_i$prb2yrd)
      avg_prb5yrd <- c(avg_prb5yrd, result_i$prb5yrd)
      avg_prb500cd4d <- c(avg_prb500cd4d, result_i$prb500cd4d)
      avg_prb350cd4d <- c(avg_prb350cd4d, result_i$prb350cd4d)
      
      avg_propdx <- c(avg_propdx, result_i$propdx)
      
    }
    
    uncertainty_time <- quantile(avg_time_dx, probs = c(0.5, 0.025, 0.975))
    val$median[val$year == year[n]] <- uncertainty_time[1]
    val$lci[val$year == year[n]] <- uncertainty_time[2]
    val$uci[val$year == year[n]] <- uncertainty_time[3]
    uncertainty_prb1yr <- quantile(avg_prb1yr, probs = c(0.5, 0.025, 0.975))
    val$prb1yr[val$year == year[n]] <- uncertainty_prb1yr[1]
    val$prb1yr_lci[val$year == year[n]] <- uncertainty_prb1yr[2]
    val$prb1yr_uci[val$year == year[n]] <- uncertainty_prb1yr[3]
    uncertainty_prb2yr <- quantile(avg_prb2yr, probs = c(0.5, 0.025, 0.975))
    val$prb2yr[val$year == year[n]] <- uncertainty_prb2yr[1]
    val$prb2yr_lci[val$year == year[n]] <- uncertainty_prb2yr[2]
    val$prb2yr_uci[val$year == year[n]] <- uncertainty_prb2yr[3]
    uncertainty_prb5yr <- quantile(avg_prb5yr, probs = c(0.5, 0.025, 0.975))
    val$prb5yr[val$year == year[n]] <- uncertainty_prb5yr[1]
    val$prb5yr_lci[val$year == year[n]] <- uncertainty_prb5yr[2]
    val$prb5yr_uci[val$year == year[n]] <- uncertainty_prb5yr[3]
    uncertainty_prb500cd4 <- quantile(avg_prb500cd4, probs = c(0.5, 0.025, 0.975))
    val$prb500cd4[val$year == year[n]] <- uncertainty_prb500cd4[1]
    val$prb500cd4_lci[val$year == year[n]] <- uncertainty_prb500cd4[2]
    val$prb500cd4_uci[val$year == year[n]] <- uncertainty_prb500cd4[3]
    uncertainty_prb350cd4 <- quantile(avg_prb350cd4, probs = c(0.5, 0.025, 0.975))
    val$prb350cd4[val$year == year[n]] <- uncertainty_prb350cd4[1]
    val$prb350cd4_lci[val$year == year[n]] <- uncertainty_prb350cd4[2]
    val$prb350cd4_uci[val$year == year[n]] <- uncertainty_prb350cd4[3]   
    
    uncertainty_prb1yrt <- quantile(avg_prb1yrt, probs = c(0.5, 0.025, 0.975))
    val$prb1yrt[val$year == year[n]] <- uncertainty_prb1yrt[1]
    val$prb1yrt_lci[val$year == year[n]] <- uncertainty_prb1yrt[2]
    val$prb1yrt_uci[val$year == year[n]] <- uncertainty_prb1yrt[3]
    uncertainty_prb2yrt <- quantile(avg_prb2yrt, probs = c(0.5, 0.025, 0.975))
    val$prb2yrt[val$year == year[n]] <- uncertainty_prb2yrt[1]
    val$prb2yrt_lci[val$year == year[n]] <- uncertainty_prb2yrt[2]
    val$prb2yrt_uci[val$year == year[n]] <- uncertainty_prb2yrt[3]
    uncertainty_prb5yrt <- quantile(avg_prb5yrt, probs = c(0.5, 0.025, 0.975))
    val$prb5yrt[val$year == year[n]] <- uncertainty_prb5yrt[1]
    val$prb5yrt_lci[val$year == year[n]] <- uncertainty_prb5yrt[2]
    val$prb5yrt_uci[val$year == year[n]] <- uncertainty_prb5yrt[3]
    uncertainty_prb500cd4t <- quantile(avg_prb500cd4t, probs = c(0.5, 0.025, 0.975))
    val$prb500cd4t[val$year == year[n]] <- uncertainty_prb500cd4t[1]
    val$prb500cd4t_lci[val$year == year[n]] <- uncertainty_prb500cd4t[2]
    val$prb500cd4t_uci[val$year == year[n]] <- uncertainty_prb500cd4t[3]
    uncertainty_prb350cd4t <- quantile(avg_prb350cd4t, probs = c(0.5, 0.025, 0.975))
    val$prb350cd4t[val$year == year[n]] <- uncertainty_prb350cd4t[1]
    val$prb350cd4t_lci[val$year == year[n]] <- uncertainty_prb350cd4t[2]
    val$prb350cd4t_uci[val$year == year[n]] <- uncertainty_prb350cd4t[3]
    
    uncertainty_prb1yrd <- quantile(avg_prb1yrd, probs = c(0.5, 0.025, 0.975))
    val$prb1yrd[val$year == year[n]] <- uncertainty_prb1yrd[1]
    val$prb1yrd_lci[val$year == year[n]] <- uncertainty_prb1yrd[2]
    val$prb1yrd_uci[val$year == year[n]] <- uncertainty_prb1yrd[3]
    uncertainty_prb2yrd <- quantile(avg_prb2yrd, probs = c(0.5, 0.025, 0.975))
    val$prb2yrd[val$year == year[n]] <- uncertainty_prb2yrd[1]
    val$prb2yrd_lci[val$year == year[n]] <- uncertainty_prb2yrd[2]
    val$prb2yrd_uci[val$year == year[n]] <- uncertainty_prb2yrd[3]
    uncertainty_prb5yrd <- quantile(avg_prb5yrd, probs = c(0.5, 0.025, 0.975))
    val$prb5yrd[val$year == year[n]] <- uncertainty_prb5yrd[1]
    val$prb5yrd_lci[val$year == year[n]] <- uncertainty_prb5yrd[2]
    val$prb5yrd_uci[val$year == year[n]] <- uncertainty_prb5yrd[3]
    uncertainty_prb500cd4d <- quantile(avg_prb500cd4d, probs = c(0.5, 0.025, 0.975))
    val$prb500cd4d[val$year == year[n]] <- uncertainty_prb500cd4d[1]
    val$prb500cd4d_lci[val$year == year[n]] <- uncertainty_prb500cd4d[2]
    val$prb500cd4d_uci[val$year == year[n]] <- uncertainty_prb500cd4d[3]
    uncertainty_prb350cd4d <- quantile(avg_prb350cd4d, probs = c(0.5, 0.025, 0.975))
    val$prb350cd4d[val$year == year[n]] <- uncertainty_prb350cd4d[1]
    val$prb350cd4d_lci[val$year == year[n]] <- uncertainty_prb350cd4d[2]
    val$prb350cd4d_uci[val$year == year[n]] <- uncertainty_prb350cd4d[3]
    
    uncertainty_propdx <- quantile(avg_propdx, probs = c(0.5, 0.025, 0.975))
    val$propdx[val$year == year[n]] <- uncertainty_propdx[1]
    val$propdx_lci[val$year == year[n]] <- uncertainty_propdx[2]
    val$propdx_uci[val$year == year[n]] <- uncertainty_propdx[3]
    
  }
  
  return(val)
  
}
simul_pool_time_dx_agg_prev_counter <- function(samp, mod, fp, year = c(2000:2018),
                                                age = c("15-24", "25-34", "35-49", "50-99"),
                                                sex = c("male", "female"),
                                                comp_risk = TRUE, std = TRUE,pmtct,hivdemo_proj,counter_years,
                                                parallel = T) {
  
  
  n_year <- length(year)
  index_w <- year - fp$ss$proj_start + 1
  
  if(all(age %in% c("15-24", "25-34", "35-49", "50-99")) & length(unique(age)) == 4) {
    index_age <- 1:66 
    age_out_time_ind <- "15-99" }
  if(all(age %in% c("15-24"))) {
    index_age <- 1:10 
    age_out_time_ind <- "15-24" }
  if(all(age %in% c("25-34"))) {
    index_age <- 11:20 
    age_out_time_ind <- "25-34" }
  if(all(age %in% c("35-49"))) {
    index_age <- 21:35 
    age_out_time_ind <- "35-49" }
  if(all(age %in% c("50-99"))) {
    index_age <- 36:66 
    age_out_time_ind <- "50-99" }      
  
  if(all(sex %in% c("male", "female")) & length(unique(sex)) == 2) {
    index_sex <- 1:2 
    sex_out_time_ind <- "both" }
  if(all(sex %in% c("male"))) {
    index_sex <- 1 
    sex_out_time_ind <- "male" }
  if(all(sex %in% c("female"))) {
    index_sex <- 2 
    sex_out_time_ind <- "female" }
  
  poids <- data.frame(year = year, w = apply(attr(mod, "infections")[index_age, index_sex, index_w, drop = FALSE], 3, FUN = sum))
  print(counter_years)
  if(parallel == T){
  # Create parameters (proper scale, etc.), and simulate model
  fun_samp_par <- function(i, samp, mod, fp, poids, age_out_time_ind, 
                           sex_out_time_ind, year, pmtct,hivdemo_proj,counter_years) {
    #creates
    sampi = samp[i,]
    convert = (length(sampi)-4)/2
    start = as.numeric(counter_years[2])
    # set as the year of recovery(after 2000)-1(to allow for indexed replacment)  
    end = as.numeric(ifelse(!is.na(counter_years[3]),(as.numeric(counter_years[3])-30-1),30))
    
    start_female = start - 30
    
    start_dx = convert + (start - 40)
    
    end_dx = start_dx + (end - start_female) 
    
    # change later to work for all years
    sampi[start_female:min(end,convert)] = sampi[start_female]
    sampi[start_dx:min(end_dx,convert+convert-10)] = sampi[start_dx]
    #sampi2
    
    fp_local <- create_anc_param(sampi, fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
    #fp_local2 <- create_anc_param(sampi2, fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
    
    
    mod_local <- simmod_anc_t(fp_local)
    
    result_i <- pool_time_dx_cr(mod_local, fp_local, year = year, age = age, sex = sex)
    result_i$sampi <- i
    rownames(result_i) <- NULL
    return(result_i)
  }
  num_cores = parallel::detectCores()-5
  cl <- parallel::makeCluster(num_cores)  # Create a cluster with available cores
  parallel::clusterExport(cl, 
                          varlist = c("samp", "mod", "fp", "poids", "age_out_time_ind", 
                                      "sex_out_time_ind", "year", "fun_samp_par",
                                      "hivdemo_proj","pmtct","counter_years"),
                          envir = environment())
  parallel::clusterEvalQ(cl, {
    library(Rcpp)
    source(paste0(here::here("anc testing"),"/1.0 simmod.R"))
    source(paste0(here::here("anc testing"),"/0.6 time dx functions.R"))
    source(paste0(here::here("anc testing"),"/src/time_dx_anc_cpp_cr.R"))
    })
  val_lst <- parallel::parLapply(
    cl, 
    X = 1:nrow(samp), 
    fun = function(i) {
      fun_samp_par(
        i, samp = samp, mod = mod, fp = fp, poids = poids, 
        age_out_time_ind = age_out_time_ind, 
        sex_out_time_ind = sex_out_time_ind, 
        year = year,hivdemo_proj = hivdemo_proj,pmtct = pmtct,
        counter_years = counter_years
      )
    }
  )
  parallel::stopCluster(cl)  # Stop the cluster
  
  }else{
    # Create parameters (proper scale, etc.), and simulate model
    source(paste0(here::here("anc testing"),"/1.0 simmod.R"))
    source(paste0(here::here("anc testing"),"/0.6 time dx functions.R"))
    
    fun_samp_par <- function(i, samp, mod, fp, poids, age_out_time_ind, sex_out_time_ind, year, pmtct, hivdemo_proj, counter_years) {
      # source("f90 cpp.R")
      
      fp_local <- create_anc_param(samp[i,], fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
      
      fp_local$hts_rate[,,,idx] <- fp_local$hts_rate[,,,start]
      
      fp_local$subvar$diagn_rate_oi[,,,4,] <- 0
      
      fp_local$diagn_rate[,,,,idx] <- (fp_local$diagn_rate[,,,,start] - fp_local$subvar$diagn_rate_oi[,,,,start])
      fp_local$diagn_rate[,,,,idx] <- (fp_local$diagn_rate[,,,,idx] + fp_local$subvar$diagn_rate_oi[,,,,idx])
      
      mod <- simmod_anc_t(fp_local)
      
      result_i <- pool_time_dx_cr(mod, fp_i, year = year, age = age, sex = sex)
      result_i$sampi <- i
      rownames(result_i) <- NULL
      result_i
    }
    
    # Run the function sequentially for each row in samp
    val_lst <- lapply(1:nrow(samp), function(i) {
      fun_samp_par(
        i, samp = samp, mod = mod, fp = fp, poids = poids, 
        age_out_time_ind = age_out_time_ind, 
        sex_out_time_ind = sex_out_time_ind, 
        year = year, hivdemo_proj = hivdemo_proj, 
        pmtct = pmtct, counter_years = counter_years
      )
    })
    
  }
  
  
  
  val_ <- data.table::rbindlist(val_lst)
  val <- merge(val_, poids, by = "year")
  val$age <-paste(age, collapse = "+")
  val$sex <- paste(sex, collapse = "+")
  
  return(val)
  
}
simul_pool_time_dx_agg_prev <- function(samp, mod, fp, year = c(2000:2018),
                                        age = c("15-24", "25-34", "35-49", "50-99"),
                                        sex = c("male", "female"),
                                        comp_risk = TRUE, std = F,pmtct,hivdemo_proj,
                                        parallel = T) {
  
  n_year <- length(year)
  index_w <- year - fp$ss$proj_start + 1
  
  if(all(age %in% c("15-24", "25-34", "35-49", "50-99")) & length(unique(age)) == 4) {
    index_age <- 1:66 
    age_out_time_ind <- "15-99" }
  if(all(age %in% c("15-24"))) {
    index_age <- 1:10 
    age_out_time_ind <- "15-24" }
  if(all(age %in% c("25-34"))) {
    index_age <- 11:20 
    age_out_time_ind <- "25-34" }
  if(all(age %in% c("35-49"))) {
    index_age <- 21:35 
    age_out_time_ind <- "35-49" }
  if(all(age %in% c("50-99"))) {
    index_age <- 36:66 
    age_out_time_ind <- "50-99" }      
  
  if(all(sex %in% c("male", "female")) & length(unique(sex)) == 2) {
    index_sex <- 1:2 
    sex_out_time_ind <- "both" }
  if(all(sex %in% c("male"))) {
    index_sex <- 1 
    sex_out_time_ind <- "male" }
  if(all(sex %in% c("female"))) {
    index_sex <- 2 
    sex_out_time_ind <- "female" 
    #print("female")
  }
  
  poids <- data.frame(year = year, w = apply(attr(mod, "infections")[index_age, index_sex, index_w, drop = FALSE], 3, FUN = sum))
  
  if(parallel == T){
    # Create parameters (proper scale, etc.), and simulate model
    fun_samp_par <- function(i, samp, mod, fp, poids, age_out_time_ind, sex_out_time_ind, year, pmtct,hivdemo_proj) {
      source(paste0(here::here("anc testing"),"/1.0 simmod.R"))
      source(paste0(here::here("anc testing"),"/0.6 time dx functions.R"))
      
      fp <- create_anc_param(samp[i,], fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
      mod <- simmod_anc_t(fp)
      
      result_i <- pool_time_dx_cr(mod, fp, year = year, age = age, sex = sex)
      result_i$sampi <- i
      rownames(result_i) <- NULL
      return(result_i)
    }
    num_cores = parallel::detectCores()-5
    cl <- parallel::makeCluster(num_cores)  # Create a cluster with available cores
    parallel::clusterExport(cl, 
                            varlist = c("samp", "mod", "fp", "poids", "age_out_time_ind", 
                                        "sex_out_time_ind", "year", "fun_samp_par","hivdemo_proj","pmtct"),
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      library(Rcpp)
      source(paste0(here::here("anc testing"),"/1.0 simmod.R"))
      source(paste0(here::here("anc testing"),"/0.6 time dx functions.R"))
      source(paste0(here::here("anc testing"),"/src/time_dx_anc_cpp_cr.R"))
      })
    val_lst <- parallel::parLapply(
      cl, 
      X = 1:nrow(samp), 
      fun = function(i) {
        fun_samp_par(
          i, samp = samp, mod = mod, fp = fp, poids = poids, 
          age_out_time_ind = age_out_time_ind, 
          sex_out_time_ind = sex_out_time_ind, 
          year = year,hivdemo_proj = hivdemo_proj,pmtct = pmtct
        )
      }
    )
    parallel::stopCluster(cl)  # Stop the cluster
  }else{
    # Create parameters (proper scale, etc.), and simulate model
    
    fun_samp_par <- function(i, samp, mod, fp, poids, age_out_time_ind, sex_out_time_ind, year, pmtct, hivdemo_proj) {
      
      fp_local <- create_anc_param(samp[i,], fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
      mod_local <- simmod_anc_t(fp_local)
      
      result_i <- pool_time_dx_cr(mod_local, fp_local, year = year, age = age, sex = sex)
      result_i$sampi <- i
      rownames(result_i) <- NULL
      return(result_i)
    }
    
    # Run the function sequentially for each row in samp
    val_lst <- lapply(1:nrow(samp), function(i) {
      fun_samp_par(
        i, samp = samp, mod = mod, fp = fp, poids = poids, 
        age_out_time_ind = age_out_time_ind, 
        sex_out_time_ind = sex_out_time_ind, 
        year = year, hivdemo_proj = hivdemo_proj, 
        pmtct = pmtct
      )
    })
    
    
  }
  
  
  val_ <- data.table::rbindlist(val_lst)
  val <- merge(val_, poids, by = "year")
  val$age <-paste(age, collapse = "+")
  val$sex <- paste(sex, collapse = "+")
  
  return(val)
  
}

# Agg_simul_pool_time_dx_prev_2 <- function(lst,
#                                           year = c(2015:2023),
#                                           age = "15-24+25-34+35-49+50-99",
#                                           sex = "male+female",
#                                           region = "SSA") {
#   
#   
#   
#   n_cnt <- length(lst)
#   n_year <- length(year)
#   
#   val <- data.frame(year = year,
#                     age = rep(paste(age), n_year),
#                     sex = rep(paste(sex), n_year),
#                     time_dx = NA, time_dx_lci = NA, time_dx_uci = NA,
#                     prb1yr = NA, prb1yr_lci = NA, prb1yr_uci = NA,
#                     prb2yr = NA, prb2yr_lci = NA, prb2yr_uci = NA,
#                     prb5yr = NA, prb5yr_lci = NA, prb5yr_uci = NA,
#                     prb500cd4 = NA, prb500cd4_lci = NA, prb500cd4_uci = NA,
#                     prb350cd4 = NA, prb350cd4_lci = NA, prb350cd4_uci = NA,
#                     propdx = NA, propdx_lci = NA, propdx_uci = NA )
#   
#   val$id <- paste(val$year, val$age, val$sex, sep = "_")
#   
#   # Create data frames
#   res <- NULL
#   
#   for (i in 1:n_cnt){
#     lst_i <- lst[[i]]
#     res_i <- as.data.frame(lst_i$out_simul_tdx_all)
#     res_i$cnt <- names(lst)[i]
#     res <- rbind(res, res_i)
#   }
#   
#   res$id <- paste(res$year, res$age, res$sex, sep = "_")
#   
#   # we calculate the nb diagnosed within each period
#   #print("break")
#   
#   # with cr
#   
#   res$nb1yr <- res$prb1yr * res$w
#   res$nb2yr <- res$prb2yr * res$w
#   res$nb5yr <- res$prb5yr * res$w
#   res$nb500cd4 <- res$prb500cd4 * res$w
#   res$nb350cd4 <- res$prb350cd4 * res$w
#   
#   res$nbdx <- res$propdx * res$w
#   #ensure it only runs for needed years
#   res = res[res$year %in% year,]
#   
#   #print("break")
#   # we pool here (cr)
#   n_id <- unique(res$id)
#   
#   for (j in 1:length(n_id)) {
#     
#     pol <- res[res$id == n_id[j], ]
#     #print(j)
#     #print(pol$w[pol$sampi == 5])
#     #print(pol$time_dx_med[pol$sampi == 5])
#     
#     for (s in 1:length(unique(pol$sampi))) {
#       # we calculate the pooled mean time in the region
#       pol$time_dx_reg[pol$sampi == s] <-  weighted.mean(pol$time_dx_med[pol$sampi == s], w = pol$w[pol$sampi == s])
#       
#       # we calculate prb numerators
#       pol$nb1yr_reg[pol$sampi == s] <- sum(pol$nb1yr[pol$sampi == s])
#       pol$nb2yr_reg[pol$sampi == s] <- sum(pol$nb2yr[pol$sampi == s])
#       pol$nb5yr_reg[pol$sampi == s] <- sum(pol$nb5yr[pol$sampi == s])
#       pol$nb500cd4_reg[pol$sampi == s] <- sum(pol$nb500cd4[pol$sampi == s])
#       pol$nb350cd4_reg[pol$sampi == s] <- sum(pol$nb350cd4[pol$sampi == s])
#       
#       
#       pol$nbdx_reg[pol$sampi == s] <- sum(pol$nbdx[pol$sampi == s])
#       
#       # we calculate prb denominators
#       pol$newinf_reg[pol$sampi == s] <- sum(pol$w[pol$sampi == s])
#       
#     }
#     
#     # we calculate the proportions of diagnosed within each period in the region, central tendencies, and uncertainty on all samps
#     
#     uncertainty <- quantile(pol$time_dx_reg, probs = c(0.5, 0.025, 0.975))
#     val$time_dx[val$id == n_id[j]] <- uncertainty[1]
#     val$time_dx_lci[val$id == n_id[j]] <- uncertainty[2]
#     val$time_dx_uci[val$id == n_id[j]] <- uncertainty[3]
#     #print(val$id)
#     #print(n_id[j])
#     
#     pol$prb1yr_reg <- pol$nb1yr_reg / pol$newinf_reg
#     uncertainty <- quantile(pol$prb1yr_reg, probs = c(0.5, 0.025, 0.975))
#     val$prb1yr[val$id == n_id[j]] <- uncertainty[1]
#     val$prb1yr_lci[val$id == n_id[j]] <- uncertainty[2]
#     val$prb1yr_uci[val$id == n_id[j]] <- uncertainty[3]
#     
#     pol$prb2yr_reg <- pol$nb2yr_reg / pol$newinf_reg
#     uncertainty <- quantile(pol$prb2yr_reg, probs = c(0.5, 0.025, 0.975))
#     val$prb2yr[val$id == n_id[j]] <- uncertainty[1]
#     val$prb2yr_lci[val$id == n_id[j]] <- uncertainty[2]
#     val$prb2yr_uci[val$id == n_id[j]] <- uncertainty[3]
#     
#     pol$prb5yr_reg <- pol$nb5yr_reg / pol$newinf_reg
#     uncertainty <- quantile(pol$prb5yr_reg, probs = c(0.5, 0.025, 0.975))
#     val$prb5yr[val$id == n_id[j]] <- uncertainty[1]
#     val$prb5yr_lci[val$id == n_id[j]] <- uncertainty[2]
#     val$prb5yr_uci[val$id == n_id[j]] <- uncertainty[3]
#     
#     pol$prb500cd4_reg <- pol$nb500cd4_reg / pol$newinf_reg
#     uncertainty <- quantile(pol$prb500cd4_reg, probs = c(0.5, 0.025, 0.975))
#     val$prb500cd4[val$id == n_id[j]] <- uncertainty[1]
#     val$prb500cd4_lci[val$id == n_id[j]] <- uncertainty[2]
#     val$prb500cd4_uci[val$id == n_id[j]] <- uncertainty[3]
#     
#     pol$prb350cd4_reg <- pol$nb350cd4_reg / pol$newinf_reg
#     uncertainty <- quantile(pol$prb350cd4_reg, probs = c(0.5, 0.025, 0.975))
#     val$prb350cd4[val$id == n_id[j]] <- uncertainty[1]
#     val$prb350cd4_lci[val$id == n_id[j]] <- uncertainty[2]
#     val$prb350cd4_uci[val$id == n_id[j]] <- uncertainty[3]
#     
#     
#     pol$propdx_reg <- pol$nbdx_reg / pol$newinf_reg
#     uncertainty <- quantile(pol$propdx_reg, probs = c(0.5, 0.025, 0.975))
#     val$propdx[val$id == n_id[j]] <- uncertainty[1]
#     val$propdx_lci[val$id == n_id[j]] <- uncertainty[2]
#     val$propdx_uci[val$id == n_id[j]] <- uncertainty[3]
#     
#   }
#   
#   return(val)
# }

#much much much faster 40min -> 10 seconds
Agg_simul_pool_time_dx_prev_2 <- function(lst,
                                          year = c(2015:2023),
                                          age = "15-24+25-34+35-49+50-99",
                                          sex = "male+female"){
  
require(data.table,quietly = T)

n_year <- length(year)
year_1 = year
# 1. Initialize output
val <- data.frame(
  year = year,
  age = rep(paste(age), n_year),
  sex = rep(paste(sex), n_year),
  time_dx = NA, time_dx_lci = NA, time_dx_uci = NA,
  prb1yr = NA, prb1yr_lci = NA, prb1yr_uci = NA,
  prb2yr = NA, prb2yr_lci = NA, prb2yr_uci = NA,
  prb5yr = NA, prb5yr_lci = NA, prb5yr_uci = NA,
  prb500cd4 = NA, prb500cd4_lci = NA, prb500cd4_uci = NA,
  prb350cd4 = NA, prb350cd4_lci = NA, prb350cd4_uci = NA,
  propdx = NA, propdx_lci = NA, propdx_uci = NA
)
val$id <- paste(val$year, val$age, val$sex, sep = "_")

# 2. Merge all simulations
res <- rbindlist(lapply(seq_along(lst), function(i) {
  dat <- as.data.frame(lst[[i]]$out_simul_tdx_all)
  dat$cnt <- names(lst)[i]
  dat
}))

setDT(res)  # make into data.table

# 3. Preprocessing
res[, id := paste(year, age, sex, sep = "_")]

res = res[res$year %in% year_1,]

# 4. Precompute quantities
res[, `:=`(
  nb1yr = prb1yr * w,
  nb2yr = prb2yr * w,
  nb5yr = prb5yr * w,
  nb500cd4 = prb500cd4 * w,
  nb350cd4 = prb350cd4 * w,
  nbdx = propdx * w
)]

# 5. Group by id and sampi
pooled <- res[, .(
  time_dx_reg = weighted.mean(time_dx_med, w),
  nb1yr_reg = sum(nb1yr),
  nb2yr_reg = sum(nb2yr),
  nb5yr_reg = sum(nb5yr),
  nb500cd4_reg = sum(nb500cd4),
  nb350cd4_reg = sum(nb350cd4),
  nbdx_reg = sum(nbdx),
  newinf_reg = sum(w)
), by = .(id, sampi)]

# 6. Now for each id calculate the uncertainties
ids <- unique(pooled$id)

for (j in ids) {
  temp <- pooled[id == j]
  
  # Time to diagnosis
  uncertainty <- quantile(temp$time_dx_reg, probs = c(0.5, 0.025, 0.975))
  val[val$id == j, c("time_dx", "time_dx_lci", "time_dx_uci")] <- uncertainty
  
  # Diagnosis probabilities
  temp[, prb1yr_reg := nb1yr_reg / newinf_reg]
  uncertainty <- quantile(temp$prb1yr_reg, probs = c(0.5, 0.025, 0.975))
  val[val$id == j, c("prb1yr", "prb1yr_lci", "prb1yr_uci")] <- uncertainty
  
  temp[, prb2yr_reg := nb2yr_reg / newinf_reg]
  uncertainty <- quantile(temp$prb2yr_reg, probs = c(0.5, 0.025, 0.975))
  val[val$id == j, c("prb2yr", "prb2yr_lci", "prb2yr_uci")] <- uncertainty
  
  temp[, prb5yr_reg := nb5yr_reg / newinf_reg]
  uncertainty <- quantile(temp$prb5yr_reg, probs = c(0.5, 0.025, 0.975))
  val[val$id == j, c("prb5yr", "prb5yr_lci", "prb5yr_uci")] <- uncertainty
  
  temp[, prb500cd4_reg := nb500cd4_reg / newinf_reg]
  uncertainty <- quantile(temp$prb500cd4_reg, probs = c(0.5, 0.025, 0.975))
  val[val$id == j, c("prb500cd4", "prb500cd4_lci", "prb500cd4_uci")] <- uncertainty
  
  temp[, prb350cd4_reg := nb350cd4_reg / newinf_reg]
  uncertainty <- quantile(temp$prb350cd4_reg, probs = c(0.5, 0.025, 0.975))
  val[val$id == j, c("prb350cd4", "prb350cd4_lci", "prb350cd4_uci")] <- uncertainty
  
  temp[, propdx_reg := nbdx_reg / newinf_reg]
  uncertainty <- quantile(temp$propdx_reg, probs = c(0.5, 0.025, 0.975))
  val[val$id == j, c("propdx", "propdx_lci", "propdx_uci")] <- uncertainty
}

  return(val)
}


Agg_simul_pool_time_dx_prev <- function(out_simul_tdx_all,
                                          year = c(2015:2023),
                                          age = "15-24+25-34+35-49+50-99",
                                          sex = "male+female") {
  
  
  
  require(data.table,quietly = T)
  
  n_year <- length(year)
  year_1 = year
  # 1. Initialize output
  val <- data.frame(
    year = year,
    age = rep(paste(age), n_year),
    sex = rep(paste(sex), n_year),
    time_dx = NA, time_dx_lci = NA, time_dx_uci = NA,
    prb1yr = NA, prb1yr_lci = NA, prb1yr_uci = NA,
    prb2yr = NA, prb2yr_lci = NA, prb2yr_uci = NA,
    prb5yr = NA, prb5yr_lci = NA, prb5yr_uci = NA,
    prb500cd4 = NA, prb500cd4_lci = NA, prb500cd4_uci = NA,
    prb350cd4 = NA, prb350cd4_lci = NA, prb350cd4_uci = NA,
    propdx = NA, propdx_lci = NA, propdx_uci = NA
  )
  val$id <- paste(val$year, val$age, val$sex, sep = "_")
  
  res = as.data.table(out_simul_tdx_all)
  
  setDT(res)  # make into data.table
  
  # 3. Preprocessing
  res[, id := paste(year, age, sex, sep = "_")]
  res = res[res$year %in% year_1,]
  
  # 4. Precompute quantities
  res[, `:=`(
    nb1yr = prb1yr * w,
    nb2yr = prb2yr * w,
    nb5yr = prb5yr * w,
    nb500cd4 = prb500cd4 * w,
    nb350cd4 = prb350cd4 * w,
    nbdx = propdx * w
  )]
  
  # 5. Group by id and sampi
  pooled <- res[, .(
    time_dx_reg = weighted.mean(time_dx_med, w),
    nb1yr_reg = sum(nb1yr),
    nb2yr_reg = sum(nb2yr),
    nb5yr_reg = sum(nb5yr),
    nb500cd4_reg = sum(nb500cd4),
    nb350cd4_reg = sum(nb350cd4),
    nbdx_reg = sum(nbdx),
    newinf_reg = sum(w)
  ), by = .(id, sampi)]
  
  # 6. Now for each id calculate the uncertainties
  ids <- unique(pooled$id)
  
  for (j in ids) {
    temp <- pooled[id == j]
    
    # Time to diagnosis
    uncertainty <- quantile(temp$time_dx_reg, probs = c(0.5, 0.025, 0.975))
    val[val$id == j, c("time_dx", "time_dx_lci", "time_dx_uci")] <- uncertainty
    
    # Diagnosis probabilities
    temp[, prb1yr_reg := nb1yr_reg / newinf_reg]
    uncertainty <- quantile(temp$prb1yr_reg, probs = c(0.5, 0.025, 0.975))
    val[val$id == j, c("prb1yr", "prb1yr_lci", "prb1yr_uci")] <- uncertainty
    
    temp[, prb2yr_reg := nb2yr_reg / newinf_reg]
    uncertainty <- quantile(temp$prb2yr_reg, probs = c(0.5, 0.025, 0.975))
    val[val$id == j, c("prb2yr", "prb2yr_lci", "prb2yr_uci")] <- uncertainty
    
    temp[, prb5yr_reg := nb5yr_reg / newinf_reg]
    uncertainty <- quantile(temp$prb5yr_reg, probs = c(0.5, 0.025, 0.975))
    val[val$id == j, c("prb5yr", "prb5yr_lci", "prb5yr_uci")] <- uncertainty
    
    temp[, prb500cd4_reg := nb500cd4_reg / newinf_reg]
    uncertainty <- quantile(temp$prb500cd4_reg, probs = c(0.5, 0.025, 0.975))
    val[val$id == j, c("prb500cd4", "prb500cd4_lci", "prb500cd4_uci")] <- uncertainty
    
    temp[, prb350cd4_reg := nb350cd4_reg / newinf_reg]
    uncertainty <- quantile(temp$prb350cd4_reg, probs = c(0.5, 0.025, 0.975))
    val[val$id == j, c("prb350cd4", "prb350cd4_lci", "prb350cd4_uci")] <- uncertainty
    
    temp[, propdx_reg := nbdx_reg / newinf_reg]
    uncertainty <- quantile(temp$propdx_reg, probs = c(0.5, 0.025, 0.975))
    val[val$id == j, c("propdx", "propdx_lci", "propdx_uci")] <- uncertainty
  }
  
  return(val)
}

