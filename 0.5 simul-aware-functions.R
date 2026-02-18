simul_aware_agg_counter <- function(samp, mod, fp,hivdemo_proj,pmtct, counter_years,counter_anc, year = c(2000:2020),
                                    age = c("15-24", "25-34", "35-49", "50-99"),
                                    sex = c("male", "female")) {
  
  source("anc testing/1.0 simmod.R")
  library(Rcpp)
  n_year <- length(year)
  index_w <- year - fp$ss$proj_start + 1
  
  if(all(age %in% c("15-24", "25-34", "35-49", "50-99")) & length(unique(age)) == 4) {
    index_age <- 1:9 
    age_out_aware_ind <- "15-99" }
  
  if(all(age %in% c("15-24", "25-34", "35-49")) & length(unique(age)) == 3) {
    index_age <- 1:8 
    age_out_aware_ind <- "15-49" }
  
  if(all(age %in% c("25-34", "35-49", "50-99")) & length(unique(age)) == 3) {
    index_age <- 4:9 
    age_out_aware_ind <- "25-99" }
  if(all(age %in% c("25-99"))) {
    index_age <- 4:9
    age_out_aware_ind <- "25-99" }
  if(all(age %in% c("15-24"))) {
    index_age <- 1:3 
    age_out_aware_ind <- "15-24" }
  if(all(age %in% c("25-34"))) {
    index_age <- 4:5 
    age_out_aware_ind <- "25-34"}
  if(all(age %in% c("35-49"))) {
    index_age <- 6:8 
    age_out_aware_ind <- "35-49"}
  if(all(age %in% c("50-99"))) {
    index_age <- 9 
    age_out_aware_ind <- "50-99"}
  
  if(all(sex %in% c("male", "female")) & length(unique(sex)) == 2) {
    index_sex <- 1:2 
    sex_out_aware_ind <- "both" }
  if(all(sex %in% c("male"))) {
    index_sex <- 1 
    sex_out_aware_ind <- "male" }
  if(all(sex %in% c("female"))) {
    index_sex <- 2 
    sex_out_aware_ind <- "female" }
  
  poids <- data.frame(year = year, w = apply(attr(mod, "hivpop")[, index_age, index_sex, index_w, drop = FALSE], 4, FUN = sum) +
                        apply(attr(mod, "artpop")[, , index_age, index_sex, ,index_w, drop = FALSE], 6, FUN = sum))
  
  # define counterfactual outside loop
  start_anc = counter_anc$start
  if (!is.na(start_anc)) {
    #need to remove minus 1 from end as end captures first year post decline.
    end_anc = if (!is.na(counter_anc$end)) {
      counter_anc$end - 1
    } else{
      62 - 1
    }
    pmtct$anc_test[start_anc:end_anc - 30] = pmtct$anc_test[counter_anc$start -
                                                              30]
    pmtct$receivepmtct[start_anc:end_anc] = pmtct$receivepmtct[counter_anc$start]
    pmtct$needpmtct[start_anc:end_anc] = pmtct$needpmtct[counter_anc$start]
  }
  
  
  # Create parameters (proper scale, etc.), and simulate model
  fun_samp_par <- function(i, samp, mod, fp,hivdemo_proj,pmtct, poids, age_out_aware_ind, sex_out_aware_ind, year,counter_years,counter_anc) {
    source("anc testing/1.0 simmod.R")
    source("anc testing/1.1 tot test out.R")
    
    sampi = samp[i,]
    
    if(!is.na(as.numeric(counter_years[2]))){
        convert = (length(sampi)-3)/2
        start = as.numeric(counter_years[2])
        
        # set as the year of recovery(after 2000)-1(to allow for indexed replacment)  
        end = as.numeric(ifelse(!is.na(counter_years[3]),(as.numeric(counter_years[3])-30-1),30))
        
        start_female = start - 30
        
        start_dx = convert + (start - 40)
        
        end_dx = start_dx + (end - start_female) 
        
        
        
        sampi[start_female:min(end,convert)] = sampi[start_female]
        sampi[start_dx:min(end_dx,convert+convert-10)] = sampi[start_dx]
    }
    
    fp_local <- create_anc_param(sampi, fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
    
    mod <- simmod_anc_t(fp_local)
    result_i <- get_out_aware(mod, fp_local, agegr = age_out_aware_ind, sex = sex_out_aware_ind)
    result_i <- result_i[result_i$year %in% year, ]
    result_i$sampi <- i
    rownames(result_i) <- NULL
    return(result_i)
  }
  
  
  num_cores = parallel::detectCores()-5
  cl <- parallel::makeCluster(num_cores)  # Create a cluster with available cores
  parallel::clusterExport(cl, 
                          varlist = c("samp", "mod", "fp", "poids", "age_out_aware_ind", 
                                      "sex_out_aware_ind", "year", "fun_samp_par",
                                      "hivdemo_proj","pmtct","counter_years","counter_anc"),
                          envir = environment())
  parallel::clusterEvalQ(cl, {
    library(Rcpp)
  })
  val_lst <- parallel::parLapply(
    cl, 
    X = 1:nrow(samp), 
    fun = function(i) {
      fun_samp_par(
        i, samp = samp, mod = mod, fp = fp, poids = poids, 
        age_out_aware_ind = age_out_aware_ind, 
        sex_out_aware_ind = sex_out_aware_ind, 
        year = year,hivdemo_proj = hivdemo_proj,
        pmtct = pmtct, counter_years = counter_years,
        counter_anc = counter_anc
      )
    }
  )
  parallel::stopCluster(cl)  # Stop the cluster
  
  val_ <- data.table::rbindlist(val_lst)
  val <- merge(val_, poids, by = "year")
  #val$age <- paste(age, collapse = "+")
  val$sex <- paste(sex, collapse = "+")
  #val$propaware <- val$value
  
  return(val)
  
}


simul_aware_agg <- function(samp, mod, fp,hivdemo_proj,pmtct, year = c(2000:2020),
                                    age = c("15-24", "25-34", "35-49", "50-99"),
                                    sex = c("male", "female")) {
  
  source("anc testing/1.0 simmod.R")
  source("anc testing/1.1 tot test out.R")
  library(Rcpp)
  n_year <- length(year)
  index_w <- year - fp$ss$proj_start + 1
  
  if(all(age %in% c("15-24", "25-34", "35-49", "50-99")) & length(unique(age)) == 4) {
    index_age <- 1:9 
    age_out_aware_ind <- "15-99" }
  
  if(all(age %in% c("15-24", "25-34", "35-49")) & length(unique(age)) == 3) {
    index_age <- 1:8 
    age_out_aware_ind <- "15-49" }
  
  if(all(age %in% c("25-34", "35-49", "50-99")) & length(unique(age)) == 3) {
    index_age <- 4:9 
    age_out_aware_ind <- "25-99" }
  if(all(age %in% c("25-99"))) {
    index_age <- 4:9
    age_out_aware_ind <- "25-99" }
  if(all(age %in% c("15-24"))) {
    index_age <- 1:3 
    age_out_aware_ind <- "15-24" }
  if(all(age %in% c("25-34"))) {
    index_age <- 4:5 
    age_out_aware_ind <- "25-34"}
  if(all(age %in% c("35-49"))) {
    index_age <- 6:8 
    age_out_aware_ind <- "35-49"}
  if(all(age %in% c("50-99"))) {
    index_age <- 9 
    age_out_aware_ind <- "50-99"}
  
  if(all(sex %in% c("male", "female")) & length(unique(sex)) == 2) {
    index_sex <- 1:2 
    sex_out_aware_ind <- "both" }
  if(all(sex %in% c("male"))) {
    index_sex <- 1 
    sex_out_aware_ind <- "male" }
  if(all(sex %in% c("female"))) {
    index_sex <- 2 
    sex_out_aware_ind <- "female" }
  
  poids <- data.frame(year = year, w = apply(attr(mod, "hivpop")[, index_age, index_sex, index_w, drop = FALSE], 4, FUN = sum) +
                        apply(attr(mod, "artpop")[, , index_age, index_sex, ,index_w, drop = FALSE], 6, FUN = sum))
  
  #print("break")
  # Create parameters (proper scale, etc.), and simulate model
  fun_samp_par <- function(i, samp, mod, fp,hivdemo_proj,pmtct, poids, age_out_aware_ind, sex_out_aware_ind, year) {
    source("anc testing/1.0 simmod.R")
    source("anc testing/1.1 tot test out.R")
    fp <- create_anc_param(samp[i, ], fp,pmtct = pmtct,hivdemo_proj = hivdemo_proj)
    
    mod <- simmod_anc_t(fp)
    result_i <- get_out_aware(mod, fp, agegr = age_out_aware_ind, sex = sex_out_aware_ind)
    result_i <- result_i[result_i$year %in% year, ]
    result_i$sampi <- i
    rownames(result_i) <- NULL
    return(result_i)
  }
  
  
  num_cores = parallel::detectCores()-5
  cl <- parallel::makeCluster(num_cores)  # Create a cluster with available cores
  parallel::clusterExport(cl, 
                          varlist = c("samp", "mod", "fp", "poids", "age_out_aware_ind", 
                                      "sex_out_aware_ind", "year", "fun_samp_par",
                                      "hivdemo_proj","pmtct"),
                          envir = environment())
  parallel::clusterEvalQ(cl, {
    library(Rcpp)
  })
  val_lst <- parallel::parLapply(
    cl, 
    X = 1:nrow(samp), 
    fun = function(i) {
      fun_samp_par(
        i, samp = samp, mod = mod, fp = fp, poids = poids, 
        age_out_aware_ind = age_out_aware_ind, 
        sex_out_aware_ind = sex_out_aware_ind, 
        year = year,hivdemo_proj = hivdemo_proj,
        pmtct = pmtct
      )
    }
  )
  parallel::stopCluster(cl)  # Stop the cluster
  
  #print("break")
  val_ <- data.table::rbindlist(val_lst)
  val <- merge(val_, poids, by = "year")
  #val$age <- paste(age, collapse = "+")
  val$sex <- paste(sex, collapse = "+")
  val$propaware <- val$value
  
  return(val)
  
}




Agg_simul_aware_old <- function(lst,
                            year = c(2015:2023),
                            age = "15-99",
                            sex = "both",
                            region = "SSA") {

  n_cnt <- length(lst)
  n_year <- length(year)
  val <- data.frame(year = year,
                    age = rep(paste(age), n_year),
                    sex = rep(paste(sex), n_year),
                    propaware = NA, lci = NA, uci = NA,
                    nb_aware = NA, nb_aware_lci = NA, nb_aware_uci = NA,
                    nb_unaware = NA, nb_unaware_lci = NA, nb_unaware_uci = NA)

  val$id <- paste(val$year, val$age, val$sex, sep = "_")
  # Create data frame
  res <- NULL

  for (i in 1:n_cnt){
    if(length(lst)==1){
      lst_i = lst
      #print("1 dataframe used")
    }else{
      lst_i <- lst[[i]]
      }


      if (age == "15-99" && sex == "both") {
        res_i <- as.data.frame(lst_i$out_simul_aware_all) }
      if (age == "15-99" && sex == "female") {
        res_i <- as.data.frame(lst_i$out_simul_aware_all) }
      if (age == "15-99" && sex == "male") {
        res_i <- as.data.frame(lst_i$out_simul_aware_all) }

      if (age == "15-24" && sex == "both") {
        res_i <- as.data.frame(lst_i$out_simul_aware_1524) }
      if (age == "25-34" && sex == "both") {
        res_i <- as.data.frame(lst_i$out_simul_aware_2534) }
      if (age == "35-49" && sex == "both") {
        res_i <- as.data.frame(lst_i$out_simul_aware_3549) }
      if (age == "50-99" && sex == "both") {
        res_i <- as.data.frame(lst_i$out_simul_aware_5099) }
      if (age == "15-24" && sex == "female") {
        res_i <- as.data.frame(lst_i$out_simul_aware_f_1524) }
      if (age == "25-34" && sex == "female") {
        res_i <- as.data.frame(lst_i$out_simul_aware_f_2534) }
      if (age == "35-49" && sex == "female") {
        res_i <- as.data.frame(lst_i$out_simul_aware_f_3549) }
      if (age == "50-99" && sex == "female") {
        res_i <- as.data.frame(lst_i$out_simul_aware_f_5099) }
      if (age == "15-24" && sex == "male") {
        res_i <- as.data.frame(lst_i$out_simul_aware_m_1524) }
      if (age == "25-34" && sex == "male") {
        res_i <- as.data.frame(lst_i$out_simul_aware_m_2534) }
      if (age == "35-49" && sex == "male") {
        res_i <- as.data.frame(lst_i$out_simul_aware_m_3549) }
      if (age == "50-99" && sex == "male") {
        res_i <- as.data.frame(lst_i$out_simul_aware_m_5099) }


    res_i$cnt <- names(lst)[i]
    res <- rbind(res, res_i)

  }

  res$id <- paste(res$year, res$agegr, res$sex, sep = "_")

  # we calculate the nb aware "nb" and the nb unaware

  res$aware <- res$propaware * res$w
  res$unaware <- (1 - res$propaware) * res$w

  # we pool here
  n_id <- unique(res$id)
  for (j in 1:length(n_id)) {

    pol <- res[res$id == n_id[j], ]

    for (s in 1:length(unique(pol$sampi))) {
      pol$nb_aware_reg[pol$sampi == s] <- sum(pol$aware[pol$sampi == s])
      pol$nb_unaware_reg[pol$sampi == s] <- sum(pol$unaware[pol$sampi == s])
      pol$PLHIV_reg[pol$sampi == s] <- sum(pol$w[pol$sampi == s])
    }

    # we calculate the proportion aware, nb aware, and nb unaware in the region
    pol$propaware_reg <- pol$nb_aware_reg / pol$PLHIV_reg
    uncertainty <- quantile(pol$propaware_reg, probs = c(0.5, 0.025, 0.975))
    val$propaware[val$id == n_id[j]] <- uncertainty[1]
    val$lci[val$id == n_id[j]] <- uncertainty[2]
    val$uci[val$id == n_id[j]] <- uncertainty[3]

    uncertainty <- quantile(pol$nb_aware_reg, probs = c(0.5, 0.025, 0.975))
    val$nb_aware[val$id == n_id[j]] <- uncertainty[1]
    val$nb_aware_lci[val$id == n_id[j]] <- uncertainty[2]
    val$nb_aware_uci[val$id == n_id[j]] <- uncertainty[3]

    uncertainty <- quantile(pol$nb_unaware_reg, probs = c(0.5, 0.025, 0.975))
    val$nb_unaware[val$id == n_id[j]] <- uncertainty[1]
    val$nb_unaware_lci[val$id == n_id[j]] <- uncertainty[2]
    val$nb_unaware_uci[val$id == n_id[j]] <- uncertainty[3]

  }

  return(val)

}




simul_aware_lst <- function(lst,
                            year = c(2000:2020),
                            cnt = "Angola",
                            age = "15-24+25-34+35-49+50-99",
                            sex = "male+female") {
  
  # if (age == "15-24+25-34+35-49+50-99" && sex == "male+female") { lst = lst_aware_all[[cnt]]$out_simul_aware_all }
  # if (age == "15-24+25-34+35-49+50-99" && sex == "female") { lst = lst_aware_all[[cnt]]$out_simul_aware_f }
  # if (age == "15-24+25-34+35-49+50-99" && sex == "male") { lst = lst_aware_all[[cnt]]$out_simul_aware_m }
  # if (age == "15-24" && sex == "male+female") { lst = lst_aware_all[[cnt]]$out_simul_aware_1524 }
  # if (age == "25-34" && sex == "male+female") { lst = lst_aware_all[[cnt]]$out_simul_aware_2534 }
  # if (age == "35-49" && sex == "male+female") { lst = lst_aware_all[[cnt]]$out_simul_aware_3549 }
  # if (age == "50-99" && sex == "male+female") { lst = lst_aware_all[[cnt]]$out_simul_aware_5099 }
  # if (age == "25-99" && sex == "male+female") { lst = lst_aware_all[[cnt]]$out_simul_aware_2599 }
  # 
  # if (age == "15-24" && sex == "male") { lst = lst_aware_all[[cnt]]$out_simul_aware_m_1524 }
  # if (age == "25-34" && sex == "male") { lst = lst_aware_all[[cnt]]$out_simul_aware_m_2534 }
  # if (age == "35-49" && sex == "male") { lst = lst_aware_all[[cnt]]$out_simul_aware_m_3549 }
  # if (age == "50-99" && sex == "male") { lst = lst_aware_all[[cnt]]$out_simul_aware_m_5099 }
  # if (age == "25-99" && sex == "male") { lst = lst_aware_all[[cnt]]$out_simul_aware_m_2599 }
  # 
  # if (age == "15-24" && sex == "female") { lst = lst_aware_all[[cnt]]$out_simul_aware_f_1524 }
  # if (age == "25-34" && sex == "female") { lst = lst_aware_all[[cnt]]$out_simul_aware_f_2534 }
  # if (age == "35-49" && sex == "female") { lst = lst_aware_all[[cnt]]$out_simul_aware_f_3549 }
  # if (age == "50-99" && sex == "female") { lst = lst_aware_all[[cnt]]$out_simul_aware_f_5099 }
  # if (age == "25-99" && sex == "female") { lst = lst_aware_all[[cnt]]$out_simul_aware_f_2599 }
  # 
  
  n_year <- length(year)
  
  val <- data.frame(year = year,
                    cnt = rep(paste(cnt), n_year),
                    age = rep(paste(age), n_year),
                    sex = rep(paste(sex), n_year),
                    propaware = NA, lci = NA, uci = NA )
  
  val$id <- paste(val$cnt, val$year, val$age, val$sex, sep = "_")
  
  # Create data frames
  res <- data.table::as.data.table(lst)
  
  for (n in 1:n_year) {
    
    uncertainty <- quantile(res$value[res$year == year[n]], probs = c(0.5, 0.025, 0.975))
    val$propaware[val$year == year[n]] <- uncertainty[1]
    val$lci[val$year == year[n]] <- uncertainty[2]
    val$uci[val$year == year[n]] <- uncertainty[3]
    
  }
  
  return(val)
  
}

Agg_simul_aware <- function(lst,
                            year = c(2015:2023),
                            age = "15-99",
                            sex = "both"
) {
  n_cnt <- length(lst)
  n_year <- length(year)
  
  # 1. Create output table
  val <- data.table(
    year = year,
    age = rep(paste(age), n_year),
    sex = rep(paste(sex), n_year),
    propaware = NA_real_, lci = NA_real_, uci = NA_real_,
    nb_aware = NA_real_, nb_aware_lci = NA_real_, nb_aware_uci = NA_real_,
    nb_unaware = NA_real_, nb_unaware_lci = NA_real_, nb_unaware_uci = NA_real_
  )
  if(sex == "both"){
    val[, sex := "male+female"]
  }
  val[, id := paste(year, age, sex, sep = "_")]
  
  # 2. Combine all simulations
  res_list <- lapply(seq_len(n_cnt), function(i) {
    lst_i <- if (n_cnt == 1) lst else lst[[i]]
    
    res_i <- switch(
      paste(age, sex),
      "15-99 both" = as.data.table(lst_i$out_simul_aware_all),
      "15-99 female" = as.data.table(lst_i$out_simul_aware_all),
      "15-99 male" = as.data.table(lst_i$out_simul_aware_all),
      "15-24 both" = as.data.table(lst_i$out_simul_aware_1524),
      "25-34 both" = as.data.table(lst_i$out_simul_aware_2534),
      "35-49 both" = as.data.table(lst_i$out_simul_aware_3549),
      "50-99 both" = as.data.table(lst_i$out_simul_aware_5099),
      "15-24 female" = as.data.table(lst_i$out_simul_aware_f_1524),
      "25-34 female" = as.data.table(lst_i$out_simul_aware_f_2534),
      "35-49 female" = as.data.table(lst_i$out_simul_aware_f_3549),
      "50-99 female" = as.data.table(lst_i$out_simul_aware_f_5099),
      "15-24 male" = as.data.table(lst_i$out_simul_aware_m_1524),
      "25-34 male" = as.data.table(lst_i$out_simul_aware_m_2534),
      "35-49 male" = as.data.table(lst_i$out_simul_aware_m_3549),
      "50-99 male" = as.data.table(lst_i$out_simul_aware_m_5099)
    )
    res_i[, cnt := names(lst)[i]]
    res_i
  })
  
  res <- rbindlist(res_list)
  
  res[, id := paste(year, agegr, sex, sep = "_")]
  
  # 3. Precompute
  res[, aware := value * w]
  res[, unaware := (1 - value) * w]
  
  # 4. Aggregate by id and sampi
  pooled <- res[, .(
    nb_aware_reg = sum(aware),
    nb_unaware_reg = sum(unaware),
    PLHIV_reg = sum(w)
  ), by = .(id, sampi)]
  
  # 5. Summarize uncertainty
  ids <- unique(pooled$id)
  
  for (j in ids) {
    temp <- pooled[id == j]
    
    temp[, propaware_reg := nb_aware_reg / PLHIV_reg]
    
    val[id == j, `:=`(
      propaware = quantile(temp$propaware_reg, probs = 0.5, na.rm = TRUE),
      lci = quantile(temp$propaware_reg, probs = 0.025, na.rm = TRUE),
      uci = quantile(temp$propaware_reg, probs = 0.975, na.rm = TRUE),
      nb_aware = quantile(temp$nb_aware_reg, probs = 0.5, na.rm = TRUE),
      nb_aware_lci = quantile(temp$nb_aware_reg, probs = 0.025, na.rm = TRUE),
      nb_aware_uci = quantile(temp$nb_aware_reg, probs = 0.975, na.rm = TRUE),
      nb_unaware = quantile(temp$nb_unaware_reg, probs = 0.5, na.rm = TRUE),
      nb_unaware_lci = quantile(temp$nb_unaware_reg, probs = 0.025, na.rm = TRUE),
      nb_unaware_uci = quantile(temp$nb_unaware_reg, probs = 0.975, na.rm = TRUE)
    )]
  }
  
  return(val)
}


