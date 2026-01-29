anc_total_tests <- function(mod, df) {
  all_diag <- rep(NA, length(df$haidx))
  for (i in seq_along(df$haidx)) {
    hivdx <- switch(df$hivstatus[i], all = 1:6, negative = 1:2, positive = 3:6)
    haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
    if (df$sidx[i] == 0) {
      sidx <- 1:2
    } else { sidx <- df$sidx[i] } 
    all_diag[i] <- colSums(attr(mod, "anctests")[haidx, sidx, hivdx, df$yidx[i], drop = FALSE], , 3)
  }
  return(all_diag)
}

vct_total_tests <- function(mod, df) {
  all_diag <- rep(NA, length(df$haidx))
  for (i in seq_along(df$haidx)) {
    hivdx <- switch(df$hivstatus[i], all = 1:6, negative = 1:2, positive = 3:6)
    haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
    if (df$sidx[i] == 0) {
      sidx <- 1:2
    } else { sidx <- df$sidx[i] } 
    all_diag[i] <- colSums(attr(mod, "vcttests")[haidx, sidx, hivdx, df$yidx[i], drop = FALSE], , 3)
  }
  return(all_diag)
}

survey_tests <- function(dat, sex, age, hivstatus) {
  if(is.null(dat)) {
    dat <- data.frame(country = 0, surveyid = 0, year= 0, agegr = 0, sex = 0, outcome = 0, 
                      hivstatus = 0, est = 0, se = 0, ci_l = 0, ci_u = 0, counts = 0)
  } else if(dim(dat)[1] == 0) {
    dat = data.frame(country = 0, surveyid = 0, year = 0, agegr = 0, sex = 0, outcome = 0, 
                     hivstatus = 0, est = 0, se = 0, ci_l = 0, ci_u = 0, counts = 0)
  } else if(dim(dat)[1] != 0) {
    dat <- dat }
  
  sex_vec = dat$sex %in% sex
  dat <- dat[sex_vec, ]
  age_vec = dat$agegr == age
  dat <- dat[age_vec, ]
  status_vec = dat$hivstatus == hivstatus
  dat <- dat[status_vec, ]
  return(dat)
}

graph_tot_tests <- function(country_makesimul, cnt,
                            sex = "both", hivstatus = "all", 
                            test_type = "hivtests", year_start = 2018, year_end = 2023,
                            survey = FALSE, TitleT = TRUE,ANC_mode = T) {
  #source(here::here("anc testing/1.0 simmod.R"))
  
  # if(test_type == "anctests" && ANC_mode == T){
  #   opt = country_makesimul$opt$par
  #   opt[50] = -100
  #   country_makesimul$fp = create_anc_param(opt,country_makesimul$fp,pmtct,hivdemo_proj)
  # }
  mod <- country_makesimul$mod
  simul <- country_makesimul$simul
  #print(country_makesimul$prg_dat)
  prgdat <- country_makesimul$prgm_dat
  if (length(unique(prgdat$sex)) > 1) {
    #some countries have a sex stratification of 2020 male, female, both, resulting in double counting, this fixes it by removing both
    if(any(prgdat$year[which(prgdat$sex == "female")] %in% prgdat$year[which(prgdat$sex == "both")])){
      prgboth = prgdat[(!(prgdat$year %in% prgdat$year[which(prgdat$sex == "female"|prgdat$sex == "male")])),] 
      prgmf = prgdat[(prgdat$year %in% prgdat$year[which(prgdat$sex == "female")] & prgdat$sex != "both"),]
      prgdat = rbind(prgboth,prgmf)
    }
    
    prg_tot <- stats::aggregate(prgdat$tot, by = list(prgdat$year), FUN = sum)
    prg_totpos <- stats::aggregate(prgdat$totpos, by = list(prgdat$year), FUN = sum)
    prg_vct <- stats::aggregate(prgdat$vct, by = list(prgdat$year), FUN = sum)
    prg_vctpos <- stats::aggregate(prgdat$vctpos, by = list(prgdat$year), FUN = sum)
    prg_anc <- stats::aggregate(prgdat$anc, by = list(prgdat$year), FUN = function(x) sum(x, na.rm = TRUE))
    prg_ancpos <- stats::aggregate(prgdat$ancpos, by = list(prgdat$year), FUN = function(x) sum(x, na.rm = TRUE))
    prg_anc$x[prg_anc$x == 0] <- NA
    prg_ancpos$x[prg_ancpos$x == 0] <- NA
    prd_df <- data.frame(country = cnt, year = prg_tot[, 1], sex = "both", agegr = "15-99",
                         tot = prg_tot$x, totpos = prg_totpos$x, vct = prg_vct$x, vctpos = prg_vctpos$x,
                         anc = prg_anc$x, ancpos = prg_ancpos$x)
    prgdat <- prd_df
  }
  
  fp <- country_makesimul$fp       
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  out_test <- expand.grid(year = year_start:2023, outcome = "numbertests", 
                          agegrp = "15+", sex = sex, hivstatus = hivstatus)
  out_test$hivstatus <- as.character(out_test$hivstatus)
  
  #print(paste(test_type, hivstatus, nrow(prgdat)))
  if (test_type == "hivtests") {
    out_test$value <- first90:::total_tests(mod, add_ss_indices(out_test, fp$ss))
    if (hivstatus == "positive" && nrow(prgdat) > 0) {
      prgdat$tot1 <- prgdat$totpos
    } else if (hivstatus %in% c("negative", "all") && nrow(prgdat) > 0) {
      prgdat$tot1 <- prgdat$tot
    }
  } else if (test_type == "anctests") {
    out_test$value <- anc_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
    if (hivstatus == "positive" && nrow(prgdat) > 0) {
      prgdat$tot1 <- prgdat$ancpos
    } else if (hivstatus %in% c("negative","all") && nrow(prgdat) > 0) { prgdat$tot1 <- prgdat$anc }
  } else if (test_type == "vcttests") {
    out_test$value <- vct_total_tests(mod, add_ss_indices(out_test, fp$ss))
    if (hivstatus == "positive" && nrow(prgdat) > 0){
      prgdat$tot1 <- prgdat$vctpos
    } else if (hivstatus %in% c("negative","all") && nrow(prgdat) > 0) {prgdat$tot1 <- prgdat$vct }
  }
  
  test_name = case_when(
    test_type == "hivtests" ~ "HIV Tests",
    test_type == "vcttests" ~ "non-ANC Tests",
    test_type == "anctests" ~ "ANC Tests"
    
  )
  
  if (TitleT == TRUE) {
    main_title <- paste0(cnt, ": ", ifelse(hivstatus == "all", "",paste0(hivstatus," " )),
                         test_name, " from ", year_start, " to ", year_end)
  } else if (TitleT == FALSE) {
    main_title = ""
  }
  
  ci <- NULL
  max_y <- max(out_test$value * 1.1)
  if (!is.null(simul)) {
    if(test_type == "vcttests"){
      
      if (hivstatus == "positive") { 
        simul1 = simul$anctestspos
        
        simul1[,7:dim(simul1)[2]] = simul$hivtestspos[,7:dim(simul1)[2]] - simul$anctestspos[,7:dim(simul1)[2]]
        simul1$outcome = "vcttests"
        
        ci <- getci(simul1)
        ci <- ci[ci$sex == sex, ]
        ci <- ci[ci$hivstatus == hivstatus, ]
        max_y <- max(ci$upper) * 1.05
      } else { 
        simul1 = simul$anctests
        simul1[,7:dim(simul1)[2]] = simul$hivtests[,7:dim(simul1)[2]] - simul$anctests[,7:dim(simul1)[2]]
        simul1$outcome = "vcttests"
        
        ci <- getci(simul1)
        ci <- ci[ci$sex == sex, ]
        ci <- ci[ci$hivstatus == hivstatus, ]
        max_y <- max(ci$upper,max_y) * 1.05
        
      }
    }else{
      test_type_name <- test_type
      if (hivstatus == "positive") { test_type_name <- paste0(test_type, "pos") }
      ci <- getci(simul[[test_type_name]])
      ci <- ci[ci$sex == sex, ]
      ci <- ci[ci$hivstatus == hivstatus, ]
      max_y <- max(ci$upper,max_y) * 1.05
    }
  }
  max_y <- max(c(max_y, prgdat$tot1 * 1.01))
  
  col <- "steelblue" # default
  if (test_type == "hivtests") { col <- "steelblue4" 
  if (hivstatus == "positive") { col <- "firebrick3" }} 
  if (test_type == "anctests") { col <- "red" 
  if (hivstatus == "positive") { col <- "lightsalmon3" }}
  if (test_type == "vcttests") { col <- "forestgreen" 
  if (hivstatus == "positive") { col <- "aquamarine3" }} 
  
  
  if (survey == FALSE | is.null(country_makesimul$dat)) {
    if (!(nrow(prgdat) > 0)) {
      out_tests <- ggplot() +
        geom_ribbon(aes(x = ci$year, ymin = ci$lower/1000, ymax = ci$upper/1000), alpha = 0.2) + 
        geom_line(out_test, mapping = aes(y = value/1000, x = year), color = col, linewidth = 1.1) +
        theme_minimal() +
        labs(title = main_title, x = "year", y = "Number of tests (thousands)") +
        xlim(year_start, year_end) +
        ylim(0, max_y/1000)
    }
    if (nrow(prgdat) > 0) {
      out_tests <- ggplot() +
        geom_ribbon(aes(x = ci$year, ymin = ci$lower/1000, ymax = ci$upper/1000), alpha = 0.2) + 
        geom_line(data = out_test, mapping = aes(y = value/1000, x = year), color = col, linewidth = 1.1) +
        theme_minimal() +
        labs(title = main_title, x = "year", y = "Number of tests (thousands)") +
        xlim(year_start, year_end) +
        ylim(0, max_y/1000) +
        geom_point(prgdat, mapping = aes(y = tot1/1000, x = year), size =  2)
    }
  }
  return(out_tests)  
}

graph_evertest <- function(country_makesimul, cnt, dat_org,
                           sex = "both", hivstatus = "all", 
                           age = "15-49", year_start = 2000, year_end = 2030,
                           survey = TRUE, outcome = "evertest", TitleT = TRUE,
                           axisy = T,axisx = T,axis_angle = NULL,
                           axisx_text = T,axisy_text = T) {
  
  mod <- country_makesimul$mod
  evertest <- country_makesimul$out_evertest
  fp <- country_makesimul$fp
  simul <- country_makesimul$simul
  
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  out_evertest <- expand.grid(year = year_start:end_date, outcome = outcome, 
                              agegrp = age, sex = sex, hivstatus = hivstatus)
  out_evertest$hivstatus <- as.character(out_evertest$hivstatus)
  evertest <- evertest[evertest$sex == sex, ]
  evertest <- evertest[evertest$agegr == age, ]
  evertest <- evertest[evertest$hivstatus == hivstatus, ]
  
  dat <- survey_tests(dat_org, sex = sex, age = age, hivstatus = hivstatus)
  
  if (TitleT) {
    main_title <- paste0(cnt, ": ", ifelse(hivstatus == "all", "", hivstatus )," proportion evertested", " from ", year_start, " to ",year_end, " sex: ",sex, " age ",age)
  } else if (TitleT == FALSE) {
    main_title <- NULL }
  
  if(axisy){
    axisy_name = "Proportion Evertested"
  }else{ axisy_name = NULL }
  
  if(axisx){
    axisx_name = "Year"
  }else{ axisx_name = NULL }
  
  
  col <- "steelblue4" # default
  if (sex == "both") { col <- "darkorange1" }
  if (sex == "male") { col <- "skyblue4" }
  if (sex == "female") { col <- "plum4" }
  
  lty <- 1
  if (hivstatus == "positive") { lty <- 2 }
  
  ci <- NULL
  if (!is.null(simul)) {
    ci <- getci(simul[[outcome]])
    ci <- ci[ci$sex == sex, ]
    ci <- ci[ci$agegr == age, ]
    ci <- ci[ci$hivstatus == hivstatus, ]
  }
  
  p = ggplot() +
    geom_ribbon(aes(x = ci$year, ymin = ci$lower, ymax = ci$upper), alpha = 0.2) + 
    geom_line(evertest, mapping = aes(y = value,x = year), 
              color = col, lty = lty, size = 1.1) +
    geom_point(dat, mapping = aes(y = est, x = year)) +
    geom_errorbar(dat, mapping = aes(ymin = ci_l,ymax = ci_u, x = year), width = 0.1) +
    theme_minimal() +
    labs(title = main_title, x = axisx_name, y = axisy_name) +
    theme(
      axis.text.x = element_text(angle = axis_angle,
                                 hjust = ifelse(!is.null(axis_angle),ifelse(axis_angle>180,0,1),0.5)),
      axis.title.y = element_text(size = 9)
      
      
    )+
    xlim(year_start, year_end) +
    ylim(0, 1)
  if (!axisx_text) {
    p <- p + theme(axis.text.x = element_blank())
  }
  
  if (!axisy_text) {
    p <- p + theme(axis.text.y = element_blank())
  }
  return(p)
}


optimized_par_anc <- function(opt, param = NULL) {
  
  max_knot_year <- if (length(opt$par) == 44) {
    2019
  } else if (length(opt$par) == 46) {
    2020
  } else if (length(opt$par) == 48) {
    2021
  } else if (length(opt$par) == 50) {
    2022
  } else if (length(opt$par) == 52) {
    2023
  } else if (length(opt$par) == 54) {
    2024
  } else {
    stop("Unexpected length of parameter vector.")
  }
  
  knots <- c(1995, 2000:max_knot_year) - 1970 + 1L
  knots_rr_dxunt <- c(2010:max_knot_year) - 1970 + 1L
  
  n_k1 <- length(knots) - 1
  n_k2 <- n_k1 + length(knots_rr_dxunt)
  n_k3 <- n_k2 + 12
  
  if (is.null(opt$hessian)) {
    print("You forgot to specify Hessian = TRUE; 95%CrI not calculated.")
    se <- rep(NA, length(opt$par))
  } else {
    if (any(diag(opt$hessian) == 0)) {
      index <- which(diag(opt$hessian) == 0)
      opt$hessian[index, index] <- opt$hessian[index, index] - 0.001
    }
    vcova <- solve(-opt$hessian)
    eS <- eigen(vcova, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -0.000001 * abs(ev[1L]))) {
      vcova <- nearPD(vcova, corr = FALSE)$mat
    }
    se <- sqrt(diag(as.matrix(vcova)))
  }
  param_names <- c("RRm_05", "RRm_12", 
                   "RR_Test10", "RR_Test15", 
                   "RR_PLHIV", 
                   "RR_DxUnt10", "RR_DxUnt22", 
                   "RR_DxART", 
                   "RR_25p_m", "RR_35p_m", 
                   "RR_25p_f", "RR_35p_f", 
                   "RR OI Dx", "Prob ANC test WLHIV ART",
                   "OR ANC overestimation","OR ANC restest" )
  description <- c("RR testing: men in 2005", "RR testing: men in 2012", 
                   "RR re-testing 2010", "RR re-testing 2015", 
                   "RR testing: PLHIV unaware", 
                   "RR re-testing: PLHIV aware (not ART) 2010", "RR re-testing: PLHIV aware (not ART) 2022", 
                   "RR re-testing: PLHIV on ART (*RR not ART)", 
                   "RR among 25-34 men", "RR among 35+ men", 
                   "RR among 25-34 women", "RR among 35+ women", 
                   "RR OI Dx (ART Cov)", 
                   "RR ANC testing for WLHIV on ART",
                   "RR ANC test volume at 85% coverage", 
                   "RR ANC re-testing"
                   )
  
  pt <- c(round(stats::plogis(opt$par[n_k2 + 1]) * 10, 2), 
          round(stats::plogis(opt$par[n_k2 + 2]) * 10, 2), 
          round(0.95 + stats::plogis(opt$par[n_k2 + 3]) * 7.05, 2), 
          round(0.95 + stats::plogis(opt$par[n_k2 + 4]) * 7.05, 2), 
          round(0.05 + stats::plogis(opt$par[n_k2 + 5]) * (1.95 - 0.05), 2),
          round(stats::plogis(opt$par[n_k1 + 1]) * 8, 2), 
          round(stats::plogis(opt$par[n_k2 - 1]) * 8, 2), 
          round(stats::plogis(opt$par[n_k2 + 6]), 2), 
          round(0.1 + stats::plogis(opt$par[n_k2 + 7]) * (6 - 0.1), 2), 
          round(0.1 + stats::plogis(opt$par[n_k2 + 8]) * (6 - 0.1), 2), 
          round(0.1 + stats::plogis(opt$par[n_k2 + 9]) * (6 - 0.1), 2), 
          round(0.1 + stats::plogis(opt$par[n_k2 + 10]) * (6 - 0.1), 2),
          round(0.25 + (stats::plogis(opt$par[n_k2 + 11])) * (1.75 - 0.25), 2),
          round(0.05 + stats::plogis(opt$par[n_k3]) * (0.75 - 0.05), 2),
          round(exp(-1 * stats::plogis(opt$par[n_k3 + 1]) * (2) * 0.85), 2),
          round(1 + stats::plogis(opt$par[n_k3 + 2]) * (10 - 1), 2))
  
  if (is.null(param)) {
    lci <- c(round(stats::plogis(opt$par[n_k2 + 1] - stats::qnorm(0.975) * se[n_k2 + 1]) * 10, 2), 
             round(stats::plogis(opt$par[n_k2 + 2] - stats::qnorm(0.975) * se[n_k2 + 2]) * 10, 2), 
             round(0.95 + stats::plogis(opt$par[n_k2 + 3] - stats::qnorm(0.975) * se[n_k2 + 3]) * 7.05, 2), 
             round(0.95 + stats::plogis(opt$par[n_k2 + 4] - stats::qnorm(0.975) * se[n_k2 + 4]) * 7.05, 2),
             round(0.05 + stats::plogis(opt$par[n_k2 + 5] - stats::qnorm(0.975) * se[n_k2 + 5]) * (1.95 - 0.05), 2),
             round(stats::plogis(opt$par[n_k1 + 1] - stats::qnorm(0.975) * se[n_k1 + 1]) * 8, 2),
             round(stats::plogis(opt$par[n_k2 - 1] - stats::qnorm(0.975) * se[n_k2 - 1]) * 8, 2),
             round(stats::plogis(opt$par[n_k2 + 6] - stats::qnorm(0.975) * se[n_k2 + 6]), 2), 
             round(0.1 + stats::plogis(opt$par[n_k2 + 7] - stats::qnorm(0.975) * se[n_k2 + 7]) * (6 - 0.1), 2), 
             round(0.1 + stats::plogis(opt$par[n_k2 + 8] - stats::qnorm(0.975) * se[n_k2 + 8]) * (6 - 0.1), 2),
             round(0.1 + stats::plogis(opt$par[n_k2 + 9] - stats::qnorm(0.975) * se[n_k2 + 9]) * (6 - 0.1), 2),
             round(0.1 + stats::plogis(opt$par[n_k2 + 10] - stats::qnorm(0.975) * se[n_k2 + 10]) * (6 - 0.1), 2), 
             round(0.25 + (stats::plogis(opt$par[n_k2 + 11] - stats::qnorm(0.975) * se[n_k2 + 11])) * (1.75 - 0.25), 2),
             round(0.05 + stats::plogis(opt$par[n_k3] - stats::qnorm(0.975) * se[n_k3]) * (0.75 - 0.05), 2),
             round(exp(-1 * stats::plogis(opt$par[n_k3 + 1] - stats::qnorm(0.975) * se[n_k3 + 1]) * (2) * 0.85), 2),
             round(1 + stats::plogis(opt$par[n_k3 + 2] - stats::qnorm(0.975) * se[n_k3 + 2]) * (10 - 1), 2))

     uci <- c(round(stats::plogis(opt$par[n_k2 + 1] + stats::qnorm(0.975) * se[n_k2 + 1]) * 10, 2), 
             round(stats::plogis(opt$par[n_k2 + 2] + stats::qnorm(0.975) * se[n_k2 + 2]) * 10, 2), 
             round(0.95 + stats::plogis(opt$par[n_k2 + 3] + stats::qnorm(0.975) * se[n_k2 + 3]) * 7.05, 2), 
             round(0.95 + stats::plogis(opt$par[n_k2 + 4] + stats::qnorm(0.975) * se[n_k2 + 4]) * 7.05, 2), 
             round(0.05 + stats::plogis(opt$par[n_k2 + 5] + stats::qnorm(0.975) * se[n_k2 + 5]) * (1.95 - 0.05), 2),
             round(stats::plogis(opt$par[n_k1 + 1] + stats::qnorm(0.975) * se[n_k1 + 1]) * 8, 2), 
             round(stats::plogis(opt$par[n_k2 - 1] + stats::qnorm(0.975) * se[n_k2 - 1]) * 8, 2),
             round(stats::plogis(opt$par[n_k2 + 6] + stats::qnorm(0.975) * se[n_k2 + 6]), 2), 
             round(0.1 + stats::plogis(opt$par[n_k2 + 7] + stats::qnorm(0.975) * se[n_k2 + 7]) * (6 - 0.1), 2), 
             round(0.1 + stats::plogis(opt$par[n_k2 + 8] + stats::qnorm(0.975) * se[n_k2 + 8]) * (6 - 0.1), 2),
             round(0.1 + stats::plogis(opt$par[n_k2 + 9] + stats::qnorm(0.975) * se[n_k2 + 9]) * (6 - 0.1), 2),
             round(0.1 + stats::plogis(opt$par[n_k2 + 10] + stats::qnorm(0.975) * se[n_k2 + 10]) * (6 - 0.1), 2), 
             round(0.25 + (stats::plogis(opt$par[n_k2 + 11] + stats::qnorm(0.975) * se[n_k2 + 11])) * (1.75 - 0.25), 2),
             round(0.05 + stats::plogis(opt$par[n_k3] + stats::qnorm(0.975) * se[n_k3]) * (0.75 - 0.05), 2),
             round(exp(-1 * stats::plogis(opt$par[n_k3 + 1] + stats::qnorm(0.975) * se[n_k3 + 1]) * (2) * 0.85), 2),
             round(1 + stats::plogis(opt$par[n_k3 + 2] + stats::qnorm(0.975) * se[n_k3 + 2]) * (10 - 1), 2))
     
  }
  if (!is.null(param)) {
    print("using param method")
    est <- rbind(stats::plogis(param[, n_k2 + 1]) * 10, 
                 stats::plogis(param[, n_k2 + 2]) * 10, 
                 0.95 + stats::plogis(param[,n_k2 + 3]) * 7.05,
                 0.95 + stats::plogis(param[,n_k2 + 4]) * 7.05,
                 0.05 + stats::plogis(param[,n_k2 + 5]) * (1.95 - 0.05),
                 stats::plogis(param[,n_k1 + 1]) * 8,
                 stats::plogis(param[,n_k2 - 1]) * 8, 
                 stats::plogis(param[, n_k2 + 6]), 
                 0.1 + stats::plogis(param[, n_k2 + 7]) * (6 - 0.1), 
                 0.1 + stats::plogis(param[, n_k2 + 8]) * (6 - 0.1), 
                 0.1 + stats::plogis(param[, n_k2 + 9]) * (6 - 0.1), 
                 0.1 + stats::plogis(param[, n_k2 + 10]) * (6 - 0.1), 
                 0.25 + (stats::plogis(param[, n_k2 + 11])) * (1.75 - 0.25),
                 0.05 + stats::plogis(opt$par[n_k3]) * (0.75 - 0.05),
                 exp(-1 * stats::plogis(opt$par[n_k3] + 1) * (2) * 0.85),
                 1 + stats::plogis(opt$par[n_k3 + 2]) * (20 - 1)
    )
    
    lci <- round(apply(est, 1, stats::quantile, probs = 0.025), 2)
    uci <- round(apply(est, 1, stats::quantile, probs = 0.975), 2)
  }
  rr_opt <- data.frame(name = description, estimate = pt, lci = lci, uci = uci)
  return(rr_opt)
}

graph_90s <- function(country_makesimul, cnt,
                      sex = "both", 
                      age = "15-49", year_start = 2000, year_end = 2030,
                      TitleT = TRUE, plot_type = "all",Legend = T,
                      true_0 = T) {
  mod <- country_makesimul$mod
  out_evertest <- country_makesimul$out_evertest
  fp <- country_makesimul$fp
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  simul = country_makesimul$simul
  
  out_aware <- subset(get_out_aware(mod, fp, age = age, sex = sex), year <= year_end)
  out_art <- subset(get_out_art(mod,age = age, fp, gender = sex), year >= 2000 & year <= year_end)
  out_evertest$year <- out_evertest$year + 0.5
  out_aware$year <- out_aware$year + 0.5
  out_art$year <- out_art$year + 0.5
  
  out_f <- subset(out_aware, agegr == age & outcome == 'aware' & sex == 'female')
  out_m <- subset(out_aware, agegr == age & outcome == 'aware' & sex == 'male')
  out_a <- subset(out_aware, agegr == age & outcome == 'aware' & sex == 'both')
  out_aware_val <- subset(out_aware, agegr == age & sex == sex)
  
  if(true_0){
    ylim2 = c(0,100)
  }else{
    ylim2 = c(min(out_art$value[out_art$year >= year_start])*95,100)
  }
  
  gender <- sex
  out_ever_all <- subset(out_evertest, agegr == age & outcome == 'evertest' & 
                           sex == gender & hivstatus == 'positive')
  #add CI's
  if(Legend == T){
    ggplot() +
      geom_line(out_art, mapping = aes(y = value * 100, x = year, color = "ART - coverage"), size = 1.1) +
      geom_line(out_aware_val, mapping = aes(y = value * 100, x = year, color = "PLHIV - aware"), size = 1.1) +
      geom_line(out_ever_all, mapping = aes(y = value * 100, x = year, color = "Evertested"), size = 1.1) +
      theme_minimal() +
      labs(title = paste(cnt,"PLHIV Ever Tested, aware, and on ART among ", sex),
           x = "year",
           y = "proportion") +
      xlim(year_start,year_end)+
      scale_color_manual(name = "legend",
                         breaks = c("Evertested","PLHIV - aware","ART - coverage"),
                         values = c("Evertested" = "orange2",
                                    "PLHIV - aware" = "tomato3", 
                                    "ART - coverage" = "deepskyblue3"))+
      ylim(ylim2)
  }else if(Legend == F){
    ggplot() +
      geom_line(out_art, mapping = aes(y = value * 100, x = year), color = "deepskyblue3", size = 1.1) +
      geom_line(out_aware_val, mapping = aes(y = value * 100, x = year), color = "tomato3", size = 1.1) +
      geom_line(out_ever_all, mapping = aes(y = value * 100, x = year), color = "orange2", size = 1.1) +
      theme_minimal() +
      labs(title = paste(cnt,"PLHIV Ever Tested, 1st 90, and ART Coverage"),
           x = "year",
           y = "proportion") +
      xlim(year_start,year_end)+
      ylim(ylim2)
  }
  
}

evertest = function (mod, fp, df, VERSION = "R") 
{
  if (VERSION == F) 
    val <- .Call(evertestC, mod, apply(attr(mod, "testnegpop"),c(1:3,5),sum), 
                 apply(attr(mod, "diagnpop"),c(1:3,5),sum), 
                 apply(attr(mod, "artpop"),c(1:4,6),sum), as.integer(df$haidx), 
                 as.integer(df$hagspan), as.integer(df$sidx), as.integer(df$hvidx), 
                 as.integer(df$yidx), as.integer(fp$ss$agfirst.idx), 
                 as.integer(fp$ss$h.ag.span))
  else {
    val <- numeric(length(df$haidx))
    for (i in seq_along(df$haidx)) {
      haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
      sidx <- if (df$sidx[i] == 0) {
        1:2
      } else df$sidx[i]
      paidx <- fp$ss$agfirst.idx[df$haidx[i]] + 1:sum(fp$ss$h.ag.span[haidx]) - 1
      tested <- 0
      pop <- 0
      if (df$hvidx[i] %in% c(0, 1)) {
        tested <- tested + sum(attr(mod, "testnegpop")[haidx, 
                                                       sidx, fp$ss$hivn.idx,, df$yidx[i]])
        pop <- pop + sum(mod[paidx, sidx, fp$ss$hivn.idx, 
                             df$yidx[i]])
      }
      if (df$hvidx[i] %in% c(0, 2)) {
        tested <- tested + 
          sum(attr(mod, "testnegpop")[haidx,sidx,fp$ss$hivp.idx, ,df$yidx[i]]) + 
          sum(attr(mod,"diagnpop")[, haidx, sidx, ,df$yidx[i]]) +
          sum(attr(mod, "artpop")[, , haidx, sidx, ,df$yidx[i]])
        pop <- pop + sum(mod[paidx, sidx, fp$ss$hivp.idx, df$yidx[i]])
      }
      val[i] <- tested/pop
    }
  }
  val
}


get_out_evertest = function (mod, fp, agegr = c("15-24", "25-34", "35-49", "15-49", 
                                                "15-64", "15+"), sex = c("both", "female", "male"), hivstatus = c("all", 
                                                                                                                  "negative", "positive")) 
{
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  out_evertest <- expand.grid(year = 2000:end_date, outcome = "evertest", 
                              agegr = agegr, sex = sex, hivstatus = hivstatus)
  out_evertest$value <- evertest(mod, fp, add_ss_indices(out_evertest, 
                                                         fp$ss))
  out_evertest
}

artcov <-  function (mod, age = "15-49", sex = "both") {
  
  
  sexid = switch(
    EXPR = sex,
    "male" = 1,
    "female" = 2,
    "both" = 1:2
  )
  ageid = switch(
    EXPR = age,
    "15-24" = 1:3,
    "25-34" = 4:5,
    "35-49" = 6:8,
    "15-49" = 1:8
  )
  
  n_art <- apply(attr(mod, "artpop")[, , ageid, sexid, , , drop = FALSE], 
                 6, sum)
  n_hiv <- colSums(attr(mod, "hivpop")[, ageid, sexid, , drop = FALSE], 
                   , 3)
  return(n_art/(n_hiv + n_art))
}

get_out_art <-  function (mod, fp, age = "15-49", gender = "both") 
{
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  out_artcov <- data.frame(year = 1970:end_date, outcome = "artcov", 
                           agegr = age, sex = gender, hivstatus = "positive", 
                           value = artcov(mod,age = age, sex = gender))
  out_artcov <- subset(out_artcov)
  out_artcov
}


diagnosed = function (mod, fp, df, VERSION = "R") 
{
  if (F) {
    val <- .Call(diagnosedC, mod, attr(mod, "diagnpop"), 
                 attr(mod, "artpop"), as.integer(df$haidx), as.integer(df$hagspan), 
                 as.integer(df$sidx), as.integer(df$yidx), as.integer(fp$ss$agfirst.idx), 
                 as.integer(fp$ss$h.ag.span))
  }
  else {
    val <- numeric(length(df$haidx))
    for (i in seq_along(df$haidx)) {
      haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
      sidx <- if (df$sidx[i] == 0) 
        1:2
      else df$sidx[i]
      paidx <- fp$ss$agfirst.idx[df$haidx[i]] + 1:sum(fp$ss$h.ag.span[haidx]) - 
        1
      diagnosed <- sum(attr(mod, "diagnpop")[, df$haidx[i] + 1:df$hagspan[i] - 1, sidx,, df$yidx[i]]) + 
        sum(attr(mod, "artpop")[, , df$haidx[i] + 1:df$hagspan[i] - 1, sidx,, df$yidx[i]])
      
      hivpop <- sum(mod[paidx, sidx, fp$ss$hivp.idx, df$yidx[i]])
      val[i] <- diagnosed/hivpop
    }
  }
  val
}

get_out_aware <- function (mod, fp, agegr = c("15-49", "15-64", "15+"), sex = c("both", 
                                                                                "female", "male")){
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  out_aware <- expand.grid(year = 2000:end_date, outcome = "aware", 
                           agegr = agegr, sex = sex, hivstatus = "positive")
  out_aware$value <- diagnosed(mod, fp, add_ss_indices(out_aware, 
                                                       fp$ss))
  out_aware
}

get_out_nbtest = function(mod,fp,sex = "both",age = "15-49",testtype = "vct",hivstatus = "all"){
  
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  out_nbtest <- expand.grid(year = 2000:end_date, outcome = "numbertests", 
                            agegrp = age, sex = sex, hivstatus = hivstatus,
                            modality = testtype)
  out_nbtest$hivstatus <- as.character(out_nbtest$hivstatus)
  out_nbtest$value <- total_tests_anc(mod, add_ss_indices(out_nbtest, 
                                                          fp$ss))
  out_nbtest
  
}

get_out_diagnosed = function(mod,fp,sex = "both",age = "15-49",testtype = "vct",hivstatus = "all"){
  
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  out_nbtest <- expand.grid(year = 2000:end_date, outcome = "numbertests", 
                            agegrp = age, sex = sex, hivstatus = hivstatus,
                            modality = testtype)
  out_nbtest$hivstatus <- as.character(out_nbtest$hivstatus)
  out_nbtest$value <- total_tests_anc(mod, add_ss_indices(out_nbtest, 
                                                          fp$ss))
  out_nbtest
  
}

diagnoses = function (mod, df) 
{
  val <- numeric(length(df$haidx))
  tt <- unique(na.omit(as.character(df$testtype)))
  
  if (any(tt %in% c("all", "vct+anc", "anc+vct")) || (all(tt %in% c("vct","anc")) && length(tt) == 2)) {
    anc <- 1:2
  } else if (all(tt == "vct")) {
    anc <- 1L
  } else if (all(tt == "anc")) {
    anc <- 2L
  } else {
    anc <- 3L
  }
  
  for (i in seq_along(df$haidx)) {
    haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
    sidx <- if (df$sidx[i] == 0) 
      1:2
    else df$sidx[i]
    paidx <- fp$ss$agfirst.idx[df$haidx[i]] + 1:sum(fp$ss$h.ag.span[haidx]) - 
      1
    diagnoses <- sum(attr(mod, "diagnoses")[, df$haidx[i] + 1:df$hagspan[i] - 1, sidx,anc, df$yidx[i]])
    
    val[i] <- diagnoses
  }
  
  val
}

get_out_diagnoses  = function (mod,fp,sex = "both",age = "15-49",testtype = "vct") 
{
  
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  out_diagnoses <- expand.grid(year = 2000:end_date, outcome = "Diagnoses", 
                            agegrp = age, sex = sex,hivstatus = "all",
                            testtype = testtype)
  out_diagnoses$value <- diagnoses(mod, df = add_ss_indices(out_diagnoses, 
                                                          fp$ss))
  out_diagnoses$hivstatus = NULL
  out_diagnoses
}

Undiagnosed = function (mod, fp, df, VERSION = "R") 
{
  val <- numeric(length(df$haidx))
  for (i in seq_along(df$haidx)) {
    haidx <- df$haidx[i] + 1:df$hagspan[i] - 1
    sidx <- if (df$sidx[i] == 0) 
      1:2
    else df$sidx[i]
    paidx <- fp$ss$agfirst.idx[df$haidx[i]] + 1:sum(fp$ss$h.ag.span[haidx]) - 
      1
    diagnosed <- sum(attr(mod, "diagnpop")[, df$haidx[i] + 1:df$hagspan[i] - 1, sidx,, df$yidx[i]]) + 
      sum(attr(mod, "artpop")[, , df$haidx[i] + 1:df$hagspan[i] - 1, sidx,, df$yidx[i]])
    
    hivpop <- sum(mod[paidx, sidx, fp$ss$hivp.idx, df$yidx[i]])
    val[i] <- hivpop - diagnosed
  }
  
  val
}




