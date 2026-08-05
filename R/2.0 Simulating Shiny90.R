
path_anc <- here::here("anc testing")

source(paste0(path_anc,"/1.0 simmod.R"))
source(paste0(path_anc,"/1.1 tot test out.R"))

#name of make country
path_name = "/data/make_country_presimul.rds"
make_country = readRDS(paste0(path_anc,path_name))

# extract the PMTCT information from the Spectrum files
pmtct_list <- readRDS(paste0(path_anc, "/data/pmtct_list_cnt.rds"))

# extract the HIV/demographic projections
hivdemo_proj_list <- readRDS(paste0(path_anc, "/data/hivdemo_proj_dt_cnt.rds"))


# adjust for specific country runs, REMOVE CHAD (needs adjusted prior and rr_m)
cntlist = names(make_country)[1]

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
        
        
        # select country specific pmtct
        pmtct <- pmtct_list[[cnt]]
        
        # select country specific demo proj
        hivdemo_proj <- hivdemo_proj_list[[cnt]]
        
        # Extract theta0 from first90 but add 2 more parameters
        data("theta0", package = "first90")
        theta1 = theta0
        theta1[1:52] = c(theta0,theta0[1:3])
        
        fp <- create_anc_param(theta = theta1, fp = fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj, verbose = T)
        
        likdat <- prepare_anc_likdat(dat_evertest = survey_hts, dat_prg = pgrm_dat, fp = fp)
        
        
        ll_hts_anc(theta = theta1, fp = fp, likdat = likdat, pmtct = pmtct, hivdemo_proj = hivdemo_proj,verboise = T)
        # preoptimization with Nelder-Mead to find BFGS start point(adjust based on country)
        
        opt <- optim(theta1,
                     ll_hts_anc,
                     fp = fp,
                     likdat = likdat,
                     pmtct = pmtct,
                     hivdemo_proj = hivdemo_proj,
                     method = "Nelder-Mead",
                     control = list(fnscale = -1, trace = 4, REPORT = 1, maxit=1000))

        
        theta1 = opt$par
        fp <- create_anc_param(theta = theta1, fp = fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj, verbose = T)
        likdat <- prepare_anc_likdat(dat_evertest = survey_hts, dat_prg = pgrm_dat, fp = fp)
        
        #using independent opt1 vs opt again here prevents some bugs
        opt1 <- optim(theta1,
                      ll_hts_anc,
                      fp = fp,
                      likdat = likdat,
                      pmtct = pmtct,
                      hivdemo_proj = hivdemo_proj,
                      method = "BFGS",
                      control = list(fnscale = -1, trace = 4, REPORT = 1, maxit = 550),
                      hessian = T)

        opt = opt1
        
        # save the opts incase the simulation crashes(very small compared to full simul files)
        # saveRDS(opt, paste0(here::here("outputs"), "/opt/opt_final/test_optimized_par_", cnt))
        
        # confirm reasonable values for opt
        optimized_par_anc(opt)
        
        ll_hts_anc(theta = opt$par, fp = fp, likdat = likdat, pmtct = pmtct, hivdemo_proj = hivdemo_proj,verboise = T)
        
        
        # fp and mod from optimized parameters
        fp <- create_anc_param(theta = opt$par, fp = fp, pmtct = pmtct, hivdemo_proj = hivdemo_proj)
        mod <- simmod_anc_t(fp)
        
        # identify credible intervals
        simul <- simul.test.anc(opt = opt, fp = fp, likdat = likdat, pmtct = pmtct,
                                hivdemo_proj = hivdemo_proj, sim = 3000)

        # save both the simul and the sample( useful for counterfactual analysis)
        samp = simul$samp
        simul = simul$simul

        # obtaining outputs and plotting them
        make_country[[cnt]]$mod <- mod
        make_country[[cnt]]$fp <- fp
        make_country[[cnt]]$simul <- simul
        make_country[[cnt]]$out_evertest <- get_out_evertest(mod, fp)
        make_country[[cnt]]$opt <- opt
        make_country[[cnt]]$samp <- samp
        
        
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

saveRDS(object = make_country, file = here::here('anc testing/data/make_country_simul.rds'))

#----CHAD----

# FOR RUNNING CHAD CHANGE THE rr_m IN 1.0 SIMMOD CREATE ANC PARAM FROM 
# 0.05 + stats::plogis(theta[(n_k2 + 1):(n_k2 + 2)]) * (10 - 0.05)
# TO 
# 0.05 + stats::plogis(theta[(n_k2 + 1):(n_k2 + 2)]) * (20 - 0.05)
# AND 
# CHANGE THE LIKLIHOOD PRIOR TO MATCH IN 1.0 SIMMOD LPRIOR
# FROM
# sum(stats::dnorm(x = theta[n_k2 + 1], mean = stats::qlogis(5 / 10), sd = 2, log = TRUE)) + 
# TO 
# sum(stats::dnorm(x = theta[n_k2 + 1], mean = stats::qlogis(5 / 20), sd = 2, log = TRUE)) + 

