source("anc testing/1.0 simmod.R")

# extract the PMTCT information from the Spectrum files
pmtct_list <- readRDS(paste0(path_anc, "/data/pmtct_list_cnt.rds"))

# extract the HIV/demographic projections
hivdemo_proj_list <-
  readRDS(paste0(path_anc, "/data/hivdemo_proj_dt_cnt.rds"))

path_out <- here::here("outputs/Paper 2026/volume decline/")


# 
# # anc rate (incorrect, for ASFR decline comparison) ----
# rate_anc_INCORRECT = matrix(data = NA,nrow = 37,ncol = 10)
# rate_anc_INCORRECT = as.data.frame(rate_anc_INCORRECT)
# names(rate_anc_INCORRECT) = c("country",2016:2023)
# 
# rate_anc_ASFR = rate_anc_INCORRECT
# fp = make_country[[1]]$fp
# 
# # find group indices( convert mod total pop age spans *66* to HTS rate age spans *9*)
# group_indices <- rep(seq_along(fp$ss$h.ag.span[1:9]), fp$ss$h.ag.span[1:9])  # Expands to match asfr rows
# group_indices_asfr <- rep(seq_along(fp$ss$h.ag.span[1:8]), fp$ss$h.ag.span[1:8])  # Expands to match asfr rows
# 
# 
# countryies = list()
# cntlist=  names(make_country)[c(1:3,8,10:12,15,16,19,20,26:37)]
# for(i in 1:length(make_country)){
#   cnt = names(make_country)[i]
#   print(cnt)
#   if(!(cnt %in% highlight_countries)){
#       next
#   }
#   fp = make_country[[cnt]]$fp
#   out_test <- expand.grid(year = 2016:2025, outcome = "numbertests",
#                           agegrp = "15+", sex = "both", hivstatus = "all",modality = "anc")
#   out_asfr <- expand.grid(year = 2016:2023, outcome = "numbertests",
#                           agegrp = "15+", sex = "both", hivstatus = "all",modality = "anc")
# 
#   years1 = counter_years_anc[counter_years_anc$country == cnt,2:3]
#   if(is.na(years1$start)){
#     years1$start = 8
#     years1$end = 8
#   }else{
#     years1$start = years1$start - 46
#     if(is.na(years1$end)){
#       years1$end = 8
# 
#     }else{
#       years1$end = years1$end - 46 - 1
# 
#     }
# 
#   }
#   grid_anc = matrix(nrow = 3000,ncol = 10)
#   for(n in 1:nrow(make_country[[cnt]]$samp)){
# 
#     fp_local = create_anc_param(
#       make_country[[cnt]]$samp[n,],
#       fp = make_country[[cnt]]$fp,
#       pmtct = pmtct_list[[cnt]],
#       hivdemo_proj = hivdemo_proj_list[[cnt]]
#     )
#     mod_local = simmod_anc_t(fp = fp_local)
# 
#     #find negative weighted testing
#     pop <- rowsum(mod_local[,2,1,47:54], group_indices)
# 
# 
#     tested_at_ANC = apply(attr(mod_local,"testnegpop")[,2,1:2,2,47:54,drop = F],c(1,5),sum)
# 
#     naive <- colSums((pop - tested_at_ANC)/pop*fp_local$anc_rate[,2,1,47:54])
#     testnegpop <- colSums((tested_at_ANC)/pop*fp_local$anc_rate[,2,2,47:54])
#     grid_anc[n,1:8] = naive+testnegpop
#     grid_anc[n,9] = (naive+testnegpop)[years1$end] / (naive+testnegpop)[years1$start]
# 
#     # Compute grouped means efficiently using rowsum()
#     asfr <-
#       rowsum(fp$asfr, group_indices_asfr) / as.numeric(table(group_indices_asfr))
# 
#     # Append zero row
#     asfr <-
#       rbind(asfr, rep(0, ncol(asfr)))  # Ensuring correct dimensions
# 
# 
# 
#     asfrdiff = (colSums(pop / sum(pop) * asfr[, 47:54])[years1$end]/colSums(pop / sum(pop) * asfr[, 47:54])[years1$start])
#     anc_diff = (naive+testnegpop)[years1$end] / (naive+testnegpop)[years1$start]
# 
#     # find proportion of ANC decline due to ASFR (i.e. ANC decline  = ASFR decline + actual decline)
#     grid_anc[n,10] = 1 - (anc_diff - asfrdiff)
# 
#     }
# 
#   anc_med = apply(grid_anc, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
# 
#   pop <- rowsum(make_country[[cnt]]$mod[,2,1,47:54], group_indices)
# 
# 
#   out_test$value[1:10] = anc_med[2,]
#   out_test$lci[1:10] = anc_med[1,]
#   out_test$uci[1:10] = anc_med[3,]
# 
# 
#   out_asfr$value[1:8] = colSums(pop / sum(pop) * asfr[, 47:54])
# 
#   countryies[[cnt]]$out_test = out_test
#   countryies[[cnt]]$out_asfr = out_asfr
#   countryies[[cnt]]$grid = grid_anc
# 
# 
#   }
# 
# create graph ----
# saveRDS(countryies,paste0(path_anc,"attribution tables.rds"))
# 
# for(i in 1:length(make_country)){
#   cnt = names(make_country)[i]
#   print(cnt)
#   if(!(cnt %in% cntlist)){
#     next
#   }
#   countries[[cnt]]$out_test[9,7:9] - countries[[cnt]]$out_test[10,7:9]*countries[[cnt]]$out_test[9,7:9]


countries = readRDS("anc testingattribution tables.rds")

# taken from 2.1 counterfactual creation 
highlight_countries = counter_years$country[!is.na(counter_years$start)]


# maximum declines ----

country_anc = data.frame(
  countries = names(countries),
  anc_diff = rep(NA,30),
  anc_difflci = rep(NA,30),
  anc_diffuci = rep(NA,30),
  attribution = rep(NA,30),
  attributionlci = rep(NA,30),
  attributionuci = rep(NA,30)
)

group_indices_anc <- rep(seq_along(fp$ss$h.ag.span[1:9]), fp$ss$h.ag.span[1:9])  # Expands to match asfr rows
group_indices_asfr <- rep(seq_along(fp$ss$h.ag.span[1:8]), fp$ss$h.ag.span[1:8])  # Expands to match asfr rows

for(i in 1:length(countries)){
    cnt = names(countries)[i]
    print(cnt)
    
    years1 = counter_years_anc[counter_years_anc$country == cnt, 2:3]
    
    if (is.na(years1$start)) {
      years1$start = 8
      years1$end = 8
    } else{
      years1$start = years1$start - 46
      if (is.na(years1$end)) {
        years1$end = 8
        
      } else{
        # 1 less to capture last year before decline
        years1$end = years1$end - 46 - 1
        
      }
    }
    asfr = hivdemo_proj_list[[cnt]]$asfr
    fp = make_country[[cnt]]$fp
    mod = make_country[[cnt]]$mod
    hivdemo_proj_list[[cnt]]$tot_births
    
    #find ASFR [ages 15-80,female,hivneg/pos,2016-2023]
    pop <- rowsum(apply(make_country[[cnt]]$mod[,2,,47:54,drop = F],c(1,4),sum), group_indices_anc)

    
    # Compute grouped means efficiently using rowsum()
    asfr2 <- rowsum(asfr, group_indices_asfr) / as.numeric(table(group_indices_asfr))
    # Append zero row
    asfr1 <- rbind(asfr2, rep(0, ncol(asfr2)))  # Ensuring correct dimensions
    
    w <- (pop)/colSums(pop) 
    
    # weighted ASFR for each year 2016–2023 (returns length-8 vector)
    asfr_adj_2016_2023 <- colSums((w) * asfr1[, 47:54])
    
    
    grid = countries[[cnt]]$grid
    grid2 = matrix(data = NA,nrow = 3000,ncol = 2)
    
    for(n in 1:nrow(make_country[[cnt]]$samp)){
      
      anc_diff = grid[n,]
      
      anc_value = min(anc_diff[(years1$start+1):years1$end] / anc_diff[(years1$start+1)])
      if(anc_value<1){
        anc_idx = which.min(anc_diff[(years1$start+1):years1$end] / anc_diff[(years1$start+1)])
      } else if(anc_value >= 1){
        anc_idx <- 8
        anc_value <- 1
      }
      
      asfrdiff = min(asfr_adj_2016_2023[anc_idx]/asfr_adj_2016_2023[years1$start],1)
        
      
      # find proportion of ANC decline due to ASFR (i.e. ANC decline  = ASFR decline + actual decline)
      grid2[n,1] = 1 - max((1 - anc_value) - (1 - asfrdiff),0)/max(1 - anc_value,1e-16)
      
      grid2[n,2] = max(1 - anc_value,0)
      
      }
    country_anc[i,1] = cnt
    country_anc[i,5:7] = quantile(grid2[,1],c(0.5,0.025,0.975))
    country_anc[i,2:4] = quantile(grid2[,2],c(0.5,0.025,0.975))
}




country_anc$countries[country_anc$countries == "Democratic Republic of the Congo"] = "DRC"
country_anc$countries[country_anc$countries == "United Republic of Tanzania"] = "Tanzania"

summary(country_anc[country_anc$countries %in% declines_anc,])

country_anc <- country_anc[country_anc$countries %in% declines_anc,] %>%
  pivot_longer(
    cols = -countries,
    names_to = c("metric", "stat"),
    names_pattern = "^(.*?)(lci|uci)?$",
    values_to = "value"
  ) %>%
  mutate(stat = if_else(stat == "", "est", stat)) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  select(countries, metric, est, lci, uci)


attribution_graph = ggplot(country_anc) +
  geom_point(aes(
    x = as.factor(countries),
    y = est*100,
    group = metric,
    colour = metric
    ), size = 3)+
  geom_errorbar(aes(
    x = as.factor(countries),
    ymin = lci*100,
    ymax = uci*100,
    group = metric,
    colour = metric
  ))+
  theme_minimal()+
  labs(
    title = "Percent Maximum Decline in ANC\nand the Attribution of ANC Declines to age weighted ASFR",
    x = NULL,
    y = "% Percent Decline/Attribution"
  )+
  scale_colour_manual(
    name = "Metric",
    labels = c(
      "anc_diff" = "ANC declines",
      "attribution" = "Atrribution of\ndecline to ASFR"
    ),
    values = c(
      "anc_diff" = "firebrick",
      "attribution" = "steelblue"
    )
  )+
  theme(
    axis.text.x = element_text(
      angle = 45,vjust = 1,hjust=1
    )
  )
  

svglite::svglite(fn <- file.path(file = paste0(path_out, "/attribution graph maximum.svg")),
                 width = 8.5, height = 8.5)
print(attribution_graph)
dev.off()


summary(diff)






# total declines ----

country_anc = data.frame(
  countries = names(countries),
  anc_diff = rep(NA,30),
  anc_difflci = rep(NA,30),
  anc_diffuci = rep(NA,30),
  attribution = rep(NA,30),
  attributionlci = rep(NA,30),
  attributionuci = rep(NA,30)
)

for(i in 1:length(countries)){
  cnt = names(countries)[i]
  print(cnt)
  
  years1 = counter_years_anc[counter_years_anc$country == cnt, 2:3]
  if (is.na(years1$start)) {
    years1$start = 8
    years1$end = 8
  } else{
    years1$start = years1$start - 46
    if (is.na(years1$end)) {
      years1$end = 8
      
    } else{
      # 1 less to capture last year before decline
      years1$end = years1$end - 46 - 1
      
    }
  }
  asfr = countries[[cnt]]$out_asfr
  grid = countries[[cnt]]$grid
  grid2 = matrix(data = NA,nrow = 3000,ncol = 2)
  
  print(asfr)
  
  for(n in 1:nrow(make_country[[cnt]]$samp)){
    
    anc_diff = grid[n,]
    
    anc_value = min(anc_diff[(years1$start+1):years1$end] / anc_diff[(years1$start+1)])
    
    asfrdiff = min(asfr$value[years1$end]/asfr$value[years1$start],1)
    
    
    # find proportion of ANC decline due to ASFR (i.e. ANC decline  = ASFR decline + actual decline)
    grid2[n,1] = 1 - max((1 - anc_value) - (1 - asfrdiff),0)/max(1 - anc_value,1e-16)
    
    grid2[n,2] = max(1 - anc_value,0)
    
  }
  country_anc[i,1] = cnt
  country_anc[i,5:7] = quantile(grid2[,1],c(0.5,0.025,0.975))
  country_anc[i,2:4] = quantile(grid2[,2],c(0.5,0.025,0.975))
}




country_anc$countries[country_anc$countries == "Democratic Republic of the Congo"] = "DRC"
country_anc$countries[country_anc$countries == "United Republic of Tanzania"] = "Tanzania"

summary(country_anc[country_anc$metric == "attribution",])

country_anc <- country_anc %>%
  pivot_longer(
    cols = -countries,
    names_to = c("metric", "stat"),
    names_pattern = "^(.*?)(lci|uci)?$",
    values_to = "value"
  ) %>%
  mutate(stat = if_else(stat == "", "est", stat)) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  select(countries, metric, est, lci, uci)


attribution_graph = ggplot(country_anc) +
  geom_point(aes(
    x = as.factor(countries),
    y = est*100,
    group = metric,
    colour = metric
  ), size = 3)+
  geom_errorbar(aes(
    x = as.factor(countries),
    ymin = lci*100,
    ymax = uci*100,
    group = metric,
    colour = metric
  ))+
  theme_minimal()+
  labs(
    title = "Percent Maximum Decline in ANC\nand the Attribution of ANC Declines to ASFR",
    x = NULL,
    y = "% Percent Decline/Attribution"
  )+
  scale_colour_manual(
    name = "Metric",
    labels = c(
      "anc_diff" = "ANC declines",
      "attribution" = "Atrribution of\ndecline to ASFR"
    ),
    values = c(
      "anc_diff" = "firebrick",
      "attribution" = "steelblue"
    )
  )+
  theme(
    axis.text.x = element_text(
      angle = 45,vjust = 1,hjust=1
    )
  )


svglite::svglite(fn <- file.path(file = paste0(path_out, "/attribution graph total.svg")),
                 width = 8.5, height = 11)
print(attribution_graph)
dev.off()





