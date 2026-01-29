library(dplyr)

# for usage of fp based parameters ie h.ag.span(identical across countries)
fp = make_country[[1]]$fp

counter_years = data.frame(
  country = rep(NA,37),
  start = rep(NA,37),
  end = rep(NA,37)
  
)

# find group indices( convert mod total pop age spans *66* to HTS rate age spans *9*)
group_indices <- rep(seq_along(fp$ss$h.ag.span[1:9]), fp$ss$h.ag.span[1:9])  # Expands to match asfr rows

for(i in 1:length(make_country)){
  cnt = names(make_country)[i]
  print(cnt)
  out_test <- expand.grid(year = 2016:2023, outcome = "numbertests", 
                          agegrp = "15+", sex = "both", hivstatus = "all",modality = "vct")
  for (j in 47:54) {
    
    #find negative weighted testing 
    
    pop <- rowsum(make_country[[cnt]]$mod[,1:2,1,j], group_indices) 
    naive <- (pop - apply(attr(make_country[[cnt]]$mod,"testnegpop")[,,1,,j],1:2,sum))/pop*make_country[[cnt]]$fp$hts_rate[,,1,j]
    testnegpop <- (apply(attr(make_country[[cnt]]$mod,"testnegpop")[,,1,,j],1:2,sum))/pop*make_country[[cnt]]$fp$hts_rate[,,2,j]
    
    out_test$value[j-46] = sum(naive + testnegpop)
    
    }
  
  out_test = out_test %>% 
    arrange(value) %>% 
    mutate(value_order = 1:dim(out_test)[1]) %>% 
    arrange(year) 
  
  res_first_x <- out_test %>%
    left_join(
      out_test %>% transmute(year = year - 1, vo_x1 = value_order),
      by = "year"
    ) %>%
    left_join(
      out_test %>% transmute(year = year - 2, vo_x2 = value_order),
      by = "year"
    ) %>%
    mutate(hit = !is.na(vo_x1) & !is.na(vo_x2) &
             vo_x1 < value_order & vo_x2 < value_order) %>%
    filter(hit) %>%
    slice_min(year, with_ties = FALSE) %>% 
    select(year) %>% 
    as.numeric()
  
  
  maxvalue = (pmax(out_test[out_test$year > res_first_x,]$value_order,out_test[out_test$year == res_first_x,]$value_order))
  if(length(maxvalue)>=1 && !is.na(all(maxvalue))){
    
    min_year = res_first_x - 1970+1
    
    idx_max <- switch(as.character(length(unique(maxvalue))),
                "1" = 1,
                "2" = 2,
                2)   # default for >2 (and also 0)
    max_year = out_test[out_test$value_order == (unique(maxvalue))[idx_max],]$year - 1970+1
    if(max_year <= min_year){
      max_year = NA
    }
    
    
  }else{
    min_year = NA
    max_year = NA
  }
  
  counter_years[i,1] = make_country[[cnt]]$cnt
  counter_years[i,2] = min_year
  counter_years[i,3] = max_year

}

# convert from indexes to years for easier confirmation/analysis
human_counter_years = counter_years %>% 
  mutate(
    end = end + 1970 - 1,
    start = start + 1970 - 1
  ) %>% 
  arrange(start)
# identify years when declines start and end
table(human_counter_years$end)
table(human_counter_years$start)

# save counter factual
saveRDS(
  object = counter_years,
  file = here::here('anc testing/data/counter_years.rds')
)

