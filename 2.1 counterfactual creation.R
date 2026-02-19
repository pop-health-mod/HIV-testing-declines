library(dplyr)


Counter_years_fun <- function(make_country){
  # for usage of fp based parameters ie h.ag.span(identical across countries)
  fp = make_country[[1]]$fp
  
  counter_years = data.frame(
    country = rep(NA,37),
    start = rep(NA,37),
    end = rep(NA,37),
    value_max = rep(NA,37),
    value_min = rep(NA,37)
    
    
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
    res_year = NA
    if(length(maxvalue)>=1 && !is.na(all(maxvalue))){
      
      min_year = res_first_x - 1970+1
      
      idx_max <- switch(as.character(length(unique(maxvalue))),
                  "1" = 1,
                  "2" = 2,
                  2)   # default for >2 (and also 0)
      res_year = out_test[out_test$value_order == (unique(maxvalue))[idx_max],]$year 
      max_year = res_year - 1970+1
      if(max_year <= min_year){
        max_year = NA
        res_year = 2023+1
      }
      
      
    }else{
      min_year = NA
      max_year = NA
    }
    
    counter_years[i,1] = make_country[[cnt]]$cnt
    counter_years[i,2] = min_year
    counter_years[i,3] = max_year
    counter_years[i,4] = out_test$value[out_test$year == res_first_x][1]
    counter_years[i,5] = out_test$value[out_test$year == res_year-1][1]
    
  
  }
  return(counter_years)
}

counter_years = Counter_years_fun(make_country)

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

# identify size of decline
human_counter_years$effect = human_counter_years$value_min/human_counter_years$value_max
human_counter_years[,c(1:3,6)]

# save counter factual
saveRDS(
  object = counter_years,
  file = here::here('anc testing/data/counter_years.rds')
)


# # anc rate (incorrect, for ASFR decline comparison) ----
# rate_anc_INCORRECT = data.frame(
#   country = rep(NA,37),
#   start = rep(NA,37),
#   end = rep(NA,37),
#   value_max = rep(NA,37),
#   value_min = rep(NA,37)
#   
# )
# 
# # find group indices( convert mod total pop age spans *66* to HTS rate age spans *9*)
# group_indices <- rep(seq_along(fp$ss$h.ag.span[1:9]), fp$ss$h.ag.span[1:9])  # Expands to match asfr rows
# 
# for(i in 1:length(make_country)){
#   cnt = names(make_country)[i]
#   print(cnt)
#   out_test <- expand.grid(year = 2016:2023, outcome = "numbertests", 
#                           agegrp = "15+", sex = "both", hivstatus = "all",modality = "anc")
#   for (j in 47:54) {
#     
#     #find negative weighted testing 
#     
#     pop <- rowsum(make_country[[cnt]]$mod[,1:2,1,j], group_indices) 
#     naive <- (pop - apply(attr(make_country[[cnt]]$mod,"testnegpop")[,,1,2,j],1:2,sum))/pop*make_country[[cnt]]$fp$anc_rate[,,1,j]
#     testnegpop <- (apply(attr(make_country[[cnt]]$mod,"testnegpop")[,,1,2,j],1:2,sum))/pop*make_country[[cnt]]$fp$anc_rate[,,2,j]
#     
#     out_test$value[j-46] = sum(naive + testnegpop)
#     
#   }
#   
#   out_test = out_test %>% 
#     arrange(value) %>% 
#     mutate(value_order = 1:dim(out_test)[1]) %>% 
#     arrange(year) 
#   
#   res_first_x <- out_test %>%
#     left_join(
#       out_test %>% transmute(year = year - 1, vo_x1 = value_order),
#       by = "year"
#     ) %>%
#     left_join(
#       out_test %>% transmute(year = year - 2, vo_x2 = value_order),
#       by = "year"
#     ) %>%
#     mutate(hit = !is.na(vo_x1) & !is.na(vo_x2) &
#              vo_x1 < value_order & vo_x2 < value_order) %>%
#     filter(hit) %>%
#     slice_min(year, with_ties = FALSE) %>% 
#     select(year) %>% 
#     as.numeric()
#   
#   
#   maxvalue = (pmax(out_test[out_test$year > res_first_x,]$value_order,out_test[out_test$year == res_first_x,]$value_order))
#   if(length(maxvalue)>=1 && !is.na(all(maxvalue))){
#     
#     min_year = res_first_x - 1970+1
#     
#     idx_max <- switch(as.character(length(unique(maxvalue))),
#                       "1" = 1,
#                       "2" = 2,
#                       2)   # default for >2 (and also 0)
#     res_year = out_test[out_test$value_order == (unique(maxvalue))[idx_max],]$year 
#     max_year = res_year - 1970+1
#     if(max_year <= min_year){
#       max_year = NA
#       res_year = 2023+1
#     }
#     
#     
#   }else{
#     min_year = NA
#     max_year = NA
#   }
#   
#   rate_anc_INCORRECT[i,1] = make_country[[cnt]]$cnt
#   rate_anc_INCORRECT[i,2] = min_year
#   rate_anc_INCORRECT[i,3] = max_year
#   rate_anc_INCORRECT[i,4] = out_test$value[out_test$year == res_first_x][1]
#   rate_anc_INCORRECT[i,5] = out_test$value[out_test$year == res_year-1][1]
#   
# }
# 
# # convert from indexes to years for easier confirmation/analysis
# human_rate_anc_INCORRECT = rate_anc_INCORRECT %>% 
#   mutate(
#     end = end + 1970 - 1,
#     start = start + 1970 - 1
#   ) %>% 
#   arrange(start)
# # identify years when declines start and end
# table(human_rate_anc_INCORRECT$end)
# table(human_rate_anc_INCORRECT$start)
# 
# human_rate_anc_INCORRECT$effect = human_rate_anc_INCORRECT$value_min/human_rate_anc_INCORRECT$value_max
# 
# 
# human_rate_anc_INCORRECT[,c(1,2,3,6)]
# 
# anc counterfactual ----

counter_years_anc = data.frame(
  country = rep(NA,37),
  start = rep(NA,37),
  end = rep(NA,37),
  value_max = rep(NA,37),
  value_min = rep(NA,37)

)

for(i in 1:length(make_country)){
  cnt = names(make_country)[i]
  print(cnt)
  out_test <- expand.grid(year = 2016:2023, outcome = "numbertests",
                          agegrp = "15+", sex = "both", hivstatus = "all",modality = "anc")

  out_test$value = make_country[[cnt]]$fp$subvar$anc_tests_prob[as.character(2016:2023)]

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
  res_year  = NA
  if(length(maxvalue)>=1 && !is.na(all(maxvalue))){

    min_year = res_first_x - 1970+1

    idx_max <- switch(as.character(length(unique(maxvalue))),
                      "1" = 1,
                      "2" = 2,
                      2)   # default for >2 (and also 0)
    res_year = out_test[out_test$value_order == (unique(maxvalue))[idx_max],]$year
    max_year = res_year - 1970+1
    if(max_year <= min_year){
      max_year = NA
      res_year = 2023+1
    }


  }else{
    min_year = NA
    max_year = NA
  }

  counter_years_anc[i,1] = make_country[[cnt]]$cnt
  counter_years_anc[i,2] = min_year
  counter_years_anc[i,3] = max_year
  counter_years_anc[i,4] = out_test$value[out_test$year == res_first_x][1]
  counter_years_anc[i,5] = out_test$value[out_test$year == res_year-1][1]

}

# convert from indexes to years for easier confirmation/analysis
human_counter_years_ANC = counter_years_anc %>%
  mutate(
    end = end + 1970 - 1,
    start = start + 1970 - 1
  ) %>%
  arrange(start)

# identify years when declines start and end
table(human_counter_years_ANC$end)
table(human_counter_years_ANC$start)

human_counter_years_ANC$effect = human_counter_years_ANC$value_min/human_counter_years_ANC$value_max


human_counter_years_ANC[,c(1,2,3,6)]


saveRDS(
  object = counter_years,
  file = here::here('anc testing/data/counter_anc_years.rds')
)


# identify combined declines
x = unique(c(human_counter_years_ANC$country[!is.na(human_counter_years_ANC$start)],human_counter_years$country[!is.na(human_counter_years$start)]))

#find countries only in ANC that are not in vct
x[!(x %in% human_counter_years$country[!is.na(human_counter_years$start)])]

#find fully missing countries for both anc and vct
human_counter_years$country[!(human_counter_years$country %in% x)]

human_both = merge(human_counter_years[,1:3],human_counter_years_ANC[,1:3],by = "country")
declines_anc = human_both[(!(is.na(human_both$start.y)|is.na(human_both$start.x))),]$country
