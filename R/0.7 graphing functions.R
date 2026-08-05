survey_fit_all <- function(make_country, cnt) {
  ## survey fits tot----
  
  panel_survey_theme <- theme(
    plot.title.position = "panel",
    plot.title = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(0, 0, 0, 0),
    axis.title.y = element_text(
      #margin = margin(r = 3),
      size = 12
    ),
    
  )
  
  hivstatus = "all"
  
  # female all
  a = graph_evertest(
    make_country[[cnt]],
    cnt = cnt,
    dat_org = make_country[[cnt]]$survey_hts,
    sex = "female",
    hivstatus = hivstatus,
    age = "15-24",
    year_start = 2000,
    year_end = 2023,
    TitleT = F,
    axisx = F,
    axisx_text = F
  )
  # female all
  b = graph_evertest(
    make_country[[cnt]],
    cnt = cnt,
    dat_org = make_country[[cnt]]$survey_hts,
    sex = "female",
    hivstatus = hivstatus,
    age = "25-34",
    year_start = 2000,
    year_end = 2023,
    TitleT = F,
    axisy = F,
    axisx = F,
    axisx_text = F,
    axisy_text = F
  )
  # female all
  c = graph_evertest(
    make_country[[cnt]],
    cnt = cnt,
    dat_org = make_country[[cnt]]$survey_hts,
    sex = "female",
    hivstatus = hivstatus,
    age = "35-49",
    year_start = 2000,
    year_end = 2023,
    TitleT = F,
    axisy = F,
    axisx = F,
    axisx_text = F,
    axisy_text = F
  )
  
  
  # male all
  am = graph_evertest(
    make_country[[cnt]],
    cnt = cnt,
    dat_org = make_country[[cnt]]$survey_hts,
    sex = "male",
    hivstatus = hivstatus,
    age = "15-24",
    year_start = 2000,
    year_end = 2023,
    TitleT = F,
    axis_angle = 315
  )
  # male all
  bm = graph_evertest(
    make_country[[cnt]],
    cnt = cnt,
    dat_org = make_country[[cnt]]$survey_hts,
    sex = "male",
    hivstatus = hivstatus,
    age = "25-34",
    year_start = 2000,
    year_end = 2023,
    TitleT = F,
    axisy = F,
    axisy_text = F,
    axis_angle = 315
  )
  # male all
  cm = graph_evertest(
    make_country[[cnt]],
    cnt = cnt,
    dat_org = make_country[[cnt]]$survey_hts,
    sex = "male",
    hivstatus = hivstatus,
    age = "35-49",
    year_start = 2000,
    year_end = 2023,
    TitleT = F,
    axisy = F,
    axisy_text = F,
    axis_angle = 315
  )
  
  
  a  <- a  + ggtitle("age: 15–24") + labs(y = "Female\nProportion Evertested") + panel_survey_theme
  b  <- b  + ggtitle("age: 25–34") + labs(y = NULL)     + panel_survey_theme
  c  <- c  + ggtitle("age: 35–49") + labs(y = NULL)     + panel_survey_theme
  
  am <- am + labs(y = "Male\nProportion Evertested")  + panel_survey_theme
  bm <- bm + labs(y = NULL)    + panel_survey_theme
  cm <- cm + labs(y = NULL)    + panel_survey_theme
  
  survey_panels <- (a + b + c) / (am + bm + cm)  +
    plot_layout(heights = c(1, 1))
  
  
  survey_title = ggplot() +
    theme_void() +
    labs(title = paste0("Proportion Evertested 2000 to 2023\nby age: ", cnt)) +
    theme(
      plot.title = element_text(
        hjust = 0,
        size = 12,
        face = "bold"
      ),
      plot.margin = margin(0, 0, 0, 0)  # small gap below the title
    )
  
  survey_plot  <- survey_title / survey_panels + plot_layout(heights = c(0.01, 1))
  
  survey_plot
}


survey_fit_postive <- function(make_country,cnt){
  
  panel_survey_theme <- theme(
    plot.title.position = "panel",
    plot.title = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(0, 0, 0, 0),
    axis.title.y = element_text(
      #margin = margin(r = 3),
      size = 12
    ),
    
  )
  ## survey fits totpos----
  
  # female hiv+
  a1 = graph_evertest(make_country[[cnt]], cnt = cnt, dat_org = make_country[[cnt]]$survey_hts,
                      sex = "female", hivstatus = "positive", 
                      age = "15-24", year_start = 2000, year_end = 2023,
                      TitleT = F,axisx = F,axisx_text = F)
  # female hiv+
  b1 = graph_evertest(make_country[[cnt]], cnt = cnt, dat_org = make_country[[cnt]]$survey_hts,
                      sex = "female", hivstatus = "positive", 
                      age = "25-34", year_start = 2000, year_end = 2023,
                      TitleT = F, axisy = F,axisx = F,axisx_text = F,axisy_text = F)   
  
  # female hiv+
  c1 = graph_evertest(make_country[[cnt]], cnt = cnt, dat_org = make_country[[cnt]]$survey_hts,
                      sex = "female", hivstatus = "positive", 
                      age = "35-49", year_start = 2000, year_end = 2023,TitleT = F,
                      axisy = F,axisx = F,axisx_text = F,axisy_text = F)   
  
  
  # male hiv+
  a1m = graph_evertest(make_country[[cnt]], cnt = cnt, dat_org = make_country[[cnt]]$survey_hts,
                       sex = "male", hivstatus = "positive", 
                       age = "15-24", year_start = 2000, year_end = 2023,TitleT = F,axis_angle = 315)
  # male hiv+
  b1m = graph_evertest(make_country[[cnt]], cnt = cnt, dat_org = make_country[[cnt]]$survey_hts,
                       sex = "male", hivstatus = "positive", 
                       age = "25-34", year_start = 2000, year_end = 2023,
                       TitleT = F,axisy = F,axisy_text = F,axis_angle = 315)   
  
  # male hiv+
  c1m = graph_evertest(make_country[[cnt]], cnt = cnt, dat_org = make_country[[cnt]]$survey_hts,
                       sex = "male", hivstatus = "positive", 
                       age = "35-49", year_start = 2000, year_end = 2023,
                       TitleT = F,axisy = F,axisy_text = F,axis_angle = 315)   
  
  
  
  a1  <- a1  + ggtitle("age: 15–24") + labs(y = "Female\nProportion Evertested") + panel_survey_theme
  b1  <- b1  + ggtitle("age: 25–34") + labs(y = NULL)     + panel_survey_theme
  c1  <- c1  + ggtitle("age: 35–49") + labs(y = NULL)     + panel_survey_theme
  
  a1m <- a1m + labs(y = "Male\nProportion Evertested") + panel_survey_theme
  b1m <- b1m + labs(y = NULL)   + panel_survey_theme
  c1m <- c1m + labs(y = NULL)   + panel_survey_theme
  
  survey_panels_pos <- (a1 + b1 + c1) / (a1m + b1m + c1m) + 
    plot_layout(heights = c(1, 1))
  
  survey_title_pos = ggplot() +
    theme_void() +
    labs(title = paste0("Proportion Positive Evertested 2000 to 2023\nby age: ", cnt)) +
    theme(
      plot.title = element_text(hjust = 0, size = 12,face = "bold"),
      plot.margin = margin(0, 0, 0, 0)  # small gap below the title
    )
  survey_plot_pos  <- survey_title_pos / survey_panels_pos + plot_layout(heights = c(0.01, 1))
  
  
  survey_plot_pos
  
  
}


counterfactual_ttd <- function(make_country, cnt){
  
  ## ttd----
  malettd = Agg_simul_pool_time_dx_prev(out_simul_tdx_all = make_country[[cnt]]$tdx_male$out_simul_tdx_all,year = 2015:2023,sex = "male")
  femalettd = Agg_simul_pool_time_dx_prev(out_simul_tdx_all = make_country[[cnt]]$tdx_female$out_simul_tdx_all,year = 2015:2023,sex = "female")
  
  
  
  col = "firebrick3"
  col2 = "steelblue"
  
  
  
  if(is.null(make_country[[cnt]]$tdx_male_counter)){
    
    maxttm = max(c(malettd$time_dx_uci))
    
    plotttm = ggplot() +
      geom_ribbon(data = malettd,
                  aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci,group = 1),
                  fill = col2,
                  alpha = 0.3) +
      geom_line(data = malettd,aes(y = time_dx, x = year), color = col2,linewidth = 1)+
      theme_minimal() +
      labs(title = paste0("Median Time to Diagnosis or AIDS Death\namong Men in ",cnt," between ",2015," and ",2023), 
           x = "Year", 
           y = "Median Years to Diagnosis or AIDS Death")+
      scale_x_continuous(breaks = c(seq(2015, 2023, 1),2023),
                         limits = c(2015, 2023))+
      scale_y_continuous(limits = c(0,maxttm))+
      theme(
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
      )
    
    maxttf = max(c(femalettd$time_dx_uci))
    
    plotttf = ggplot() +
      geom_ribbon(data = femalettd,
                  aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci,group = 1),
                  fill = col,
                  alpha = 0.2)+
      geom_line(data = femalettd,aes(y = time_dx, x = year), color = col,linewidth = 1)+
      theme_minimal() +
      labs(title = paste0("Median Time to Diagnosis or AIDS Death\namong Women in ",cnt," between ",2015," and ",2023), 
           x = "Year", 
           y = "Median Years to Diagnosis or AIDS Death")+
      scale_x_continuous(breaks = c(seq(2015, 2023, 1),2023),
                         limits = c(2015, 2023))+
      scale_y_continuous(limits = c(0,maxttf))+
      theme(
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
      )
    
    
  } else if(!is.null(make_country[[cnt]]$tdx_male_counter)){
    
    malettd_counter = Agg_simul_pool_time_dx_prev(out_simul_tdx_all = make_country[[cnt]]$tdx_male_counter$out_simul_tdx_all,year = 2015:2023,sex = "male")
    femalettd_counter = Agg_simul_pool_time_dx_prev(out_simul_tdx_all = make_country[[cnt]]$tdx_female_counter$out_simul_tdx_all,year = 2015:2023,sex = "female")
    
    maxttf = max(c(femalettd$time_dx_uci,femalettd_counter$time_dx_uci))
    
    plotttf = ggplot() +
      #female
      geom_ribbon(data = femalettd,
                  aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci,group = 1),
                  fill = col,
                  alpha = 0.3)+
      geom_line(data = femalettd,aes(y = time_dx, x = year), color = col,linewidth = 1)+
      #female counter
      geom_ribbon(data = femalettd_counter,
                  aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci,group = 1),
                  fill = col,
                  alpha = 0.2)+
      geom_line(data = femalettd_counter,aes(y = time_dx, x = year), color = col,linewidth = 1,linetype = 2)+
      theme_minimal() +
      labs(title = paste0("Observed and Counterfactual Median Time to Diagnosis or AIDS Death\namong Women in ",cnt," between ",2015," and ",2023), 
           x = "Year", 
           y = "Median Years to Diagnosis or AIDS Death")+
      scale_x_continuous(breaks = c(seq(2015, 2023, 1),2023),
                         limits = c(2015, 2023))+
      scale_y_continuous(limits = c(0,maxttf))+
      # #Add legend for Line 1
      annotate("segment", x = 2016.1,xend = 2016.4,y = maxttf*0.1,yend = maxttf*0.1 ,color = "firebrick3", linewidth = 1,linetype = 2)+
      annotate("segment", x = 2016.6,xend = 2017,y = maxttf*0.1,yend = maxttf*0.1 ,color = "firebrick3", linewidth = 1)+
      annotate("text", x = 2015.3, y = maxttf*0.1, label = "Female", color = "firebrick3", hjust = 0, vjust = 0.4)+
      annotate("text", x = 2016.24, y = maxttf*0.32, label = "Observed", color = "black", hjust = 0, vjust = 0.5,angle = 315)+
      annotate("text", x = 2015.27, y = maxttf*0.40, label = "Counterfactual", color = "black", hjust = 0, vjust = 0.5,angle = 315)+
      theme(
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
      )
    
    # male ttd counter  
    maxttm = max(c(malettd$time_dx_uci,malettd_counter$time_dx_uci))
    
    
    plotttm = ggplot() +
      #male 
      geom_ribbon(data = malettd,
                  aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci,group = 1),
                  fill = col2,
                  alpha = 0.4) +
      geom_line(data = malettd,aes(y = time_dx, x = year), color = col2,linewidth = 1)+
      #male counter
      geom_ribbon(data = malettd_counter,
                  aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci,group = 1),
                  fill = col2,
                  alpha = 0.3) +
      geom_line(data = malettd_counter,aes(y = time_dx, x = year), color = col2,linewidth = 1,linetype = 2)+
      theme_minimal() +
      labs(title = paste0("Observed and Counterfactual Median Time to Diagnosis or AIDS Death\namong Men in ",cnt," between ",2015," and ",2023), 
           x = "Year", 
           y = "Median Years to Diagnosis or AIDS Death")+
      scale_x_continuous(breaks = c(seq(2015, 2023, 1),2023),
                         limits = c(2015, 2023))+
      scale_y_continuous(limits = c(0,maxttm))+
      # #Add legend for Line 1
      annotate("segment", x = 2016.1,xend = 2016.4,y = maxttm*0.1,yend = maxttm*0.1 ,color = "steelblue", linewidth = 1,linetype = 2)+
      annotate("segment", x = 2016.6,xend = 2017,y = maxttm*0.1,yend = maxttm*0.1 ,color = "steelblue", linewidth = 1)+
      annotate("text", x = 2015.3, y = maxttm*0.1, label = "Male", color = "steelblue", hjust = 0, vjust = 0.4)+
      annotate("text", x = 2016.24, y = maxttm*0.32, label = "Observed", color = "black", hjust = 0, vjust = 0.5,angle = 315)+
      annotate("text", x = 2015.27, y = maxttm*0.40, label = "Counterfactual", color = "black", hjust = 0, vjust = 0.5,angle = 315)+
      theme(
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
      )
    
    
  }
  
  plotttf + plotttm
}

counter_awareness <- function(make_country,cnt){
  sex1 = "Female"
  sex2 = "Male"
  
  var = paste0("aggpooled",sex1)
  varc = paste0("aggpooled",sex1,"C")
  var2 = paste0("aggpooled",sex2)
  varc2 = paste0("aggpooled",sex2,"C")
  
  
  if(sex1 == "Female"){
    col = "firebrick3"
    col2 = "steelblue"
  }else{
    col = "steelblue"
    col2 = "firebrick3"
    
  }
  
  aggpooledMale = Agg_simul_aware(make_country[[cnt]]$aware_male,age = c("15-99"),year = 2015:2023,sex = "male")
  aggpooledFemale = Agg_simul_aware(make_country[[cnt]]$aware_female,age = c("15-99"),year = 2015:2023,sex = "female")
  
  if(is.null(make_country[[cnt]]$aware_male_counter)){
    
    maxaware = max(c(aggpooledFemale$uci,aggpooledMale$uci))
    
    awareplotfemale = ggplot() +
      geom_ribbon(data = get(var), 
                  aes(x = year, ymin = lci, ymax = uci,group = 1), 
                  fill = col, 
                  alpha = ifelse(sex1 == "female",0.2,0.3)) +
      
      geom_line(data = get(var),aes(y = propaware, x = year), color = col,linewidth = 1)+
      
      geom_hline(yintercept = 0.95,linetype = 3)+
      theme_minimal() +
      labs(title = paste0("Proportion WLHIV Aware of Their Status in ",cnt,": ",2015,"-",2023), 
           x = "Year", 
           y = "Proportion WLHIV aware ")+
      scale_x_continuous(breaks = seq(2015, 2023, 1),
                         limits = c(2015, 2023))+
      scale_y_continuous(limits = c(0,1),breaks = c(seq(0.00, 0.95, 0.20),0.95))+
      theme(
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
      )
    awareplotfemale
  } else if(!is.null(make_country[[cnt]]$aware_male_counter)){
    
    
    aggpooledFemaleC = Agg_simul_aware(make_country[[cnt]]$aware_female_counter,age = c("15-99"),year = 2015:2023,sex = "female")
    
    
    awareplotfemale = ggplot() +
      # women
      geom_ribbon(data = get(var), 
                  aes(x = year, ymin = lci, ymax = uci,group = 1), 
                  fill = col, 
                  alpha = ifelse(sex1 == "Female",0.2,0.3)) +
      
      geom_line(data = get(var),aes(y = propaware, x = year), color = col,linewidth = 1)+
      #women counter
      geom_ribbon(data = get(varc),
                  aes(x = year, ymin = lci, ymax = uci,group = 1),
                  fill = col,
                  alpha = ifelse(sex1 == "Female",0.2,0.3)) +
      geom_line(data = get(varc),aes(y = propaware, x = year), color = col,linewidth = 1,linetype = 2)+
      geom_hline(yintercept = 0.95,linetype = 3)+
      theme_minimal() +
      labs(title = paste0("Proportion WLHIV Aware of Their Status in ",cnt,": ",2015,"-",2023), 
           x = "Year", 
           y = "Proportion WLHIV aware ")+
      scale_x_continuous(breaks = seq(2015, 2023, 1),
                         limits = c(2015, 2023))+
      scale_y_continuous(limits = c(0,1),breaks = c(seq(0.00, 0.95, 0.20),0.95))+
      annotate("segment", x = 2020.3,xend = 2020.8,y = 0.1,yend = 0.1 ,color = col, linewidth = 1)+
      annotate("segment", x = 2020.3,xend = 2020.8,y = 0.17,yend = 0.17 ,color = col, linewidth = 1,linetype = "dashed")+
      annotate("text", x = 2020.9, y = 0.1, label = "Observed", color = "black", hjust = 0, vjust = 0.5)+
      annotate("text", x = 2020.9, y = 0.17, label = "Counterfactual", color = "black", hjust = 0, vjust = 0.5)+
      theme(
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
      )
    awareplotfemale
    
  }
  
  if(is.null(make_country[[cnt]]$aware_male_counter)){
    
    
    awareplotmale = ggplot() +
      geom_ribbon(data = get(var2), 
                  aes(x = year, ymin = lci, ymax = uci,group = 1), 
                  fill = col2, 
                  alpha = ifelse(sex2 == "female",0.2,0.3)) +
      
      geom_line(data = get(var2),aes(y = propaware, x = year), color = col2,linewidth = 1)+
      
      geom_hline(yintercept = 0.95,linetype = 3)+
      theme_minimal() +
      labs(title = paste0("Proportion MLHIV Aware of Their Status in ",cnt,": ",2015,"-",2023), 
           x = "Year", 
           y = "Proportion MLHIV aware ")+
      scale_x_continuous(breaks = seq(2015, 2023, 1),
                         limits = c(2015, 2023))+
      scale_y_continuous(limits = c(0,1),breaks = c(seq(0.00, 0.95, 0.20),0.95))+
      theme(
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
      )
    awareplotmale
  } else if(!is.null(make_country[[cnt]]$aware_male_counter)){
    
    
    aggpooledMaleC = Agg_simul_aware(make_country[[cnt]]$aware_male_counter,age = c("15-99"),year = 2015:2023,sex = "male")
    
    
    awareplotmale = ggplot() +
      # women
      geom_ribbon(data = get(var2), 
                  aes(x = year, ymin = lci, ymax = uci,group = 1), 
                  fill = col2, 
                  alpha = ifelse(sex2 == "female",0.2,0.4)) +
      
      geom_line(data = get(var2),aes(y = propaware, x = year), color = col2,linewidth = 1)+
      #women counter
      geom_ribbon(data = get(varc2),
                  aes(x = year, ymin = lci, ymax = uci,group = 1),
                  fill = col2,
                  alpha = ifelse(sex2 == "female",0.2,0.3)) +
      geom_line(data = get(varc2),aes(y = propaware, x = year), color = col2,linewidth = 1,linetype = 2)+
      geom_hline(yintercept = 0.95,linetype = 3)+
      theme_minimal() +
      labs(title = paste0("Proportion MLHIV Aware of Their Status in ",cnt,": ",2015,"-",2023), 
           x = "Year", 
           y = "Proportion MLHIV aware ")+
      scale_x_continuous(breaks = seq(2015, 2023, 1),
                         limits = c(2015, 2023))+
      scale_y_continuous(limits = c(0,1),breaks = c(seq(0.00, 0.95, 0.20),0.95))+
      annotate("segment", x = 2020.3,xend = 2020.8,y = 0.1,yend = 0.1 ,color = "steelblue", linewidth = 1)+
      annotate("segment", x = 2020.3,xend = 2020.8,y = 0.17,yend = 0.17 ,color = "steelblue", linewidth = 1,linetype = "dashed")+
      annotate("text", x = 2020.9, y = 0.1, label = "Observed", color = "black", hjust = 0, vjust = 0.5)+
      annotate("text", x = 2020.9, y = 0.17, label = "Counterfactual", color = "black", hjust = 0, vjust = 0.5)+
      theme(
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
      )
    awareplotmale
    
  }
  awareplotmale + awareplotfemale
}

counter_unaware <- function(make_country,cnt){
 
  unaware = make_country[[i]]$simul$unaware
  unaware_counter = make_country[[i]]$counter_simul$unaware
  
  #find decrease in both
  unaware_obs_b = subset(getci(unaware), (sex == "both" &
                                            agegr == "15+"))
  unaware_obs_b$count = "observed"
  unaware_obs_b$mid = subset(
    cbind(
      unaware[, (1:5)],
      apply(
        X = unaware[, (6:3005)],
        MARGIN = 1,
        FUN = stats::quantile,
        probs = c(0.5),
        na.rm = TRUE
      )
    ), (sex == "both" & agegr == "15+")
  )[, 6]
  
  unaware_counter_b = subset(getci(unaware_counter), (sex == "both" &
                                                        agegr == "15+"))
  unaware_counter_b$count = "counter"
  unaware_counter_b$mid = subset(cbind(
    unaware_counter[, (1:5)],
    apply(
      X = unaware_counter[, (6:3005)],
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "both" &
     agegr == "15+"))[, 6]
  
  #find decrease in both
  unaware_obs_b = subset(getci(unaware), (sex == "both" &
                                            agegr == "15+"))
  unaware_obs_b$count = "observed"
  unaware_obs_b$mid = subset(
    cbind(
      unaware[, (1:5)],
      apply(
        X = unaware[, (6:3005)],
        MARGIN = 1,
        FUN = stats::quantile,
        probs = c(0.5),
        na.rm = TRUE
      )
    ), (sex == "both" & agegr == "15+")
  )[, 6]
  
  #find prop of PLHIV due to declines
  unaware_counter_b = subset(getci(cbind(
    unaware_counter[, (1:5)],
    (unaware[,6:3005] - unaware_counter[,6:3005])/unaware[,6:3005])),
    (sex == "both" & agegr == "15+"))
  
  
  unaware_counter_b$count = "counter"
  unaware_counter_b$mid = subset(cbind(
    unaware_counter[, (1:5)],
    apply(
      X =  (unaware[,6:3005] - unaware_counter[,6:3005])/unaware[,6:3005],
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )),(sex == "both" &
     agegr == "15+"))[, 6]
  
  unaware_counter_b[24,c(9,6,7)]
  
  #find % increase in PLHIV due to declines
  unaware_counter_b = subset(getci(cbind(
    unaware_counter[, (1:5)],
    unaware[,6:3005]/unaware_counter[,6:3005])),
    (sex == "both" & agegr == "15+"))
  
  
  unaware_counter_b$count = "counter"
  unaware_counter_b$mid = subset(cbind(
    unaware_counter[, (1:5)],
    apply(
      X = (unaware[,6:3005]/unaware_counter[,6:3005]),
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "both" &
     agegr == "15+"))[, 6]
  
  unaware_counter_b[24,c(9,6,7)]
  
  # for abstract/paper male/female PLHIV prop
  #find PLHIV prop due to declines
  unaware_male_increase = subset(getci(cbind(
    unaware_counter[, (1:5)],
    (unaware[,6:3005] - unaware_counter[,6:3005])/unaware[,6:3005])),
    (sex == "male" & agegr == "15+"))
  
  
  unaware_male_increase$mid = subset(cbind(
    unaware_counter[, (1:5)],
    apply(
      X = (unaware[,6:3005]/unaware_counter[,6:3005]),
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "male" &
     agegr == "15+"))[, 6]
  
  unaware_male_increase[24,c(8,6,7)]
  
  #find PLHIV prop due to declines
  unaware_female_increase = subset(getci(cbind(
    unaware_counter[, (1:5)],
    (unaware[,6:3005] - unaware_counter[,6:3005])/unaware[,6:3005])),
    (sex == "female" & agegr == "15+"))
  
  
  unaware_female_increase$mid = subset(cbind(
    unaware_counter[, (1:5)],
    apply(
      X = (unaware[,6:3005]/unaware_counter[,6:3005]),
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "female" &
     agegr == "15+"))[, 6]
  
  unaware_female_increase[24,c(8,6,7)]
  
  
  
  # for graph
  unaware_obs_m = subset(getci(unaware), (sex == "male" &
                                            agegr == "15+"))
  unaware_obs_m$count = "observed"
  unaware_obs_m$mid = subset(cbind(
    unaware[, (1:5)],
    apply(
      X = unaware[, (6:3005)],
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "male" &
     agegr == "15+"))[, 6]
  
  unaware_counter_m = subset(getci(unaware_counter), (sex == "male" &
                                                        agegr == "15+"))
  unaware_counter_m$count = "counter"
  unaware_counter_m$mid = subset(cbind(
    unaware_counter[, (1:5)],
    apply(
      X = unaware_counter[, (6:3005)],
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "male" &
     agegr == "15+"))[, 6]
  unaware_m = rbind(unaware_counter_m, unaware_obs_m)
  
  
  unaware_obs_f = subset(getci(unaware), (sex == "female" &
                                            agegr == "15+"))
  unaware_obs_f$count = "observed"
  unaware_obs_f$mid = subset(cbind(
    unaware[, (1:5)],
    apply(
      X = unaware[, (6:3005)],
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "female" &
     agegr == "15+"))[, 6]
  
  unaware_counter_f = subset(getci(unaware_counter), (sex == "female" &
                                                        agegr == "15+"))
  unaware_counter_f$count = "counter"
  unaware_counter_f$mid = subset(cbind(
    unaware_counter[, (1:5)],
    apply(
      X = unaware_counter[, (6:3005)],
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "female" &
     agegr == "15+"))[, 6]
  unaware_f = rbind(unaware_counter_f, unaware_obs_f)
  
  unaware_m$sex = "Male"
  unaware_f$sex = "Female"
  
  unaware_sex = rbind(unaware_m, unaware_f)
  
  unaware_plot = ggplot(unaware_sex) +
    geom_line(aes(
      x = year,
      y = mid,
      linetype = count,
      group = interaction(count, sex),
      colour = sex
    )) +
    geom_ribbon(aes(
      x = year,
      ymin = lower ,
      ymax = upper ,
      group = interaction(count, sex),
      fill = sex
    ),
    alpha = 0.4) +
    #geom_area(aes(x = year,y=upper,group = count, fill = count),alpha = 0.2)+
    scale_y_continuous(limits = c(0, max(unaware_sex$upper[unaware_sex$year%in% 2015:2023])*1.15)) +
    scale_x_continuous(limits = c(2015, 2023),
                       breaks = 2015:2023) +
    theme_minimal() +
    labs(
      title = paste0(
        "Number of Undiagnosed PLHIV\nPooled in Countries with a Decline:\n",
        2015,
        "-",
        2023
      ),
      x = NULL,
      y = "Number of Undiagosed PLHIV (millions)"
    ) +
    scale_linetype_manual(
      name = "Scenario",
      label = c("observed" = "Observed",
                "counter" = "Counterfactual"),
      values = c("observed" = 1,
                 "counter" = 2)
    ) +
    scale_fill_manual(name = "Sex",
                      values = c("Male" = "steelblue",
                                 "Female" = "firebrick3")) +
    scale_colour_manual(name = "Sex",
                        values = c("Male" = "steelblue",
                                   "Female" = "firebrick3")) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      # Center the title,
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(
        size = 15,
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.text.y = element_text(size = 15),
      #legend.text = element_text(size = 15)
    )
  
  
  #use unaware tot
  unaware_tot = as.data.frame(matrix(nrow = 465, ncol = 3006, data = 0))
  unaware_tot = cbind(make_country[[i]]$simul$unaware[, 1:5],
                      unaware[,6:3005] - unaware_counter[,(6:3005)])
  
  unaware_obs_tot_m = subset(getci(unaware_tot), (sex == "male" &
                                                    agegr == "15+"))
  unaware_obs_tot_m$count = "observed"
  unaware_obs_tot_m$mid = subset(cbind(
    unaware_tot[, (1:5)],
    apply(
      X = unaware_tot[, (6:3005)],
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "male" &
     agegr == "15+"))[, 6]
  
  
  
  unaware_obs_tot_f = subset(getci(unaware_tot), (sex == "female" &
                                                    agegr == "15+"))
  unaware_obs_tot_f$count = "observed"
  unaware_obs_tot_f$mid = subset(cbind(
    unaware_tot[, (1:5)],
    apply(
      X = unaware_tot[, (6:3005)],
      MARGIN = 1,
      FUN = stats::quantile,
      probs = c(0.5),
      na.rm = TRUE
    )
  ),
  (sex == "female" &
     agegr == "15+"))[, 6]
  
  
  unaware_obs_tot_m$sex = "Male"
  unaware_obs_tot_f$sex = "Female"
  
  unaware_tot_p = rbind(unaware_obs_tot_m, unaware_obs_tot_f)
  
  pd <-
    position_dodge(width = 0.92)
  
  unaware_plot_tot = ggplot(unaware_tot_p) +
    geom_col(aes(
      x = year,
      y = mid / 1000,
      group = sex,
      fill = sex
    ),
    position = pd,
    alpha = 0.7) +
    geom_errorbar(
      aes(
        x = year,
        ymin = lower / 1000,
        ymax = upper / 1000,
        group = sex,
        colour = sex
      ),
      position = pd,
      alpha = 1,
      linewidth = 0.6,
      width = 0.5
    ) +
    geom_errorbar(
      aes(
        x = year,
        ymin = lower / 1000,
        ymax = upper / 1000,
        group = sex
      ),
      position = pd,
      alpha = 1,
      linewidth = 0.2,
      width = 0.5
    ) +
    #geom_area(aes(x = year,y=upper,group = count, fill = count),alpha = 0.2)+
    #scale_y_continuous(limits = c(0,2e5/1000))+
    scale_x_continuous(limits = c(2015, 2023.5),
                       breaks = c(2015:2023)) +
    theme_minimal() +
    labs(
      title = paste0(
        "Additional Number of Undiagnosed \nPLHIV among Countries with a Decline: \n",
        2015,
        "-",
        2023
      ),
      x = NULL,
      y = "Number of Undiagosed PLHIV (Thousands)"
    ) +
    scale_fill_manual(name = "Sex",
                      values = c("Male" = "steelblue",
                                 "Female" = "firebrick3")) +
    scale_colour_manual(name = "Sex",
                        values = c("Male" = "steelblue",
                                   "Female" = "firebrick3")) +
    theme(
      #legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      # Center the title,
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(
        size = 15,
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.text.y = element_text(size = 15),
      #legend.text = element_text(size = 15)
    )
  
  
  unaware_plot_tot+unaware_plot
  
  
}
