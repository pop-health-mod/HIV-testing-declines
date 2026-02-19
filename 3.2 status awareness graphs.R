library(data.table)
library(ggplot2)
source("anc testing/0.5 simul-aware-functions.R")
source("anc testing/1.0 simmod.R")
path_out <- here::here("outputs/paper 2026/TDX")
counter_years <-  read_rds("anc testing/data/counter_years.rds")


# set output directory
path_out <- here::here("outputs/Paper 2026/awareness")
#make_country = readRDS("anc testing/data/make_country_simul_final.rds")


# ----prep for graphs----
aware_agg_simul_malefull <-
  readRDS(file = paste0(here::here("outputs"),
                        "/male aware observed.rda"))
aware_agg_simul_male_counter <-
  readRDS(file = paste0(here::here("outputs"),
                        "/male aware counter.rda"))
aware_agg_simul_femalefull <-
  readRDS(file = paste0(here::here("outputs"),
                        "/female aware observed.rda"))
aware_agg_simul_female_counter <-
  readRDS(file = paste0(here::here("outputs"),
                        "/female aware counter.rda"))
aware_agg_simul_bothfull <-
  readRDS(file = paste0(here::here("outputs"),
                        "/both aware observed.rda"))
aware_agg_simul_both_counter <-
  readRDS(file = paste0(here::here("outputs"),
                        "/both aware counter.rda"))

# aware_agg_simul_male = aware_agg_simul_malefull
# aware_agg_simul_female = aware_agg_simul_femalefull
# aware_agg_simul_both = aware_agg_simul_bothfull

aware_agg_simul_male = aware_agg_simul_malefull[names(aware_agg_simul_malefull) %in% names(aware_agg_simul_male_counter)]
aware_agg_simul_female = aware_agg_simul_femalefull[names(aware_agg_simul_femalefull) %in% names(aware_agg_simul_male_counter)]
aware_agg_simul_both =  aware_agg_simul_bothfull[names(aware_agg_simul_bothfull) %in% names(aware_agg_simul_both_counter)]

for (cnt in names(aware_agg_simul_bothfull)) {
  aware_agg_simul_female_counter[[cnt]]$out_simul_aware_all$propaware = aware_agg_simul_female_counter[[cnt]]$out_simul_aware_all$value
  aware_agg_simul_male_counter[[cnt]]$out_simul_aware_all$propaware = aware_agg_simul_male_counter[[cnt]]$out_simul_aware_all$value
  aware_agg_simul_both_counter[[cnt]]$out_simul_aware_all$propaware = aware_agg_simul_both_counter[[cnt]]$out_simul_aware_all$value
  
}


aware_agg_simul_diff_Female = list()
aware_agg_simul_diff_Male = list()

for (i in 1:length(aware_agg_simul_male)) {
  cnt = names(aware_agg_simul_male)[i]
  print(cnt)
  
  if (!is.null(aware_agg_simul_male_counter[[cnt]])) {
    tryCatch(
      expr = {
        diffF = aware_agg_simul_female[[cnt]]
        diffM = aware_agg_simul_male[[cnt]]
        diffFc = aware_agg_simul_female_counter[[cnt]]
        diffMc = aware_agg_simul_male_counter[[cnt]]
        
        
        simul_agg_simul_diffF = diffFc
        simul_agg_simul_diffM = diffMc
        
        simul_agg_simul_diffM$out_simul_aware_all$propaware = diffMc$out_simul_aware_all$propaware - diffM$out_simul_aware_all$propaware
        simul_agg_simul_diffF$out_simul_aware_all$propaware = diffFc$out_simul_aware_all$propaware - diffF$out_simul_aware_all$propaware
        simul_agg_simul_diffM$out_simul_aware_all$value = diffMc$out_simul_aware_all$propaware - diffM$out_simul_aware_all$propaware
        simul_agg_simul_diffF$out_simul_aware_all$value = diffFc$out_simul_aware_all$propaware - diffF$out_simul_aware_all$propaware
        
        
        
        print(max(simul_agg_simul_diffF$out_simul_aware_all$propaware))
        
        aware_agg_simul_diff_Female[[cnt]] = simul_agg_simul_diffF
        aware_agg_simul_diff_Male[[cnt]] = simul_agg_simul_diffM
        
        #aware_agg_simul[[cnt]]$out_simul_aware_all = simul_aware
        
        
      },
      error = function(e) {
        message("Caught an error: ", e$message)
        
      },
      finally = {
        
      }
    )
  }
  
}


aggpooledMale = Agg_simul_aware(
  aware_agg_simul_male,
  age = c("15-99"),
  year = 2015:2023,
  sex = "male"
)
aggpooledFemale = Agg_simul_aware(
  aware_agg_simul_female,
  age = c("15-99"),
  year = 2015:2023,
  sex = "female"
)

aggpooledMalec = Agg_simul_aware(
  aware_agg_simul_male_counter,
  age = c("15-99"),
  year = 2015:2023,
  sex = "male"
)
aggpooledFemalec = Agg_simul_aware(
  aware_agg_simul_female_counter,
  age = c("15-99"),
  year = 2015:2023,
  sex = "female"
)

aggpooledBoth = Agg_simul_aware(
  aware_agg_simul_both,
  age = c("15-99"),
  year = 2015:2023,
  sex = "both"
)
aggpooledBothc = Agg_simul_aware(
  aware_agg_simul_both_counter,
  age = c("15-99"),
  year = 2015:2023,
  sex = "both"
)
aggpooledBothfull = Agg_simul_aware(
  aware_agg_simul_bothfull,
  age = c("15-99"),
  year = 2015:2023,
  sex = "both"
)

  # ----prop aware male----


sex = "Male"

var = paste0("aggpooled", sex)
varc = paste0("aggpooled", sex, "c")


if (sex == "Female") {
  col = "firebrick3"
  col2 = "steelblue"
} else{
  col = "steelblue"
  col2 = "firebrick3"
  
}

start_year = 2015
end_year = 2023


plot = ggplot() +
  geom_ribbon(
    data = get(var),
    aes(
      x = year,
      ymin = lci,
      ymax = uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  
  geom_line(
    data = get(var),
    aes(y = propaware, x = year),
    color = col,
    linewidth = 1
  ) +
  geom_line(
    data = get(varc),
    aes(y = propaware, x = year),
    color = col,
    linewidth = 1,
    linetype = 2
  ) +
  
  geom_ribbon(
    data = get(varc),
    aes(
      x = year,
      ymin = lci,
      ymax = uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  
  # geom_line(data = get(var2),aes(y = propaware, x = year), color = col2,linewidth = 1)+
  # geom_line(data = get(varc2),aes(y = propaware, x = year), color = col2,linewidth = 1,linetype = 2)+
  
  theme_minimal() +
  labs(
    title = paste0(
      "Status Awareness among ",
      ifelse(sex == "Female", "WLHIV", "MLHIV"),
      "\nin Countries with a Decline: ",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Proportion PLHIV aware"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0.00, 1.00, 0.20), 0.95)) +
  
  geom_hline(yintercept = 0.95,
             linetype = 3,
             linewidth = 1) +
  theme(
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
    
    
  )
plot

ggsave(
  plot = plot,
  file = paste0(path_out, "/paper_propaware", sex, ".png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)
#stop()

# ----propaware female----
sex = "Female"

var = paste0("aggpooled", sex)
varc = paste0("aggpooled", sex, "c")


if (sex == "Female") {
  col = "firebrick3"
  col2 = "steelblue"
} else{
  col = "steelblue"
  col2 = "firebrick3"
  
}

start_year = 2015
end_year = 2023


plot = ggplot() +
  geom_ribbon(
    data = get(var),
    aes(
      x = year,
      ymin = lci,
      ymax = uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  
  geom_line(
    data = get(var),
    aes(y = propaware, x = year),
    color = col,
    linewidth = 1
  ) +
  geom_line(
    data = get(varc),
    aes(y = propaware, x = year),
    color = col,
    linewidth = 1,
    linetype = 2
  ) +
  
  geom_ribbon(
    data = get(varc),
    aes(
      x = year,
      ymin = lci,
      ymax = uci,
      group = 1
    ),
    fill = col2,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  
  # geom_line(data = get(var2),aes(y = propaware, x = year), color = col2,linewidth = 1)+
  # geom_line(data = get(varc2),aes(y = propaware, x = year), color = col2,linewidth = 1,linetype = 2)+
  
  theme_minimal() +
  labs(
    title = paste0(
      "Status Awareness among ",
      ifelse(sex == "Female", "WLHIV", "MLHIV"),
      "\nin Countries with a Decline: ",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Proportion PLHIV aware"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0.00, 1.00, 0.20), 0.95)) +
  #max(tdx_female$time_dx_med[tdx_female$year >= start_year]))))+
  # #Add legend for Line 1
  #annotate("segment", x = 2019.4,xend = 2019.7,y = 0.5,yend = 0.5 ,color = female_colour, linewidth = 1)+
  #annotate("segment", x = 2020.3,xend = 2020.8,y = 0.5,yend = 0.5 ,color = "firebrick3", linewidth = 1)+
  #annotate("segment", x = 2019.4,xend = 2019.7,y = 0.65,yend = 0.65 ,color = "firebrick3", linewidth = 1)+
  #annotate("segment", x = 2020.3,xend = 2020.8,y = 0.55,yend = 0.55 ,color = "firebrick3", linewidth = 1,linetype = "dashed")+
  # annotate("text", x = 2019.3, y = 0.8, label = "Female", color = "firebrick3", hjust = 0, vjust = 0.5)+
  # annotate("text", x = 2020.3, y = 0.8, label = "Male", color = "steelblue", hjust = 0, vjust = 0.5)+
  #annotate("text", x = 2020.9, y = 0.5, label = "Observed", color = "black", hjust = 0, vjust = 0.5)+
  #annotate("text", x = 2020.9, y = 0.55, label = "Counterfactual", color = "black", hjust = 0, vjust = 0.5)+
  #geom_vline(xintercept = 2018, linetype = "dashed", color = "black", size = 0.5)+
geom_hline(yintercept = 0.95, linetype = 3) +
  ##geom_vline(xintercept = 2016, linetype = "dashed", color = "black", size = 0.1)+
  theme(
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
    
    
  )

plot

ggsave(
  plot = plot,
  file = paste0(path_out, "/paper_propaware", sex, ".png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)


# ----prop aware both----


sex = "Both"

var = paste0("aggpooled", sex)
varc = paste0("aggpooled", sex, "c")


if (sex == "Both") {
  col = "cyan4"
  col2 = "steelblue"
} else{
  col = "steelblue"
  col2 = "firebrick3"
  
}

start_year = 2015
end_year = 2023


plot = ggplot() +
  geom_ribbon(
    data = get(var),
    aes(
      x = year,
      ymin = lci,
      ymax = uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  
  geom_line(
    data = get(var),
    aes(y = propaware, x = year),
    color = col,
    linewidth = 1
  ) +
  geom_line(
    data = get(varc),
    aes(y = propaware, x = year),
    color = col,
    linewidth = 1,
    linetype = 2
  ) +
  
  geom_ribbon(
    data = get(varc),
    aes(
      x = year,
      ymin = lci,
      ymax = uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  
  # geom_line(data = get(var2),aes(y = propaware, x = year), color = col2,linewidth = 1)+
  # geom_line(data = get(varc2),aes(y = propaware, x = year), color = col2,linewidth = 1,linetype = 2)+
  
  theme_minimal() +
  labs(
    title = paste0(
      "Status Awareness among ",
      "PLHIV",
      "\nPooled in Countries with a Decline:\n",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Proportion PLHIV Aware"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0.00, 1.00, 0.20), 0.95)) +
  geom_hline(yintercept = 0.95, linetype = 3) +
  theme(
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
    
    
  )
plot

ggsave(
  plot = plot,
  file = paste0(path_out, "/paper_propaware", sex, ".png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)



# ---- simul difference graph ----




diffpooledM = Agg_simul_aware(
  aware_agg_simul_diff_Male,
  age = c("15-99"),
  year = 2015:2023,
  sex = "male"
)
diffpooledF = Agg_simul_aware(
  aware_agg_simul_diff_Female,
  age = c("15-99"),
  year = 2015:2023,
  sex = "female"
)


start_year = 2015
end_year = 2023
aware_agg_simul_diff_Female
aware_agg_simul_diff_Male
male_colour = "steelblue"
female_colour = "firebrick3"

plot = ggplot() +
  geom_ribbon(
    data = diffpooledM,
    aes(
      x = year,
      ymin = lci * 100,
      ymax = uci * 100,
      group = 1
    ),
    fill = male_colour,
    alpha = 0.3
  ) +
  geom_ribbon(
    data = diffpooledF,
    aes(
      x = year,
      ymin = lci * 100,
      ymax = uci * 100,
      group = 1
    ),
    fill = female_colour,
    alpha = 0.1
  ) +
  
  geom_line(
    data = diffpooledM,
    aes(y = propaware * 100, x = year),
    color = male_colour,
    linewidth = 1
  ) +
  geom_line(
    data = diffpooledF,
    aes(y = propaware * 100, x = year),
    color = female_colour,
    linewidth = 1
  ) +
  # geom_ribbon(data = aggpooledFC,
  #             aes(x = year, ymin = lci, ymax = uci,group = 1),
  #             fill = "steelblue",
  #             alpha = 0.3,colour = "steelblue") +
  # geom_ribbon(data = aggpooledFC,
  #             aes(x = year, ymin = lci, ymax = uci,group = 1),
  #             fill = "firebrick3",
  #             alpha = 0.1,colour = "firebrick3") +
  # # geom_line(data = aggpooledMC,aes(y = propaware, x = year), color = "steelblue",linewidth = 1,linetype = 2)+
  # geom_line(data = aggpooledFC,aes(y = propaware, x = year), color = "firebrick3",linewidth = 1,linetype = 2)+
  theme_minimal() +
  labs(
    title = paste0(
      "Additional Percentage Points of\nStatus Awareness Lost due to Declines:\n ",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Percentage Points of Status Awareness"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(-0.27, 2.4),
                     breaks = c(0, 0.5, 1.0, 1.5, 2.0, 2.5)) +
  theme(
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
    axis.text.y = element_text(size = 15)
  )
plot

ggsave(
  plot = plot,
  file = paste0(path_out, "/paper_propawaredifference.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)


#====geographical status awarness====

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggnewscale)
library(tidyverse)

# Get the world map (western sahara missing to align with UNAIDS)
world <-
  read_sf("ne_10m_admin_0_countries_vnm/ne_10m_admin_0_countries_vnm.shp")
world$name = world$NAME

# Filter for africa_anc
africa_anc <- world[world$CONTINENT == "Africa", ]

#adjust names to match with those from package( ensure your match up too)
africa_anc$name[africa_anc$name == "Dem. Rep. Congo"] = "Democratic Republic of the Congo"
africa_anc$name[africa_anc$name == "S. Sudan"] = "South Sudan"
africa_anc$name[africa_anc$name == "Tanzania"] = "United Republic of Tanzania"
africa_anc$name[africa_anc$name == "Cabo Verde"] = "Cape Verde"
africa_anc$name[africa_anc$name == "Republic of the Congo"] = "Congo"



## -----female aware ----
#this makes a data frame of testing by year for all countries, col 9 is decline in testing
propaware_tests_female = matrix(nrow = length(aware_agg_simul_diff_Female),
                                ncol = 10)
for (i in 1:length(aware_agg_simul_diff_Female)) {
  cnt = names(aware_agg_simul_diff_Female)[i]
  print(cnt)
  PROPAWARE = subset(
    aware_agg_simul_female[[cnt]]$out_simul_aware_all,
    age = c("15-99"),
    year = 2015:2023,
    sex = "female"
  )[, quantile(propaware, probs = c(0.5)), by = year][, 2]
  
  
  propaware_tests_female[i, 1:9] = unname(unlist(PROPAWARE))
  
}
propaware_tests_female[, 10] = propaware_tests_female[, 9] - propaware_tests_female[, 1]
propaware_tests_female = as.data.frame(propaware_tests_female)
rownames(propaware_tests_female) = names(aware_agg_simul_diff_Female)

dim(propaware_tests_female[propaware_tests_female$V9>0.95,8:9])[1]

#create dataframe with col1 = name of country, col2 = data(decline)
africa_anc_data <-
  data.frame(name = names(aware_agg_simul_diff_Female),
             hiv_decline = propaware_tests_female$V10 * 100)

# Merge with map data
africa_anc_map <-
  merge(
    africa_anc,
    africa_anc_data,
    by.x = "name",
    by.y = "name",
    all.x = TRUE
  )

vec_empty = vector()

#find those without data
for (i in 1:length(make_country)) {
  if (is.null(make_country[[i]]$simul)) {
    vec_empty = c(vec_empty, names(make_country)[i])
  }
}
#set countries find countries with data
black_countries <- names(make_country)
black_countries = black_countries[!(black_countries %in% vec_empty)]

# Create a new column for color
africa_anc_map$color <-
  ifelse(africa_anc_map$name %in% black_countries, "black", NA)


#adjust colours on the scale
africa_anc_map <- africa_anc_map %>%
  mutate(
    category = case_when(
      !(name %in% black_countries) ~ "Selected Countries",
      #for no data
      is.na(hiv_decline) ~ "No Data",
      name %in% names(make_country)[7] ~ "Madagascar",
      # for no decline
      TRUE ~ "Gradient"
    )
  )


plot_africa_aware_female = ggplot() +
  # Gradient layer for hiv_decline
  geom_sf(
    data = africa_anc_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline),
    color = "black"
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey80",
    name = "percent (%) change\nin HIV testing",
    limits = c(0, 50),
    direction = 1,
    #guide = F
    
  ) +
  
  # New fill scale for categorical values
  ggnewscale::new_scale_fill() +
  
  # Categorical layers (No Data & Selected Countries)
  geom_sf(
    data = africa_anc_map %>% filter(category %in% c(
      "No Data", "Selected Countries", "Madagascar"
    )),
    aes(fill = category),
    # Map fill to category
    color = "black"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Selected Countries" = "grey80",
      "No Data" = "white",
      "Madagascar" = "black"
    ),
    labels = c(
      "Selected Countries" = "No Data",
      "No Data" = "No Decline",
      "Madagascar" = "29% Madagascar"
    )
    
    
    #guide = F
  ) +
  
  # Theme and labels
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    #legend.background = element_blank(),legend.key = element_blank()
    plot.title = element_text(hjust = 0.5,        # Center the title
                              face = "bold",      # Make the title bold
                              size = 12)
  ) +
  labs(title = "Percent (%) Change in HIV Testing at \nAntenatalcare (ANC) Testing from 2017 to 2023")

plot_africa_aware_female
ggsave(
  plot = plot_africa_aware_female,
  file = paste0(
    path_out,
    "/africa_geograph_ANC female status awareness change from 2023 to 2017.png"
  ),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)

## ----male aware----
#this makes a data frame of testing by year for all countries, col 9 is decline in testing
propaware_tests_male = matrix(nrow = length(aware_agg_simul_diff_Male), ncol = 10)
for (i in 1:length(aware_agg_simul_diff_Male)) {
  cnt = names(aware_agg_simul_diff_Male)[i]
  print(cnt)
  PROPAWARE = subset(
    aware_agg_simul_male[[cnt]]$out_simul_aware_all,
    age = c("15-99"),
    year = 2015:2023,
    sex = "male"
  )[, quantile(propaware, probs = c(0.5)), by = year][, 2]
  
  
  propaware_tests_male[i, 1:9] = unname(unlist(PROPAWARE))
  
}
propaware_tests_male[, 10] = propaware_tests_male[, 9] - propaware_tests_male[, 1]
propaware_tests_male = as.data.frame(propaware_tests_male)
rownames(propaware_tests_male) = names(aware_agg_simul_diff_Male)


#create dataframe with col1 = name of country, col2 = data(decline)
africa_anc_data <-
  data.frame(name = names(aware_agg_simul_diff_Male),
             hiv_decline = propaware_tests_male$V10 * 100)
# Merge with map data
africa_anc_map <-
  merge(
    africa_anc,
    africa_anc_data,
    by.x = "name",
    by.y = "name",
    all.x = TRUE
  )

vec_empty = vector()

#find those without data
for (i in 1:length(make_country)) {
  if (is.null(make_country[[i]]$simul)) {
    vec_empty = c(vec_empty, names(make_country)[i])
  }
}
#set countries find countries with data
black_countries <-
  names(make_country)
black_countries = black_countries[!(black_countries %in% vec_empty)]

# Create a new column for color
africa_anc_map$color <-
  ifelse(africa_anc_map$name %in% black_countries, "black", NA)

#adjust colours on the scale
africa_anc_map <-
  africa_anc_map %>%
  mutate(category = case_when(
    !(name %in% black_countries) ~ "Selected Countries",
    #for no data
    is.na(hiv_decline) ~ "No Data",
    #name %in% names(make_country)[7] ~ "Madagascar",# for no decline
    TRUE ~ "Gradient"
  ))



plot_africa_aware_male = ggplot() +
  # Gradient layer for hiv_decline
  geom_sf(
    data = africa_anc_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline),
    color = "black"
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey80",
    name = "percent (%) change\nin HIV testing",
    limits = c(0, 50),
    direction = 1,
    #guide = F
    
  ) +
  
  # New fill scale for categorical values
  ggnewscale::new_scale_fill() +
  
  # Categorical layers (No Data & Selected Countries)
  geom_sf(
    data = africa_anc_map %>% filter(category %in% c("No Data", "Selected Countries")),
    aes(fill = category),
    # Map fill to category
    color = "black"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Selected Countries" = "grey80",
      "No Data" = "white"
    ),
    labels = c("Selected Countries" = "No Data",
               "No Data" = "No Decline")
    
    
  ) +
  
  # Theme and labels
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5,
                              # Center the title
                              face = "bold",
                              # Make the title bold
                              size = 12)
  ) +
  #labs(title = "ANC")
  labs(title = "Percent (%) Change in HIV Testing at \nAntenatalcare (ANC) Testing from 2017 to 2023")

plot_africa_aware_male
ggsave(
  plot = plot_africa_aware_male,
  file = paste0(
    path_out,
    "/africa_geograph_ANC male status awareness change from 2023 to 2017.png"
  ),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)



#==== counter geographicalstatus awarness====

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggnewscale)

## -----female aware ----
#this makes a data frame of testing by year for all countries, col 9 is decline in testing
propaware_tests_female = matrix(nrow = length(aware_agg_simul_diff_Female),
                                ncol = 10)
for (i in 1:length(aware_agg_simul_diff_Female)) {
  cnt = names(aware_agg_simul_diff_Female)[i]
  print(cnt)
  PROPAWARE = subset(
    aware_agg_simul_female[[cnt]]$out_simul_aware_all,
    age = c("15-99"),
    year = 2015:2023,
    sex = "female"
  )[, quantile(propaware, probs = c(0.5)), by = year][, 2]
  
  
  propaware_tests_female[i, 1:9] = unname(unlist(PROPAWARE))
  
}
propaware_tests_female[, 10] = propaware_tests_female[, 9] - propaware_tests_female[, 1]
propaware_tests_female = as.data.frame(propaware_tests_female)
rownames(propaware_tests_female) = names(aware_agg_simul_diff_Female)


#create dataframe with col1 = name of country, col2 = data(decline)
africa_anc_data <-
  data.frame(name = names(aware_agg_simul_diff_Female),
             hiv_decline = propaware_tests_female$V10 * 100)

# Merge with map data
africa_anc_map <-
  merge(
    africa_anc,
    africa_anc_data,
    by.x = "name",
    by.y = "name",
    all.x = TRUE
  )

vec_empty = vector()

#find those without data
for (i in 1:length(make_country)) {
  if (is.null(make_country[[i]]$simul)) {
    vec_empty = c(vec_empty, names(make_country)[i])
  }
}
#set countries find countries with data
black_countries <-
  names(make_country)
black_countries = black_countries[!(black_countries %in% vec_empty)]

# Create a new column for color
africa_anc_map$color <-
  ifelse(africa_anc_map$name %in% black_countries, "black", NA)

#adjust colours on the scale
africa_anc_map <-
  africa_anc_map %>%
  mutate(
    category = case_when(
      !(name %in% black_countries) ~ "Selected Countries",
      #for no data
      is.na(hiv_decline) ~ "No Data",
      name %in% names(make_country)[7] ~ "Madagascar",
      # for no decline
      TRUE ~ "Gradient"
    )
  )



plot_africa_aware_female = ggplot() +
  # Gradient layer for hiv_decline
  geom_sf(
    data = africa_anc_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline),
    color = "black"
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey80",
    name = "percent (%) change\nin HIV testing",
    limits = c(0, 50),
    direction = 1,
    #guide = F
    
  ) +
  
  # New fill scale for categorical values
  ggnewscale::new_scale_fill() +
  
  # Categorical layers (No Data & Selected Countries)
  geom_sf(
    data = africa_anc_map %>% filter(category %in% c(
      "No Data", "Selected Countries", "Madagascar"
    )),
    aes(fill = category),
    # Map fill to category
    color = "black"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Selected Countries" = "grey80",
      "No Data" = "white",
      "Madagascar" = "black"
    ),
    labels = c(
      "Selected Countries" = "No Data",
      "No Data" = "No Decline",
      "Madagascar" = "29% Madagascar"
    )
    
    
    #guide = F
  ) +
  
  # Theme and labels
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5,
                              # Center the title
                              face = "bold",
                              # Make the title bold
                              size = 12)
  ) +
  #labs(title = "ANC")
  labs(title = "Percent (%) Change in HIV Testing at \nAntenatalcare (ANC) Testing from 2017 to 2023")

plot_africa_aware_female
ggsave(
  plot = plot_africa_aware_female,
  file = paste0(
    path_out,
    "/africa_geograph_ANC female status awareness change from 2023 to 2017.png"
  ),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)

## ----male aware----
#this makes a data frame of testing by year for all countries, col 9 is decline in testing
propaware_tests_male_diff = matrix(nrow = length(aware_agg_simul_diff_Male), ncol = 10)
for (i in 1:length(aware_agg_simul_diff_Male)) {
  cnt = names(aware_agg_simul_diff_Male)[i]
  print(cnt)
  PROPAWARE = subset(
    aware_agg_simul_diff_Female[[cnt]]$out_simul_aware_all,
    age = c("15-99"),
    year = 2015:2023,
    sex = "male"
  )[, quantile(propaware, probs = c(0.5)), by = year][, 2]
  
  
  propaware_tests_male_diff[i, 1:9] = unname(unlist(PROPAWARE))
  
}
#propaware_tests_male_diff[,10] = propaware_tests_male_diff[,9] - propaware_tests_male_diff[,1]
propaware_tests_male_diff = as.data.frame(propaware_tests_male_diff)
rownames(propaware_tests_male_diff) = names(aware_agg_simul_diff_Male)


#create dataframe with col1 = name of country, col2 = data(decline)
africa_anc_data <-
  data.frame(name = names(aware_agg_simul_diff_Male),
             hiv_decline = propaware_tests_male_diff$V9 * 100)

# Merge with map data
africa_anc_map <-
  merge(
    africa_anc,
    africa_anc_data,
    by.x = "name",
    by.y = "name",
    all.x = TRUE
  )

vec_empty = vector()

#find those without data
for (i in 1:length(make_country)) {
  if (is.null(make_country[[i]]$simul)) {
    vec_empty = c(vec_empty, names(make_country)[i])
  }
}
#set countries find countries with data
black_countries <-
  names(make_country)
black_countries = black_countries[!(black_countries %in% vec_empty)]

# Create a new column for color
africa_anc_map$color <-
  ifelse(africa_anc_map$name %in% black_countries, "black", NA)

#adjust colours on the scale
africa_anc_map <-
  africa_anc_map %>%
  mutate(category = case_when(
    !(name %in% black_countries) ~ "Selected Countries",
    #for no data
    is.na(hiv_decline) ~ "No Data",
    TRUE ~ "Gradient"
  ))

plot_africa_aware_male = ggplot() +
  # Gradient layer for hiv_decline
  geom_sf(
    data = africa_anc_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline),
    color = "black"
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey80",
    name = "percent (%) change\nin HIV testing",
    limits = c(-1, 4),
    direction = 1,
    #guide = F
    
  ) +
  
  # New fill scale for categorical values
  ggnewscale::new_scale_fill() +
  
  # Categorical layers (No Data & Selected Countries)
  geom_sf(
    data = africa_anc_map %>% filter(category %in% c("No Data", "Selected Countries")),
    aes(fill = category),
    # Map fill to category
    color = "black"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Selected Countries" = "grey80",
      "No Data" = "white"
      #"Madagascar" = "black"
    ),
    labels = c("Selected Countries" = "No Data",
               "No Data" = "No Decline")
  ) +
  
  # Theme and labels
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    #legend.background = element_blank(),legend.key = element_blank()
    plot.title = element_text(hjust = 0.5,
                              # Center the title
                              face = "bold",
                              # Make the title bold
                              size = 12)
  ) +
  #labs(title = "ANC")
  labs(title = "Percent (%) Change in HIV Testing at \nAntenatalcare (ANC) Testing from 2017 to 2023")

plot_africa_aware_male
ggsave(
  plot = plot_africa_aware_male,
  file = paste0(
    path_out,
    "/africa_geograph_ANC male status awareness change from 2023 to 2017.png"
  ),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)







## ---- years saved for first95----
vector_male_miss95 = data.frame(country = vector(),
                                idxyear = vector())
for (i in 1:length(aware_agg_simul_male)) {
  a  = Agg_simul_aware(aware_agg_simul_male[[i]], sex = "male")
  print(max(a$propaware))
  if (any(a$propaware > 0.95)) {
    print(names(aware_agg_simul_male)[i])
    temp = data.frame(country = names(aware_agg_simul_male)[i],
                      idxyear = min(which(a$propaware > 0.95)))
    vector_male_miss95  = distinct(rbind(vector_male_miss95, temp))
  }
  
}

vector_male_miss95c = data.frame(country = vector(),
                                 idxyear = vector())

for (i in 1:length(aware_agg_simul_male_counter)) {
  a  = Agg_simul_aware(aware_agg_simul_male_counter[[i]], sex = "male")
  if (any(a$propaware > 0.95)) {
    print(names(aware_agg_simul_male_counter)[i])
    temp = data.frame(
      country = names(aware_agg_simul_male_counter)[i],
      idxyear = min(which(a$propaware > 0.95))
    )
    vector_male_miss95c  = distinct(rbind(vector_male_miss95c, temp))
  }
  
}


vector_female_miss95 = data.frame(country = vector(),
                                  idxyear = vector())
i = 14
for (i in 1:length(aware_agg_simul_female)) {
  a  = Agg_simul_aware(aware_agg_simul_female[[i]], sex = "female")
  print(names(aware_agg_simul_female)[i])
  print(max(a$propaware))
  if (any(a$propaware > 0.95)) {
    print(names(aware_agg_simul_female)[i])
    temp = data.frame(country = names(aware_agg_simul_female)[i],
                      idxyear = min(which(a$propaware > 0.95)))
    vector_female_miss95  = distinct(rbind(vector_female_miss95, temp))
  }
  
}

vector_female_miss95c = data.frame(country = vector(),
                                   idxyear = vector())
for (i in 1:length(aware_agg_simul_female_counter)) {
  a  = Agg_simul_aware(aware_agg_simul_female_counter[[i]], sex = "female")
  if (any(a$propaware > 0.95)) {
    print(names(aware_agg_simul_female_counter)[i])
    temp = data.frame(
      country = names(aware_agg_simul_female_counter)[i],
      idxyear = min(which(a$propaware > 0.95))
    )
    vector_female_miss95c  = distinct(rbind(vector_female_miss95c, temp))
  }
  
}

aggpooledFemalec
miss95 = vector_female_miss95c
miss95 = merge(miss95, vector_female_miss95, by = "country", all.x = T)

miss95 = merge(miss95, vector_male_miss95c, by = "country", all.x = T)
miss95 = merge(miss95, vector_male_miss95, by = "country", all.x = T)
names(miss95) = c("country", "femalecounter", "female", "malecounter", "male")

femalemiss95 = miss95[, 1:3]
femalemiss95$yearslost = femalemiss95$female - femalemiss95$femalecounter

malemiss95 = miss95[!is.na(miss95$malecounter), c(1, 4, 5)]
malemiss95$yearslost = malemiss95$male - malemiss95$malecounter

malemiss95$yearslost[is.na(malemiss95$yearslost)] = 999
femalemiss95$yearslost[is.na(femalemiss95$yearslost)] = 999


###----men-----
# 3. Prepare your decline data

# create dataframe with country name and hiv_decline
africa_anc_data <-
  data.frame(name = malemiss95$country,
             hiv_decline = unlist(malemiss95$yearslost))

# Merge with map data
africa_map <-
  africa_anc %>%
  left_join(africa_anc_data, by = c("name" = "name"))

vec_empty = vector()

i = 31
for (i in 1:length(make_country)) {
  if (is.null(make_country[[i]]$simul)) {
    vec_empty = c(vec_empty, names(make_country)[i])
  }
}
black_countries <-
  names(make_country)
black_countries = black_countries[!(black_countries %in% vec_empty)]

# Create a new column for color
africa_map$color <-
  ifelse(africa_map$name %in% black_countries, "black", NA)

black_countries[!(black_countries %in% africa_map$name)]

sum(!is.na(africa_map$hiv_decline))

countries <-
  c(
    "Angola",
    "Botswana",
    "Burundi",
    "Cameroon",
    "Côte d'Ivoire",
    "Dominican Republic",
    "Democratic Republic of the Congo",
    "Swaziland",
    "Ethiopia",
    "Haiti",
    "Kenya",
    "Lesotho",
    "Malawi",
    "Mozambique",
    "Namibia",
    "Nigeria",
    "Rwanda",
    "South Africa",
    "South Sudan",
    "Congo",
    "United Republic of Tanzania",
    "Uganda",
    "Ukraine",
    "Vietnam",
    "Zambia",
    "Zimbabwe"
  )

africa_map <-
  africa_map %>%
  mutate(
    category = case_when(
      !(name %in% black_countries) ~ "Selected Countries",
      is.na(hiv_decline) ~ "No Data",
      TRUE ~ "Gradient"
    ),
    pepfar = case_when(name %in% countries ~ "pepfar",
                       TRUE ~ "not"),
    # NEW: binning hiv_decline into discrete groups
    hiv_decline_category = factor(
      case_when(
        hiv_decline == 999 ~ "Did Not Reach",
        hiv_decline == 1 ~ "1 Years",
        hiv_decline == 2 ~ "2 Years",
        #hiv_decline == 3 ~ "3 years",
        hiv_decline == 0 ~ "Not Late"
        
      ),
      levels = c("1 Years", "2 Years", "Did Not Reach", "Not Late")
    )
  )


plot_africa_excess_years_men = ggplot() +
  
  # Other layers same as before
  geom_sf(
    data = africa_map %>% filter(category %in% c("No Data", "Selected Countries")),
    aes(fill = category),
    color = "black"
  ) +
  scale_fill_manual(
    #guide = F,
    name = NULL,
    values = c(
      "No Data" = "white",
      "Selected Countries" = "grey80"
    ),
    labels = c("No Data" = "Did Not Reach 95%\nin Counterfactual",
               "Selected Countries" = "Not Included"),
    guide = guide_legend(order = 2,
                         nrow = 2)
  ) +
  ggnewscale::new_scale_fill() +
  # Discrete binned layer
  geom_sf(
    data = africa_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline_category),
    color = "black"
  ) +
  
  
  
  scale_fill_manual(
    #guide = F,
    name = "Years \nLate",
    values = c(
      "1 Years" = "#fff7bc",
      "2 Years" = "#fec44f",
      "3 Years" = "#fe9929",
      "Did Not Reach" = "#cc0000",
      "Not Late" = "#66c2a4"
      
    ),
    guide = guide_legend(
      order = 1,
      title.position = "left",
      title.hjust = 0,
      nrow = 2
    ),
    drop = FALSE
  ) +
  
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.key = element_rect(size = 0.1),
    legend.position = "bottom"
    
  ) +
  guides(fill = guide_legend(
    order = 1,
    title.position = "left",
    title.hjust = 0,
    nrow = 2
  ))



plot_africa_excess_years_men

ggsave(
  plot = plot_africa_excess_years_men,
  file = paste0(path_out, "/plot_africa_excess_years_men.png"),
  width = 5,
  height = 5,
  dpi = 700
)


###---guide----
plot_africa_excess_years_men = ggplot() +
  
  # Discrete binned layer
  geom_sf(
    data = africa_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline_category),
    color = "black"
  ) +
  
  scale_fill_manual(
    name = "Years \nLate",
    values = c(
      "1 Years" = "#fff7bc",
      "2 Years" = "#fec44f",
      "3 Years" = "#fe9929",
      "Did Not Reach" = "#cc0000",
      "Not Late" = "#66c2a4"
      
    ),
    drop = FALSE
  ) +
  
  ggnewscale::new_scale_fill() +
  
  # Other layers same as before
  geom_sf(
    data = africa_map %>% filter(category %in% c("No Data", "Selected Countries")),
    aes(fill = category),
    color = "black"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "No Data" = "white",
      "Selected Countries" = "grey80"
    ),
    labels = c("No Data" = "Did not reach 95%\nin Counterfactual",
               "Selected Countries" = "Not Included")
  ) +
  
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.key = element_rect(size = 0.1)
  ) +
  
  labs(title = "Excess Years Before Countries Reached\nUNAIDS First 95 in Men Due to Declines")

plot_africa_excess_years_men

ggsave(
  plot = plot_africa_excess_years_men,
  file = paste0(path_out, "/plot_africa_excess_years_men guide12_30.png"),
  width = 5,
  height = 5,
  dpi = 700
)



###----women-----

# create dataframe with country name and hiv_decline
africa_anc_data <-
  data.frame(name = femalemiss95$country,
             hiv_decline = unlist(femalemiss95$yearslost))

# Merge with map data
africa_map <-
  africa_anc %>%
  left_join(africa_anc_data, by = c("name" = "name"))

vec_empty = vector()


for (i in 1:length(make_country)) {
  if (is.null(make_country[[i]]$simul)) {
    vec_empty = c(vec_empty, names(make_country)[i])
  }
}
black_countries <-
  names(make_country)
black_countries = black_countries[!(black_countries %in% vec_empty)]

# Create a new column for color
africa_map$color <-
  ifelse(africa_map$name %in% black_countries, "black", NA)

black_countries[!(black_countries %in% africa_map$name)]

sum(!is.na(africa_map$hiv_decline))

countries <-
  c(
    "Angola",
    "Botswana",
    "Burundi",
    "Cameroon",
    "Côte d'Ivoire",
    "Dominican Republic",
    "Democratic Republic of the Congo",
    "Swaziland",
    "Ethiopia",
    "Haiti",
    "Kenya",
    "Lesotho",
    "Malawi",
    "Mozambique",
    "Namibia",
    "Nigeria",
    "Rwanda",
    "South Africa",
    "South Sudan",
    "United Republic of Tanzania",
    "Uganda",
    "Ukraine",
    "Vietnam",
    "Zambia",
    "Zimbabwe"
  )

africa_map$hiv_decline
africa_map <-
  africa_map %>%
  mutate(
    category = case_when(
      !(name %in% black_countries) ~ "Selected Countries",
      is.na(hiv_decline) ~ "No Data",
      TRUE ~ "Gradient"
    ),
    pepfar = case_when(name %in% countries ~ "pepfar",
                       TRUE ~ "not"),
    # NEW: binning hiv_decline into discrete groups
    hiv_decline_category = case_when(
      hiv_decline == 999 ~ "Did Not Reach",
      hiv_decline == 1 ~ "1 Years",
      hiv_decline == 2 ~ "2 Years",
      hiv_decline == 3 ~ "3 Years",
      hiv_decline == 0 ~ "Not Late"
      
    )
  )

plot_africa_excess_years_women = ggplot() +
  
  # Discrete binned layer
  geom_sf(
    data = africa_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline_category),
    color = "black"
  ) +
  
  scale_fill_manual(
    #guide = F,
    name = "Years \nLate",
    values = c(
      "1 Years" = "#fff7bc",
      "2 Years" = "#fec44f",
      "3 Years" = "#fe9929",
      "Did Not Reach" = "#cc0000",
      "Not Late" = "#66c2a4"
      
    ),
    drop = FALSE
  ) +
  
  ggnewscale::new_scale_fill() +
  
  # Other layers same as before
  geom_sf(
    data = africa_map %>% filter(category %in% c("No Data", "Selected Countries")),
    aes(fill = category),
    color = "black"
  ) +
  scale_fill_manual(
    #guide = T,
    name = NULL,
    values = c(
      "No Data" = "white",
      "Selected Countries" = "grey80"
    ),
    labels = c("No Data" = "Did not reach 95%\nin Counterfactual",
               "Selected Countries" = "Not Included")
  ) +
  
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.key = element_rect(size = 0.1)
  ) +
  
  labs(title = "Excess Years Before Countries Reached\nUNAIDS First 95 in Women Due to Declines")

plot_africa_excess_years_women

ggsave(
  plot = plot_africa_excess_years_women,
  file = paste0(path_out, "/plot_africa_excess_years_women.png"),
  width = 5,
  height = 5,
  dpi = 700
)



aware_agg_simul_diff_Female


vector_female_miss95 = data.frame(country = vector(),
                                  idxyear = vector())
for (i in 1:length(aware_agg_simul_diff_Female)) {
  a  = Agg_simul_aware(aware_agg_simul_diff_Female[[i]], sex = "female")
  if (any(a$propaware > 0.95)) {
    print(names(aware_agg_simul_diff_Female)[i])
    temp = data.frame(
      country = names(aware_agg_simul_diff_Female)[i],
      idxyear = min(which(a$propaware > 0.95))
    )
    vector_female_miss95  = distinct(rbind(vector_female_miss95, temp))
  }
  
}
vector_male_miss95 = data.frame(country = vector(),
                                idxyear = vector())
list_am = list()
for (i in 1:length(aware_agg_simul_diff_Male)) {
  a  = Agg_simul_aware(aware_agg_simul_diff_Male[[i]], sex = "male")
  list_am[[names(aware_agg_simul_diff_Male)[i]]] = a
  if (any(a$propaware < 0)) {
    print(names(aware_agg_simul_diff_Male)[i])
    temp = data.frame(
      country = names(aware_agg_simul_diff_Male)[i],
      idxyear = min(which(a$propaware < 0), na.rm = T)
    )
    vector_male_miss95  = distinct(rbind(vector_male_miss95, temp))
  }
  
}
# ----unaware----
# make_country = readRDS("anc testing/data/make_country_simul_final.rds")
unaware_pre = as.data.frame(matrix(nrow = 465, ncol = 3000, data = 0))
unaware_counter_pre = as.data.frame(matrix(nrow = 465, ncol = 3000, data = 0))

for (i in 1:length(make_country)) {
  print(i)
  print(make_country[[i]]$cnt)
  
  if(is.null(make_country[[i]]$counter_simul) & is.null(make_country[[i]]$simul$unaware$unaware)) {
    print("skipping")
    next
  } else if(is.null(make_country[[i]]$simul$unaware$unaware)){
    make_country[[i]]$simul$unaware$unaware = make_country[[i]]$simul$unaware
  }
  unaware_pre = unaware_pre + make_country[[i]]$simul$unaware$unaware[, 6:3005]
  #print(make_country[[i]]$simul$unaware$unaware[140:149, 6])
  unaware_counter_pre = unaware_counter_pre + make_country[[i]]$counter_simul$unaware[, 6:3005]
  print((make_country[[i]]$simul$unaware$unaware[130:149, 6] - make_country[[i]]$counter_simul$unaware[130:149, 6]))
}

unaware = cbind(make_country[[i]]$simul$unaware$unaware[, 1:5], unaware_pre)
unaware_counter = cbind(make_country[[i]]$simul$unaware$unaware[, 1:5], unaware_counter_pre)

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
  (unaware_pre - unaware_counter_pre)/unaware_pre)),
 (sex == "both" & agegr == "15+"))

 
unaware_counter_b$count = "counter"
unaware_counter_b$mid = subset(cbind(
  unaware_counter[, (1:5)],
  apply(
    X = (unaware_pre - unaware_counter_pre)/unaware_pre,
    MARGIN = 1,
    FUN = stats::quantile,
    probs = c(0.5),
    na.rm = TRUE
  )
),
(sex == "both" &
   agegr == "15+"))[, 6]

unaware_counter_b[24,c(9,6,7)]

#find % increase in PLHIV due to declines
unaware_counter_b = subset(getci(cbind(
  unaware_counter[, (1:5)],
  (unaware_pre)/unaware_counter_pre)),
  (sex == "both" & agegr == "15+"))


unaware_counter_b$count = "counter"
unaware_counter_b$mid = subset(cbind(
  unaware_counter[, (1:5)],
  apply(
    X = (unaware_pre)/unaware_counter_pre,
    MARGIN = 1,
    FUN = stats::quantile,
    probs = c(0.5),
    na.rm = TRUE
  )
),
(sex == "both" &
   agegr == "15+"))[, 6]

unaware_counter_b[24,c(9,6,7)]

0.1254025 *8
# for abstract/paper male/female PLHIV prop
#find PLHIV prop due to declines
unaware_male_increase = subset(getci(cbind(
  unaware_counter[, (1:5)],
  (unaware_pre - unaware_counter_pre)/unaware_pre)),
  (sex == "male" & agegr == "15+"))


unaware_male_increase$mid = subset(cbind(
  unaware_counter[, (1:5)],
  apply(
    X = (unaware_pre - unaware_counter_pre)/unaware_pre,
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
  (unaware_pre - unaware_counter_pre)/unaware_pre)),
  (sex == "female" & agegr == "15+"))


unaware_female_increase$mid = subset(cbind(
  unaware_counter[, (1:5)],
  apply(
    X = (unaware_pre - unaware_counter_pre)/unaware_pre,
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

unaware = rbind(unaware_m, unaware_f)


start_year = 2015
end_year = 2023


unaware_plot = ggplot(unaware) +
  geom_line(aes(
    x = year,
    y = mid / 1e6,
    linetype = count,
    group = interaction(count, sex),
    colour = sex
  )) +
  geom_ribbon(aes(
    x = year,
    ymin = lower / 1e6,
    ymax = upper / 1e6,
    group = interaction(count, sex),
    fill = sex
  ),
  alpha = 0.4) +
  #geom_area(aes(x = year,y=upper,group = count, fill = count),alpha = 0.2)+
  scale_y_continuous(limits = c(0, 3e6 / 1e6)) +
  scale_x_continuous(limits = c(2015, 2023),
                     breaks = 2015:2023) +
  theme_minimal() +
  labs(
    title = paste0(
      "Number of Undiagnosed PLHIV\nPooled in Countries with a Decline:\n",
      start_year,
      "-",
      end_year
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
# guides(
#   fill = guide_legend(
#     order = 1,
#     title.position = "left",
#     title.hjust = 0,
#     nrow = 1
#   ),linetype = guide_legend(
#     order = 2,
#     title.position = "left",
#     title.hjust = 0,
#     nrow = 1
#   ),colour = guide_legend(
#     order = 1,
#     title.position = "left",
#     title.hjust = 0,
#     nrow = 1
#   ))
#

unaware_plot

ggsave(
  plot = unaware_plot,
  file = paste0(path_out, "/unaware legend.png"),
  width = 5,
  height = 5,
  dpi = 700
)

#use unaware tot
unaware_tot = as.data.frame(matrix(nrow = 465, ncol = 3006, data = 0))
unaware_tot = cbind(make_country[[i]]$simul$unaware$unaware[, 1:5],
                    unaware_pre - unaware_counter_pre)

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
      start_year,
      "-",
      end_year
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


unaware_plot_tot

ggsave(
  plot = unaware_plot_tot,
  file = paste0(path_out, "/unaware difference.png"),
  width = 5,
  height = 5,
  dpi = 700
)
# find proportion unaware
unaware_tot = as.data.frame(matrix(nrow = 465, ncol = 3006, data = 0))
unaware_tot = cbind(make_country[[i]]$simul$unaware$unaware[, 1:5],
                    unaware_pre - unaware_counter_pre)

unaware_obs_tot_b = subset(getci(unaware_tot), (sex == "both" &
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

