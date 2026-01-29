source("anc testing/0.6 time dx functions.R")
source("anc testing/1.1 tot test out.R")
path_out <- here::here("outputs/paper/TDX")

library(scales)  # Needed for formatting
library(first90)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggnewscale)

tdx_agg_simul_male_diff = list()
tdx_agg_simul_female_diff = list()
simul_tdxM = readRDS("outputs/2026 ttd/male tdx final observed")
simul_tdxF = readRDS("outputs/2026 ttd/female tdx final observed.rda")
simul_tdxB = readRDS("outputs/2026 ttd/both tdx final observed.rda")

simul_tdxMcount = readRDS("outputs/2026 ttd/male tdx final counter.rda")
simul_tdxFcount = readRDS("outputs/2026 ttd/female tdx final counter.rda")
simul_tdxBcount = readRDS("outputs/2026 ttd/both tdx final counter.rda")

tdx_agg_simul_male = simul_tdxM
tdx_agg_simul_female = simul_tdxF
tdx_agg_simul_both = simul_tdxB
tdx_agg_simul_male_counter = simul_tdxMcount
tdx_agg_simul_female_counter = simul_tdxFcount
tdx_agg_simul_both_counter = simul_tdxBcount

countries <- c(
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
  "Zimbabwe",
  "Burkina Faso",
  "Ghana",
  "Liberia",
  "Mali",
  "Sierra Leone",
  "Senegal",
  "Togo"
)
x = names(make_country)[(names(make_country) %in% countries)]
x = names(make_country)[!(names(make_country) %in% countries)]

x[!(x %in% names(tdx_agg_simul_both_counter))]



tdx_agg_simul_male_diff = list()
tdx_agg_simul_female_diff = list()
tdx_agg_simul_both_diff = list()
tdx_agg_simul_diff_diff = list()
tdx_agg_simul_diff_diff_obs = list()

for (i in 1:length(tdx_agg_simul_male)) {
  cnt = names(tdx_agg_simul_male)[i]
  print(cnt)
  if (!is.null(simul_tdxBcount[[cnt]])) {
    tryCatch(
      expr = {
        yearsim = 2015:2023
        
        diff_men = subset(simul_tdxM[[cnt]]$out_simul_tdx_all, year %in% yearsim)
        diff_women = subset(simul_tdxF[[cnt]]$out_simul_tdx_all, year %in% yearsim)
        diff_both = subset(simul_tdxB[[cnt]]$out_simul_tdx_all, year %in% yearsim)
        
        diff_diff = subset(simul_tdxB[[cnt]]$out_simul_tdx_all, year %in% yearsim)
        diff_diff_obs = subset(simul_tdxB[[cnt]]$out_simul_tdx_all, year %in% yearsim)
        
        
        
        diff_men$time_dx_med = (
          subset(simul_tdxM[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5] - subset(simul_tdxMcount[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5]
        )$time_dx_med
        diff_women$time_dx_med = (
          subset(simul_tdxF[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5] - subset(simul_tdxFcount[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5]
        )$time_dx_med
        diff_both$time_dx_med = (
          subset(simul_tdxB[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5] - subset(simul_tdxBcount[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5]
        )$time_dx_med
        
        diff_diff$time_dx_med = (
          subset(simul_tdxMcount[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5]$time_dx_med -
            subset(simul_tdxFcount[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5]
        )$time_dx_med
        
        diff_diff_obs$time_dx_med = (
          subset(simul_tdxM[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5]$time_dx_med -
            subset(simul_tdxF[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 5]
        )$time_dx_med
        
        quantile(diff_women$time_dx_med, c(0.95, 0.5, 0.05))
        
        quantile(diff_men$time_dx_med, c(0.95, 0.5, 0.05))
        
        
        tdx_agg_simul_male_diff[[cnt]]$out_simul_tdx_all = diff_men
        tdx_agg_simul_female_diff[[cnt]]$out_simul_tdx_all = diff_women
        tdx_agg_simul_both_diff[[cnt]]$out_simul_tdx_all = diff_both
        
        tdx_agg_simul_diff_diff[[cnt]]$out_simul_tdx_all = diff_diff
        tdx_agg_simul_diff_diff_obs[[cnt]]$out_simul_tdx_all = diff_diff_obs
        
        
      },
      error = function(e) {
        message("Caught an error: ", e$message)
        
      },
      finally = {
        
      }
    )
  }
  
}

pepfar_male_ttd = list()
pepfar_female_ttd = list()
pepfar_both_ttd = list()
nonpepfar_both_ttd = list()
nonpepfar_male_ttd = list()
nonpepfar_female_ttd = list()
sort(names(pepfar_male_ttd))
sort(names(nonpepfar_male_ttd))

for (i in 1:length(tdx_agg_simul_male_diff)) {
  cnt = names(tdx_agg_simul_male_diff)[i]
  if (cnt %in% countries) {
    pepfar_male_ttd[[cnt]] = tdx_agg_simul_male_diff[[cnt]]
    pepfar_female_ttd[[cnt]] = tdx_agg_simul_female_diff[[cnt]]
    pepfar_both_ttd[[cnt]] = tdx_agg_simul_both_diff[[cnt]]
    
  } else{
    nonpepfar_male_ttd[[cnt]] = tdx_agg_simul_male_diff[[cnt]]
    nonpepfar_female_ttd[[cnt]] = tdx_agg_simul_female_diff[[cnt]]
    nonpepfar_both_ttd[[cnt]] = tdx_agg_simul_both_diff[[cnt]]
    
  }
  
}


male_agg_diff = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_diff, sex = "male")
female_agg_diff = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_diff, sex = "female")
both_agg_diff = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_both_diff, sex = "male+female")
diff_agg_diff = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_diff_diff, sex = "male+female")
diff_agg_diff_obs = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_diff_diff_obs, sex = "male+female")



pepfarM = Agg_simul_pool_time_dx_prev_2(pepfar_male_ttd, sex = "male")
pepfarF = Agg_simul_pool_time_dx_prev_2(pepfar_female_ttd, sex = "female")
pepfarB = Agg_simul_pool_time_dx_prev_2(pepfar_both_ttd, sex = "male+female")

non_pepfarM = Agg_simul_pool_time_dx_prev_2(nonpepfar_male_ttd, sex = "male")
non_pepfarF = Agg_simul_pool_time_dx_prev_2(nonpepfar_female_ttd, sex = "female")
non_pepfarB = Agg_simul_pool_time_dx_prev_2(nonpepfar_both_ttd, sex = "male+female")

#isolate to only countries with a decline

tdx_agg_simul_male_subset = list()
tdx_agg_simul_female_subset = list()
tdx_agg_simul_both_subset = list()

for (i in 1:length(tdx_agg_simul_male_counter)) {
  cnt = names(tdx_agg_simul_male_counter) [i]
  tdx_agg_simul_male_subset[[cnt]] = simul_tdxM[[cnt]]
  tdx_agg_simul_female_subset[[cnt]] = simul_tdxF[[cnt]]
  tdx_agg_simul_both_subset[[cnt]] = simul_tdxB[[cnt]]
}



male_agg = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_subset, sex = "male")
female_agg = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_subset, sex = "female")
both_agg = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_both_subset, sex = "male+female")

male_agg_counter = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_counter, sex = "male")
female_agg_counter = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_counter, sex = "female")
both_agg_counter = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_both_counter, sex = "male+female")

both_no_decline_agg = Agg_simul_pool_time_dx_prev_2(simul_tdxB, sex = "male+female")

male_agg$time_dx - female_agg$time_dx

# ----plot male_female ttd----
start_year = 2015
male_colour = "steelblue"
female_colour = "firebrick"
end_year = 2023
#install.packages("ggpattern")
#library(ggpattern)

# Define sex setting
sex <- "female"  # Options: "male", "female", "both"

if (sex == "female") {
  col = "firebrick3"
  col2 = "steelblue"
  sex2 = "male"
} else{
  col = "steelblue"
  col2 = "firebrick3"
  sex2 = "female"
}

# Dynamically construct the variable name
var_name <- paste0(sex, "_agg")
var_name2 <- paste0(sex, "_agg_counter")

var_namea = paste0(sex2, "_agg")
var_namea2 = paste0(sex2, "_agg_counter")

plot = ggplot() +
  geom_ribbon(
    data = get(var_name),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_name),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_name2),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_name2),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1,
    linetype = 2
  ) +
  
  
  geom_ribbon(
    data = get(var_namea),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col2,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_namea),
    aes(y = time_dx, x = year),
    color = col2,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_namea2),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col2,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_namea2),
    aes(y = time_dx, x = year),
    color = col2,
    linewidth = 1,
    linetype = 2
  ) +
  theme_minimal() +
  labs(
    title = paste0(
      "Pooled Median Time to Diagnosis or AIDS\nDeath ",
      "by Sex in Countries With a Decline:\n",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Years to Diagnosis or AIDS Death"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 4)) +
  #max(tdx_female$time_dx_med[tdx_female$year >= start_year]))))+
  # #Add legend for Line 1
  # annotate("segment", x = 2019.4,xend = 2020,y = 0.5,yend = 0.5 ,color = "steelblue", linewidth = 1)+
  # annotate("segment", x = 2020.3,xend = 2020.9,y = 0.5,yend = 0.5 ,color = female_colour, linewidth = 1)+
  # annotate("segment", x = 2019.4,xend = 2020,y = 0.7,yend = 0.7 ,color = "steelblue", linewidth = 1,linetype = "dashed")+
  # annotate("segment", x = 2020.3,xend = 2020.9,y = 0.7,yend = 0.7 ,color = female_colour, linewidth = 1, linetype = "dashed")+
  # annotate("text", x = 2019.3, y = 0.9, label = "Male", color = "steelblue", hjust = 0, vjust = 0.5)+
  # annotate("text", x = 2020.3, y = 0.9, label = "Female", color = "firebrick3", hjust = 0, vjust = 0.5)+
  # annotate("text", x = 2021.1, y = 0.5, label = "Observed", color = "black", hjust = 0, vjust = 0.5)+
  # annotate("text", x = 2021.1, y = 0.7, label = "Counterfactual", color = "black", hjust = 0, vjust = 0.5)+
  #geom_vline(xintercept = 2018, linetype = "dashed", color = "black", size = 0.5)+
#geom_vline(xintercept = 2016, linetype = "dashed", color = "black", size = 0.1)+
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
  file = paste0(path_out, "/_tdx_paper male_female.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)







# ----map both----

geo_ttd_both = matrix(nrow = 9, ncol = 26)
geo_ttd_both = as.data.frame(geo_ttd_both)
geo_ttd_both[, 1] = 2015:2023

for (i in 1:length(tdx_agg_simul_female_diff)) {
  cnt = names(tdx_agg_simul_female_diff)[i]
  
  
  agg_temp =  Agg_simul_pool_time_dx_prev(tdx_agg_simul_both_diff[[cnt]]$out_simul_tdx_all, sex = "male+female")
  geo_ttd_both[, i + 1] = agg_temp$time_dx
}
names(geo_ttd_both) = c("year", names(tdx_agg_simul_female_diff))

geo_ttd_both[geo_ttd_both < 0] = 0
geo_ttd_both[geo_ttd_both == 0] = NA


# Get the world map (western sahara missing to align with UNAIDS)
world <-
  read_sf("ne_10m_admin_0_countries_vnm/ne_10m_admin_0_countries_vnm.shp")
world$name = world$NAME

# Filter for africa_anc
africa_anc <- world[world$CONTINENT == "Africa",]

#adjust names to match with those from package( ensure your match up too)
africa_anc$name[africa_anc$name == "Dem. Rep. Congo"] = "Democratic Republic of the Congo"
africa_anc$name[africa_anc$name == "S. Sudan"] = "South Sudan"
africa_anc$name[africa_anc$name == "Tanzania"] = "United Republic of Tanzania"
africa_anc$name[africa_anc$name == "Cabo Verde"] = "Cape Verde"
africa_anc$name[africa_anc$name == "Republic of the Congo"] = "Congo"



# create dataframe with country name and hiv_decline
africa_both_data2015 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[1, 2:dim(geo_ttd_both)[2]]))
# create dataframe with country name and hiv_decline
africa_both_data2016 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[2, 2:dim(geo_ttd_both)[2]]))
# create dataframe with country name and hiv_decline
africa_both_data2017 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[3, 2:dim(geo_ttd_both)[2]]))
# create dataframe with country name and hiv_decline
africa_both_data2018 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[4, 2:dim(geo_ttd_both)[2]]))
# create dataframe with country name and hiv_decline
africa_both_data2019 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[5, 2:dim(geo_ttd_both)[2]]))
# create dataframe with country name and hiv_decline
africa_both_data2020 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[6, 2:dim(geo_ttd_both)[2]]))
# create dataframe with country name and hiv_decline
africa_both_data2021 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[7, 2:dim(geo_ttd_both)[2]]))
# create dataframe with country name and hiv_decline
africa_both_data2022 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[8, 2:dim(geo_ttd_both)[2]]))
# create dataframe with country name and hiv_decline
africa_both_data2023 <-
  data.frame(name = names(geo_ttd_both[, 2:dim(geo_ttd_both)[2]]),
             hiv_decline = unlist(geo_ttd_both[9, 2:dim(geo_ttd_both)[2]]))

for (j in 1:length(geo_ttd_both$year)) {
  year1 = geo_ttd_both$year[j]
  # Merge with map data
  africa_map <- africa_anc %>%
    left_join(get(paste0("africa_both_data", year1)), by = c("name" = "name"))
  
  vec_empty = vector()
  
  
  for (i in 1:length(make_country)) {
    if (is.null(make_country[[i]]$simul)) {
      vec_empty = c(vec_empty, names(make_country)[i])
    }
  }
  black_countries <- names(make_country)
  black_countries = black_countries[!(black_countries %in% vec_empty)]
  
  # Create a new column for color
  africa_map$color <-
    ifelse(africa_map$name %in% black_countries, "black", NA)
  
  black_countries[!(black_countries %in% africa_map$name)]
  
  sum(!is.na(africa_map$hiv_decline))
  
  countries <- c(
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
  
  
  africa_map <- africa_map %>%
    mutate(
      category = case_when(
        !(name %in% black_countries) ~ "Selected Countries",
        is.na(hiv_decline) ~ "No Data",
        
        TRUE ~ "Gradient"
      ),
      pepfar = case_when(name %in% countries ~ "pepfar",
                         TRUE ~ "not")
    )
  africa_map$category
  
  plot_africa_ttd = ggplot() +
    # Gradient layer for hiv_decline
    geom_sf(
      data = africa_map %>% filter(category == "Gradient"),
      aes(fill = hiv_decline),
      color = "black"
    ) +
    scale_fill_viridis_c(
      option = "magma",
      na.value = "white",
      name = "Additional \nyears to\ndiagnosis ",
      direction = -1,
      limits = c(0, 3.1),
      breaks = c(seq(0, 3.1, by = 0.6)),
      guide = F
      
    ) +
    
    # New fill scale for categorical values
    ggnewscale::new_scale_fill() +
    
    # Categorical layers (No Data & Selected Countries)
    geom_sf(
      data = africa_map %>% filter(category %in% c("No Data", "Selected Countries")),
      aes(fill = category),
      # Map fill to category
      color = "black"
    ) +
    scale_fill_manual(
      guide = F,
      name = NULL,
      values = c(
        "No Data" = "white",
        "Selected Countries" = "grey80"
      ),
      labels = c("No Data" = "No Decline",
                 "Selected Countries" = "No Data")
    ) +
    geom_sf(
      data = africa_map %>% filter(pepfar == "pepfar"),
      aes(color = pepfar),
      linewidth = 1,
      fill = NA
    ) +
    
    # Define the red outline color
    scale_color_manual(
      name = NULL,
      values = c("pepfar" = "NA"),
      labels = c("pepfar" = NULL),
      guide = NULL  # Remove legend if unnecessary
    ) +
    
    # Theme and labels
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      #legend.background = element_blank(),legend.key = element_blank()
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        # Center the title
        size = 18           # Adjust the font size
      ),
      #legend.key = element_rect(size = 0.1),
      #legend.key.size = c(2,2)
      
    ) +
    
    #labs(title = paste0("Difference in Time to Diagnosis or AIDS death between Counterfactual and Observed in ",year1))+
    labs(title = year1) +
    theme()
  plot_africa_ttd
  
  ggsave(
    plot = plot_africa_ttd,
    file = paste0(
      path_out,
      "/time to diagnosis africa/plot_africa_ttd_both_",
      year1,
      " 1 4.png"
    ),
    width = 5.5,
    height = 6.2,
    dpi = 700
  )
  
}

## map axis
year1 = geo_ttd_both$year[j]
# Merge with map data
africa_map <- africa_anc %>%
  left_join(get(paste0("africa_both_data", year1)), by = c("name" = "name"))

vec_empty = vector()


for (i in 1:length(make_country)) {
  if (is.null(make_country[[i]]$simul)) {
    vec_empty = c(vec_empty, names(make_country)[i])
  }
}
black_countries <- names(make_country)
black_countries = black_countries[!(black_countries %in% vec_empty)]

# Create a new column for color
africa_map$color <-
  ifelse(africa_map$name %in% black_countries, "black", NA)

black_countries[!(black_countries %in% africa_map$name)]

sum(!is.na(africa_map$hiv_decline))

countries <- c(
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
north_Africa = c("W. Sahara", "Morocco", "Libya", "Tunisia", "Egypt", "Algeria")

africa_map <- africa_map %>%
  mutate(
    category = case_when(
      (name %in% north_Africa) ~ "North africa",!(name %in% black_countries) ~ "Selected Countries",
      is.na(hiv_decline) ~ "No Data",
      
      TRUE ~ "Gradient"
    ),
    pepfar = case_when(name %in% countries ~ "pepfar",
                       TRUE ~ "not")
  )
africa_map$category

plot_africa_ttd = ggplot() +
  # Gradient layer for hiv_decline
  geom_sf(
    data = africa_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline),
    color = "black"
  ) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "white",
    name = "Additional \nyears to\ndiagnosis ",
    direction = -1,
    limits = c(0, 3.1),
    breaks = c(seq(0, 3.1, by = 0.6))
    
  ) +
  
  # New fill scale for categorical values
  ggnewscale::new_scale_fill() +
  
  # Categorical layers (No Data & Selected Countries)
  geom_sf(
    data = africa_map %>% filter(category %in% c("No Data", "Selected Countries")),
    aes(fill = category),
    # Map fill to category
    color = "black"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "No Data" = "white",
      "Selected Countries" = "grey80",
      "North africa" = "grey70"
      
    ),
    labels = c("No Data" = "No Decline",
               "Selected Countries" = "Not Included")
  ) +
  geom_sf(
    data = africa_map %>% filter(pepfar == "pepfar"),
    aes(color = pepfar),
    linewidth = 1,
    fill = NA
  ) +
  
  # Define the red outline color
  scale_color_manual(
    name = NULL,
    values = c("pepfar" = "NA"),
    labels = c("pepfar" = NULL),
    guide = NULL  # Remove legend if unnecessary
  ) +
  
  # Theme and labels
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      # Center the title
      size = 16,
      legend.key = element_rect(size = 0.1),
    )
  ) +
  labs(title = year1) +
  theme()
plot_africa_ttd

ggsave(
  plot = plot_africa_ttd,
  file = paste0(path_out, "/time to diagnosis africa/plot legend.png"),
  width = 10,
  height = 7,
  dpi = 700
)



# ----plot female diff ttd----
start_year = 2015
male_colour = "steelblue"
female_colour = "firebrick"
end_year = 2023

# Define sex setting
sex <- "female"  # Options: "male", "female", "both"

if (sex == "female") {
  col = "firebrick3"
  col2 = "steelblue"
  sex2 = "male"
} else{
  col = "steelblue"
  col2 = "firebrick3"
  sex2 = "female"
}

# Dynamically construct the variable name
var_name <- paste0(sex, "_agg_diff")

var_namea = paste0(sex2, "_agg_diff")

plot = ggplot() +
  geom_ribbon(
    data = get(var_name),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_name),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1
  ) +
  
  geom_ribbon(
    data = get(var_namea),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col2,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_namea),
    aes(y = time_dx, x = year),
    color = col2,
    linewidth = 1
  ) +
  theme_minimal() +
  labs(
    title = paste0(
      "Additional Years to Diagnosis or AIDS\n ",
      "Death by Sex in Countries With a Decline:\n",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Years to Diagnosis or AIDS Death"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(-0.1, 1.01)) +
  
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
  file = paste0(path_out, "/_tdx_paper male_female diff.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)

#---- pepfar ----

pepfarM
non_pepfarM
pepfarF
non_pepfarF
data <- data.frame(
  Country = c("**Pepfar**", "**non-Pepfar**"),
  CentralEstimate = c(pepfarB[9, 4:6][[1]], non_pepfarB[9, 4:6][[1]]),
  LowerCI = c(pepfarB[9, 4:6][[2]], non_pepfarB[9, 4:6][[2]]),
  UpperCI = c(pepfarB[9, 4:6][[3]], non_pepfarB[9, 4:6][[3]])
)


data[4, 2:4] = 0



plot1 = ggplot(data, aes(x = Country, y = CentralEstimate)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI),
                width = 0.15,
                color = "red") +  # Error bars for 95% CI
  geom_point(size = 3, color = "black") +  # Central estimate as a point
  theme_minimal() +
  labs(title = "Additional Years of Median Time to Diagnosis or\nAIDS Death in PEPFAR and non-PEPFAR Countries in 2023",
       x = NULL,
       y = "Additional Years of Median Time to Diagnosis or AIDS Death") +
  # caption = paste0("Footnote1: PEPFAR countries:\n",
  #                  # stringr::str_wrap(paste(names(tdx_agg_simul_male)[names(tdx_agg_simul_male)%in%countries][1:5], collapse = ", "),width = 80),"\n",
  #                  stringr::str_wrap(paste(sort(names(pepfar_male_ttd)), collapse = ", "),width = 120),"\n",
  #                  "\nFootnote2: NON-PEPFAR countries:\n",
  #                  # stringr::str_wrap(paste(names(tdx_agg_simul_male)[!(names(tdx_agg_simul_male)%in%countries)][1:5], collapse = ", "),width = 10),"\n",
  #                  stringr::str_wrap(paste(sort(names(nonpepfar_male_ttd)), collapse = ", "),width = 120)
  # ))+
  # annotate("text", x = 1, y = 0.55, label = "non-PEPFAR", size = 5) +
  # annotate("text", x = 2, y = 0.55, label = "PEPFAR", size = 5) +
  # geom_segment(aes(x = 0.7, xend = 1.3, y = 0.5, yend = 0.5), size = 0.5) + # Line for PEPFAR
  # geom_segment(aes(x = 1.7, xend = 2.3, y = 0.5, yend = 0.5), size = 0.5) + # Line for PEPFAR
scale_y_continuous(limits = c(-0.09, 0.6)) +
  scale_x_discrete(labels = c("NON-PEPFAR", "PEPFAR")) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 10),
    # Increase text size and make bold
    axis.text.y = element_text(size = 10),
    # Increase text size and make bold
    axis.title.y = element_text(size = 10)  # Increase text size and make bold
    
  )


pepfarM
non_pepfarM
pepfarF
non_pepfarF

data <- data.frame(
  Country = c(
    "**Pepfar Male**",
    "**Pepfar Female**",
    "**non-Pepfar Male**",
    "**non-Pepfar Female**"
  ),
  CentralEstimate = c(pepfarM[9, 4:6][[1]], pepfarF[9, 4:6][[1]], non_pepfarM[9, 4:6][[1]], non_pepfarF[9, 4:6][[1]]),
  LowerCI = c(pepfarM[9, 4:6][[2]], pepfarF[9, 4:6][[2]], non_pepfarM[9, 4:6][[2]], non_pepfarF[9, 4:6][[2]]),
  UpperCI = c(pepfarM[9, 4:6][[3]], pepfarF[9, 4:6][[3]], non_pepfarM[9, 4:6][[3]], non_pepfarF[9, 4:6][[3]])
)


plot1 = ggplot(data, aes(x = Country, y = CentralEstimate)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI),
                width = 0.1,
                color = "red") +  # Error bars for 95% CI
  geom_point(size = 3, color = "black") +  # Central estimate as a point
  theme_minimal() +
  labs(title = "Additional Years of Median Time to Diagnosis\n or AIDS Death in PEPFAR and non-PEPFAR\nCountries in 2023",
       x = NULL,
       y = "Additional Years of Median Time to Diagnosis or AIDS Death") +
  
  #   caption = paste0("Footnote1: PEPFAR countries:\n",
  #                    # stringr::str_wrap(paste(names(tdx_agg_simul_male)[names(tdx_agg_simul_male)%in%countries][1:5], collapse = ", "),width = 80),"\n",
  #                    stringr::str_wrap(paste(sort(names(tdx_agg_simul_male)[names(tdx_agg_simul_male)%in%countries][1:14]), collapse = ", "),width = 120),"\n",
  #                    "\nFootnote2: NON-PEPFAR countries:\n",
  #                    # stringr::str_wrap(paste(names(tdx_agg_simul_male)[!(names(tdx_agg_simul_male)%in%countries)][1:5], collapse = ", "),width = 10),"\n",
  #                    stringr::str_wrap(paste(sort(names(tdx_agg_simul_male)[!(names(tdx_agg_simul_male)%in%countries)][1:10]), collapse = ", "),width = 120)
  #   ))+
  annotate(
    "text",
    x = 1.5,
    y = 1.4,
    label = "non-PEPFAR",
    size = 5,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 3.5,
    y = 1.4,
    label = "PEPFAR",
    size = 5,
    fontface = "bold"
  ) +
  geom_segment(aes(
    x = 0.8,
    xend = 2.2,
    y = 1.35,
    yend = 1.35
  ), size = 0.5) + # Line for PEPFAR
  geom_segment(aes(
    x = 2.8,
    xend = 4.2,
    y = 1.35,
    yend = 1.35
  ), size = 0.5) + # Line for PEPFAR
  scale_y_continuous(limits = c(0, 1.40)) +
  scale_x_discrete(labels = c("Females", "Males", "Females", "Males")) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0),
    plot.title = element_text(size = 16, hjust = 0.6, face = "bold"),
    axis.text.x = element_text(size = 15, face = "bold"),
    # Increase text size and make bold
    axis.text.y = element_text(size = 15, face = "bold"),
    # Increase text size and make bold
    axis.title.y = element_text(
      size = 15,
      face = "bold",
      vjust = 2,
      hjust = 0.3
    )  # Increase text size and make bold
    
  )


plot1



ggsave(
  plot = plot1,
  file = paste0(
    path_out,
    "/pepfar vs non-pepfar additional years of decline 1 4.png"
  ),
  dpi = 500,
  height = 5,
  width = 5,
  scale = 1
)

pepfar_male_ttd$`South Africa`

male_agg_counter$time_dx - female_agg_counter$time_dx







# ----plot male ttd----
start_year = 2015
male_colour = "steelblue"
female_colour = "firebrick"
end_year = 2023
#install.packages("ggpattern")
#library(ggpattern)

# Define sex setting
sex <- "male"  # Options: "male", "female", "both"

if (sex == "female") {
  col = "steelblue"
  col2 = "steelblue"
  sex2 = "male"
} else{
  col = "steelblue"
  col2 = "steelblue"
  sex2 = "male"
}

# Dynamically construct the variable name
var_name <- paste0(sex, "_agg")
var_name2 <- paste0(sex, "_agg_counter")

var_namea = paste0(sex2, "_agg")
var_namea2 = paste0(sex2, "_agg_counter")

plot = ggplot() +
  geom_ribbon(
    data = get(var_name),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_name),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_name2),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_name2),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1,
    linetype = 2
  ) +
  
  
  geom_ribbon(
    data = get(var_namea),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col2,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_namea),
    aes(y = time_dx, x = year),
    color = col2,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_namea2),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col2,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_namea2),
    aes(y = time_dx, x = year),
    color = col2,
    linewidth = 1,
    linetype = 2
  ) +
  theme_minimal() +
  labs(
    title = paste0(
      "Pooled Median Time to Diagnosis or AIDS Death\n ",
      "by Gender in Countries With a Decline: ",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Years to Diagnosis or AIDS Death"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 4)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    # Center the title,
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    
    
  )
plot


ggsave(
  plot = plot,
  file = paste0(path_out, "/_tdx_paper male.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)


# ----plot male_female ttd----
start_year = 2015
male_colour = "steelblue"
female_colour = "firebrick"
end_year = 2023
#install.packages("ggpattern")
#library(ggpattern)

# Define sex setting
sex <- "female"  # Options: "male", "female", "both"

if (sex == "female") {
  col = "firebrick3"
  col2 = "firebrick3"
  sex2 = "female"
} else{
  col = "firebrick3"
  col2 = "firebrick3"
  sex2 = "female"
}

# Dynamically construct the variable name
var_name <- paste0(sex, "_agg")
var_name2 <- paste0(sex, "_agg_counter")

var_namea = paste0(sex2, "_agg")
var_namea2 = paste0(sex2, "_agg_counter")

plot = ggplot() +
  geom_ribbon(
    data = get(var_name),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_name),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_name2),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_name2),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1,
    linetype = 2
  ) +
  
  
  geom_ribbon(
    data = get(var_namea),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col2,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_namea),
    aes(y = time_dx, x = year),
    color = col2,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_namea2),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col2,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_namea2),
    aes(y = time_dx, x = year),
    color = col2,
    linewidth = 1,
    linetype = 2
  ) +
  theme_minimal() +
  labs(
    title = paste0(
      "Pooled Median Time to Diagnosis or AIDS Death\n ",
      "by Gender in Countries With a Decline: ",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Years to Diagnosis or AIDS Death"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 4)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    # Center the title,
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    
    
  )
plot


ggsave(
  plot = plot,
  file = paste0(path_out, "/_tdx_paper female.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)


# ----plot both ttd----
start_year = 2015
male_colour = "steelblue"
female_colour = "firebrick"
end_year = 2023
#install.packages("ggpattern")
#library(ggpattern)

# Define sex setting
sex <- "both"  # Options: "male", "female", "both"

if (sex == "both") {
  col = "cyan4"
  col2 = "firebrick3"
  sex2 = "female"
} else{
  col = "firebrick3"
  col2 = "firebrick3"
  sex2 = "female"
}

# Dynamically construct the variable name
var_name <- paste0(sex, "_agg")
var_name2 <- paste0(sex, "_agg_counter")


plot = ggplot() +
  geom_ribbon(
    data = get(var_name),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_name),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_name2),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_name2),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1,
    linetype = 2
  ) +
  
  
  theme_minimal() +
  labs(
    title = paste0(
      "Pooled Median Time to Diagnosis or AIDS Death\n ",
      "in Countries With a Decline: ",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Years to Diagnosis or AIDS Death"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 4)) +
  theme(
    plot.title = element_text(hjust = 0.7, size = 16, face = "bold"),
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
  file = paste0(path_out, "/_tdx_paper both_decline_ 1 4.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)




# ----plot both no decline ttd----
start_year = 2015
male_colour = "steelblue"
female_colour = "firebrick"
end_year = 2023
#install.packages("ggpattern")
#library(ggpattern)

# Define sex setting
sex <- "both"  # Options: "male", "female", "both"

if (sex == "both") {
  col = "cyan3"
  col2 = "firebrick3"
  sex2 = "female"
} else{
  col = "firebrick3"
  col2 = "firebrick3"
  sex2 = "female"
}

# Dynamically construct the variable name
var_name <- paste0(sex, "_no_decline_agg")
var_name2 <- paste0(sex, "_agg_counter")


plot = ggplot() +
  geom_ribbon(
    data = get(var_name),
    aes(
      x = year,
      ymin = time_dx_lci,
      ymax = time_dx_uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_name),
    aes(y = time_dx, x = year),
    color = col,
    linewidth = 1
  ) +
  
  theme_minimal() +
  labs(
    title = paste0(
      "Pooled Median Time to Diagnosis or AIDS Death\n ",
      "in Countries With a Decline: ",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Years to Diagnosis or AIDS Death"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 4)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    # Center the title,
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    
    
  )
plot


ggsave(
  plot = plot,
  file = paste0(path_out, "/_tdx_paper both_decline.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)
