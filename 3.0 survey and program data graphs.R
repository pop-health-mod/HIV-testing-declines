library(stringi)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggh4x)
library(forcats)

path_anc <- here::here("anc testing")
make_country = readRDS(paste0(path_anc, "/data/make_country_simul_final.rds"))


# make survey program and ANC program data/ pmtct data avilibility graphs
testtype = c("tot", "vct", "anc")
prgpres = data.frame(year = vector(),
                     Modality = vector(),
                     country = vector())
for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  print(cnt)
  # some countries have sex stratification (and include both, i.e. male, female, both)
  # this code ensure no double counting
  prg <- make_country[[i]]$prgm_dat %>%
    group_by(country, year) %>%
    summarise(across(everything(), ~ {
      if ("both" %in% sex) {
        .[sex == "both"][1]
      } else if (is.numeric(.)) {
        sum(.[sex %in% c("male", "female")], na.rm = TRUE)
      } else if (cur_column() == "sex") {
        "both"
      } else {
        first(.[sex %in% c("male", "female")])
      }
    }),
    .groups = "drop")
  
  prg = prg[, c("year", "tot", "anc", "vct")]
  # no true 0's only artifacts from processing
  prg[prg == 0] <- NA
  temp = prgpres[0,]
  
  # selects for years with each modality - checks for stratified/unstratified by anc
  for (test in testtype) {
    temp_mod = prgpres[0,]
    if (length(prg$year[!is.na(prg[, test])]) > 0) {
      temp_mod[1:length(prg$year[!is.na(prg[, test])]), 1] = prg$year[!is.na(prg[, test])]
      temp_mod$Modality = test
      temp_mod$country = cnt
      
      temp = rbind(temp, temp_mod)
    }
  }
  for (year in temp$year) {
    if (length(temp$Modality[temp$year == year]) == 3) {
      temp$Modality[(temp$year == year & temp$Modality == "tot")] = "vct"
      temp = distinct(temp)
    }
    if (length(temp$Modality[temp$year == year]) == 2 &&
        "tot" %in% temp$Modality[temp$year == year]) {
      temp$Modality[(temp$year == year & temp$Modality == "vct")] = "tot"
      temp = distinct(temp)
      
    }
  }
  prgpres = rbind(prgpres, temp)
  
}

#do this before rbind, finds total years
subset(prgpres, Modality %in% c("tot", "vct")) %>%
  count(country) %>%
  select(n) %>%
  sum()

#and finds info on antenatal care years
subset(prgpres, Modality %in% c("anc")) %>%
  count(country) %>%
  select(n) %>%
  sum()

#required for next step to make detecting discontinuities way easier
prgpres = rbind(prgpres, prgpres)

# this segenebts tge graog by modality
df_segments <- prgpres %>%
  arrange(country, Modality, year) %>%
  group_by(country, Modality) %>%
  mutate(gap = year - lag(year, default = first(year) - 1),
         block = cumsum(gap != 1)) %>%
  group_by(country, Modality, block) %>%
  summarise(
    year_start = min(year),
    year_end = min(year + 1, 2023),
    .groups = "drop"
  )


Modality_offset <- data.frame(Modality = c("tot", "vct", "anc"),
                              offset = c(0, 0.1, 0))  # adjust spacing as needed

df_segments <- df_segments %>%
  left_join(Modality_offset, by = "Modality") %>%
  mutate(y_pos = as.numeric(factor(country)) + offset)

country_base_y <-
  data.frame(country = unique(df_segments$country),
             y_base = as.numeric(factor(unique(
               df_segments$country
             ))))

df_segments <- df_segments %>%
  left_join(country_base_y, by = "country") %>%
  mutate(y_pos = (y_base + offset) * 1.5)

df_segments = subset(df_segments, Modality != "vct")

# surveys
survey_type <- function(survey_code) {
  dplyr::case_when(
    grepl("\\d(?:AIS|DHS)$", survey_code) ~ "DHS",
    grepl("MICS",    survey_code) ~ "MICS",
    grepl("PHIA",    survey_code) ~ "PHIA",
    TRUE                         ~ "other"
  )
}


#find participant numbers
particpants = 0
for (cnt in names(make_country)) {
  survey_hts = make_country[[cnt]]$survey_hts
  print(cnt)
  print(sum(survey_hts$counts), na.rm = T)
  survey_hts[159, ]
  survey_hts <- survey_hts %>%
    mutate(
      se2 = ifelse(is.na(se) | se <= 0, (ci_u - ci_l) / (2 * 1.96), se),
      n_eff = est * (1 - est) / (se2 ^ 2),
      counts = ifelse(is.na(counts) |
                        counts <= 0, n_eff, counts)
    ) %>%
    select(-se2)
  
  surv2 = (select_hts(
    survey_hts,
    cnt = cnt,
    age_group = c('15-24', '25-34', '35-49', '50+')
  ))[, c("surveyid", "year", "sex", "hivstatus", "counts", "agegr")]
  particpants = particpants + sum(surv2$counts, na.rm = T)
  
}

# in millions
particpants / 1e6

sex = c("Female", "Male", "Both")
surveydf = data.frame(
  year = vector(),
  surveytype = vector(),
  country = vector(),
  sex = vector(),
  hivstatus = vector()
)
i=1
for (i in 1:length(make_country)) {
  surv = make_country[[i]]$survey_hts[, c("surveyid", "year", "sex", "hivstatus")]
  cnt = names(make_country)[i]
  
  surveydf_temp = surveydf[0,]
  if (any(grepl("﻿", surv$surveyid))) {
    surv$surveyid = sub("﻿", "", surv$surveyid)
  }
  
  surv = distinct(surv)
  for (surveyid_1 in unique(surv$surveyid)) {
    
    print(surveyid_1)
    
    surv_tmp = subset(surv, surveyid %in% surveyid_1)
    if ("both" %in% surv_tmp$sex) {
      surv_tmp$sex = "both"
      #surv_tmp = distinct(surv_tmp)
    }
    
    if (length(unique(surv_tmp$sex)) > 2) {
      print(cnt)
      stop("major error")
    }
    if (length(unique(surv_tmp$sex)) == 2) {
      surv_tmp$sex = "both"
    }
    if (any(c("positive", "serodata") %in% surv_tmp$hivstatus)) {
      surv_tmp$hivstatus = "serodata"
    } else {
      surv_tmp$hivstatus = "no serodata"
    }
    
    surv[surv$surveyid == surveyid_1,] = surv_tmp
  }
  surv = distinct(surv)
  surv$country = cnt
  
  surv$surveyid = survey_type(surv$surveyid)
  # removes counts
  surv = surv[, c(2, 1, 5, 3, 4)]
  names(surv) = names(surveydf)
  surv = distinct(surv)
  surveydf = rbind(surveydf, surv)
  
  
}

# Assign region
surveydf$region <- case_when(
  surveydf$country %in% c(
    "Burundi",
    "Comoros",
    "Eritrea",
    "Ethiopia",
    "Kenya",
    "Madagascar",
    "Malawi",
    "Mozambique",
    "Rwanda",
    "South Sudan",
    "United Republic of Tanzania",
    "Uganda",
    "Zimbabwe"
  ) ~ "Eastern\nAfrica",
  
  surveydf$country %in% c(
    "Angola",
    "Chad",
    "Congo",
    "Democratic Republic of the Congo",
    "Gabon"
  ) ~ "Central\nAfrica",
  
  surveydf$country %in% c("Botswana", "Lesotho", "eSwatini", "South Africa", "Zambia") ~ "Southern\nAfrica",
  
  surveydf$country %in% c(
    "Benin",
    "Burkina Faso",
    "Cape Verde",
    "Côte d'Ivoire",
    "Gambia",
    "Ghana",
    "Guinea",
    "Guinea-Bissau",
    "Liberia",
    "Mali",
    "Mauritania",
    "Niger",
    "Nigeria",
    "Senegal",
    "Sierra Leone",
    "Togo"
  ) ~ "Western\nAfrica",
  # Cote d'Ivore breaks the selection so set it as default
  TRUE ~ "Western\nAfrica"
)

surveydf$region <- factor(
  surveydf$region,
  levels = c(
    "Western\nAfrica",
    "Central\nAfrica",
    "Eastern\nAfrica",
    "Southern\nAfrica"
  )
)

region_colors <- c(
  "Eastern\nAfrica" = "lightgrey",
  "Western\nAfrica" = "lightgrey",
  "Southern\nAfrica" = "lightgrey",
  "Central\nAfrica" = "lightgrey"
)

#Rename DRC and Tanzania for size reasons, before factoring
surveydf$country[surveydf$country  == "United Republic of Tanzania"] = "Tanzania"
surveydf$country[surveydf$country  == "Democratic Republic of the Congo"] = "DRC"
df_segments$country[df_segments$country  == "United Republic of Tanzania"] = "Tanzania"
df_segments$country[df_segments$country  == "Democratic Republic of the Congo"] = "DRC"

surveydf$region <- factor(surveydf$region, levels = names(region_colors))

df_segments = df_segments %>%
  arrange(country)

df_segments$region <- surveydf %>%
  distinct(country, region) %>%
  right_join(df_segments, by = "country") %>%
  arrange(country) %>%
  pull(region)

# Factor levels for shape
surveydf <- surveydf %>%
  mutate(
    country = fct_reorder2(country, region, year, .na_rm = F),
    shape_sex = case_when(sex == "both" ~ 16,
                          sex == "female" ~ 15,
                          sex == "male" ~ 17)
  )

df_segments1 <- df_segments %>%
  left_join(select(surveydf, country, region) %>% distinct(), by = "country") %>%
  mutate(region = factor(
    region.x,
    levels = c(
      "Western\nAfrica",
      "Central\nAfrica",
      "Eastern\nAfrica",
      "Southern\nAfrica"
    )
  ))

surveydf <- surveydf %>%
  left_join(country_base_y, by = "country") %>%
  mutate(y_pos = y_base * 1.5)


# Make a named list of element_rect() for each region
strip_theme <- strip_themed(
  background_y = list(
    "Western\nAfrica" = element_rect(fill = region_colors["Western\nAfrica"]),
    "Central\nAfrica" = element_rect(fill = region_colors["Central\nAfrica"]),
    "Eastern\nAfrica" = element_rect(fill = region_colors["Eastern\nAfrica"]),
    "Southern\nAfrica" = element_rect(fill = region_colors["Southern\nAfrica"])
  ),
  text_y = list(
    "Western\nAfrica" = element_text(
      size = 12,
      face = "bold",
      color = "black"
    ),
    "Central\nAfrica" = element_text(
      size = 12,
      face = "bold",
      color = "black"
    ),
    "Eastern\nAfrica" = element_text(
      size = 12,
      face = "bold",
      color = "black"
    ),
    "Southern\nAfrica" = element_text(
      size = 12,
      face = "bold",
      color = "black"
    )
  )
)


# region then country, alphabetical
country_levels <- surveydf %>%
  distinct(country, region) %>%
  mutate(country_key = tolower(stri_trans_general(country, "Latin-ASCII"))) %>%
  arrange(region, country_key) %>%
  pull(country)

# reverse so A is at top (ggplot puts first level at bottom)
country_levels <- rev(country_levels)

df_segments <- df_segments %>%
  mutate(
    country = factor(country, levels = country_levels),
    region  = factor(
      region,
      levels = c(
        "Eastern\nAfrica",
        "Western\nAfrica",
        "Southern\nAfrica",
        "Central\nAfrica"
      )
    )
  )

surveydf <- surveydf %>%
  mutate(
    country = factor(country, levels = country_levels),
    region  = factor(
      region,
      levels = c(
        "Eastern\nAfrica",
        "Western\nAfrica",
        "Southern\nAfrica",
        "Central\nAfrica"
      )
    )
  )

combined_plot <- ggplot() +
  # Program segments
  geom_segment(
    data = df_segments,
    aes(
      x = year_start,
      xend = year_end,
      y = (country),
      yend = country,
      color = Modality
    ),
    linewidth = 1
  ) +
  
  # Survey points
  geom_point(
    data = surveydf,
    aes(
      x = year,
      y = country,
      shape = factor(shape_sex),
      color = surveytype
    ),
    size = 3
  ) +
  
  # Overlay white dot
  geom_point(
    data = surveydf %>% filter(hivstatus == "no serodata"),
    aes(x = year, y = country),
    shape = 16,
    color = "white",
    size = 1.2
  )+
  
  scale_color_manual(
    name = "Modality",
    breaks = c("anc", "DHS", "MICS", "tot", "PHIA", "other"),
    values = c(
      "anc" = "firebrick",
      "tot" = "steelblue",
      "DHS" = "red",
      "MICS" = "royalblue",
      "PHIA" = "gold2",
      "other" = "forestgreen"
    ),
    labels = c(
      "anc" = "ANC/Non-ANC HTS",
      "tot" = "Total HTS only",
      "DHS" = "DHS/AIS",
      "MICS" = "MICS",
      "PHIA" = "PHIA",
      "other" = "Other"
    )
  ) +
  scale_shape_manual(
    values = c("15" = 15, "16" = 16, "17" = 17),
    labels = c("Female", "Both", "Male"),
    name = "Sex"
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 1)) +
  facet_grid2(
    rows = vars(region),
    scales = "free_y",
    space = "free",
    strip = strip_theme
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    strip.placement = "outside",
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 305, hjust = 0),
    axis.text.y = element_text(size = 12),
    strip.text.y.right = element_text(size = 16, face = "bold")
    
    
  ) + guides(
    color = guide_legend(
      order = 1,
      title.position = "left",
      title.hjust = 0,
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        linetype  = c(1, 0, 0, 1, 0, 0),
        shape     = c(NA, 16, 16, NA, 16, 16),
        linewidth = c(1, 0, 0, 1, 0, 0)
      )
    ),
    shape = guide_legend(
      order = 2,
      title.position = "left",
      title.hjust = 0,
      nrow = 2
    )
  ) +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.box.just = "left",
    legend.title = element_text(face = "bold")
  )
path_out <-
  here::here("outputs/Paper 2026/survey and program graph")

combined_plot
stop()
ggsave(
  plot = combined_plot,
  file = paste0(path_out, "/paper_survey_data.png"),
  width = 8.5,
  height = 11,
  dpi = 700,
  scale = 1
)


svglite::svglite(fn <- file.path(file = paste0(path_out, "/paper_survey_data.svg")),
                 width = 8.5, height = 11)
print(combined_plot)
dev.off()
