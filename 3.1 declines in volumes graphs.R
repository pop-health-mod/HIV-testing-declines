library(scales)
library(first90)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggnewscale)

path_out <- here::here("outputs/Paper/volume decline/")

path_anc <- here::here("anc testing")
make_country = readRDS(paste0(path_anc, "/data/make_country_simul_final.rds"))

source(paste0(path_anc, "/1.1 tot test out.R"))

# ----total tests/anc-nonanc ----

# these are for checking if countries fail
didnt_run = vector()

vcttestvalue = data.frame(year = 2005:2030, value = NA)
simul_vec_vct = array(0, dim = c(31, 3000, 39))
for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  if (is.null(make_country[[cnt]]$simul)) {
    didnt_run = c(didnt_run, cnt)
    next
  }
  fp <- make_country[[cnt]]$fp
  mod = make_country[[cnt]]$mod
  year_start = 2005
  sex  = "both"
  hivstatus = "all"
  test_type = "vcttests"
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  
  out_test <-
    expand.grid(
      year = year_start:end_date,
      outcome = "numbertests",
      agegrp = "15+",
      sex = sex,
      hivstatus = hivstatus
    )
  
  out_test$hivstatus <- as.character(out_test$hivstatus)
  
  out_test$value <-
    vct_total_tests(mod, add_ss_indices(out_test, fp$ss))
  
  out_test = out_test[c(1, 6)]
  names(out_test)[2] = cnt
  
  vcttestvalue = merge(vcttestvalue, out_test, by = "year")
  
  simul = subset(make_country[[cnt]]$simul$vcttests, sex == "both")
  simul = tibble(simul[, 7:3006])
  
  simul_vec_vct[, , i] = as.matrix(simul)
  
}

didnt_run = vector()

anctestvalue = data.frame(year = 2005:2030, value = NA)
simul_vec_anc = array(0, dim = c(31, 3000, 39))

for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  if (is.null(make_country[[i]]$simul)) {
    didnt_run = c(didnt_run, cnt)
    next
  }
  fp <- make_country[[i]]$fp
  mod = make_country[[i]]$mod
  year_start = 2005
  sex  = "both"
  hivstatus = "all"
  test_type = "anctests"
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  
  out_test <-
    expand.grid(
      year = year_start:end_date,
      outcome = "numbertests",
      agegrp = "15+",
      sex = sex,
      hivstatus = hivstatus
    )
  
  out_test$hivstatus <- as.character(out_test$hivstatus)
  
  if (test_type == "anctests") {
    out_test$value <-
      anc_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  } else if (test_type == "vcttests") {
    out_test$value <-
      vct_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  }
  out_test = out_test[c(1, 6)]
  names(out_test)[2] = cnt
  
  anctestvalue = merge(anctestvalue, out_test, by = "year")
  
  simul = subset(make_country[[cnt]]$simul$anctests, sex == "both")
  simul = tibble(simul[, 7:3006])
  
  simul_vec_anc[, , i] = as.matrix(simul)
  
}


vcttestvalue = select(vcttestvalue, !value)
anctestvalue = select(anctestvalue, !value)


CI_VCT = matrix(nrow = 31, ncol = 3)
CI_VCT = as.data.frame(CI_VCT)
colnames(CI_VCT) = c("year", "lci", "uci")
CI_VCT[, 1] = 2000:2030
CI_ANC = CI_VCT

for (j in 1:dim(simul_vec_vct)[1]) {
  #need to do this for now( later fix simul creation)
  CI_VCT[j, 2:3] = quantile(apply(simul_vec_vct[j, , , drop = FALSE], 2, sum), c(0.025, 0.975))
  CI_ANC[j, 2:3] = quantile(apply(simul_vec_anc[j, , , drop = FALSE], 2, sum), c(0.025, 0.975))
  
  
}
# find declines #2017(18) for vct max and 2021(22) min
1 - (quantile(
  probs =  c(0.025, 0.5, 0.975),
  x = (apply(simul_vec_vct[22, , , drop = FALSE], 2, sum) /
         apply(simul_vec_vct[19, , , drop = FALSE], 2, sum))
))


tottests = data.frame(
  Year = vcttestvalue$year,
  test = c(rowSums(vcttestvalue[-1]), rowSums(anctestvalue[-1])),
  lci = c(CI_VCT$lci[CI_VCT$year > 2004],
          CI_ANC$lci[CI_ANC$year > 2004]),
  uci = c(CI_VCT$uci[CI_VCT$year > 2004],
          CI_ANC$uci[CI_ANC$year > 2004]),
  testtype = rep(
    x = c("VCT", "ANC"),
    each = length(vcttestvalue$year)
  )
)

col = "forestgreen"
col2 = "red"

plot_total_anc_vct = ggplot() +
  geom_ribbon(
    data = tottests,
    aes(
      x = Year,
      ymin = lci / 1e6,
      ymax = uci / 1e6,
      group = testtype
    ),
    alpha = 0.2
  ) +
  geom_line(
    data = tottests,
    mapping = aes(y = test / 1e6, x = Year, colour = testtype),
    linewidth = 1.1
  ) +
  labs(title = "Total Number of HIV Tests by Year\nin 37 African Countries: 2015-2023",
       x = NULL,
       y = "Number of HIV Tests (Millions)") +
  scale_color_manual(
    name = NULL,
    values = c("VCT" = col,
               "ANC" = col2),
    labels = c("VCT" = "Non-Antenatal care\nHIV testing",
               "ANC" = "Antenatal care\nHIV testing")
    
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(
      size = 15,
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    axis.text.y = element_text(size = 15),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = comma,
                     breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 120)) +  # does not pick good break points automatically
  scale_x_continuous(breaks = c(2015:2023),
                     limits = c(2015, 2023))  # Formats numbers with commas

plot_total_anc_vct

ggsave(
  plot = plot_total_anc_vct,
  paste0(path_out, "/total tests vct and anc 2015 to 2023.png"),
  width = 5,
  height = 5,
  dpi = 500
)

# ----total postives/positivity anc-nonanc ----

didnt_run = vector()

vcttestvaluepos = data.frame(year = 2005:2030, value = NA)
simul_vec_vct_pos = array(0, dim = c(31, 3000, 39))
names(make_country)
for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  if (is.null(make_country[[i]]$simul)) {
    didnt_run = c(didnt_run, cnt)
    next
  }
  fp <- make_country[[i]]$fp
  mod = make_country[[i]]$mod
  year_start = 2005
  sex  = "both"
  hivstatus = "positive"
  test_type = "vcttests"
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  
  out_test <-
    expand.grid(
      year = year_start:end_date,
      outcome = "numbertests",
      agegrp = "15+",
      sex = sex,
      hivstatus = hivstatus
    )
  
  out_test$hivstatus <- as.character(out_test$hivstatus)
  
  if (test_type == "anctests") {
    out_test$value <-
      anc_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  } else if (test_type == "vcttests") {
    out_test$value <-
      vct_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  }
  out_test = out_test[c(1, 6)]
  names(out_test)[2] = cnt
  
  vcttestvaluepos = merge(vcttestvaluepos, out_test, by = "year")
  
  simul = subset(make_country[[cnt]]$simul$vcttestspos, sex == "both")
  simul = tibble(simul[, 7:3006])
  
  simul_vec_vct_pos[, , i] = as.matrix(simul)
  
  
}

anctestvaluepos = data.frame(year = 2005:2030, value = NA)
simul_vec_anc_pos = array(0, dim = c(31, 3000, 39))
for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  if (is.null(make_country[[i]]$simul)) {
    #didnt_run = c(didnt_run,cnt)
    next
  }
  fp <- make_country[[i]]$fp
  mod = make_country[[i]]$mod
  year_start = 2005
  sex  = "both"
  hivstatus = "positive"
  test_type = "anctests"
  end_date <- fp$ss$proj_start + fp$ss$PROJ_YEARS - 1L
  
  out_test <-
    expand.grid(
      year = year_start:end_date,
      outcome = "numbertests",
      agegrp = "15+",
      sex = sex,
      hivstatus = hivstatus
    )
  
  out_test$hivstatus <- as.character(out_test$hivstatus)
  
  if (test_type == "anctests") {
    out_test$value <-
      anc_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  } else if (test_type == "vcttests") {
    out_test$value <-
      vct_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  }
  out_test = out_test[c(1, 6)]
  names(out_test)[2] = cnt
  
  anctestvaluepos = merge(anctestvaluepos, out_test, by = "year")
  
  simul = subset(make_country[[cnt]]$simul$anctestspos, sex == "both")
  simul = tibble(simul[, 7:3006])
  
  simul_vec_anc_pos[, , i] = as.matrix(simul)
  
}

vcttestvaluepos = select(vcttestvaluepos, !value)
anctestvaluepos = select(anctestvaluepos, !value)


CI_VCT = matrix(nrow = 31, ncol = 3)
CI_VCT = as.data.frame(CI_VCT)
colnames(CI_VCT) = c("year", "lci", "uci")
CI_VCT[, 1] = 2000:2030
CI_ANC = CI_VCT

for (j in 1:dim(simul_vec_vct_pos)[1]) {
  CI_VCT[j, 2:3] = quantile(apply(simul_vec_vct_pos[j, , , drop = FALSE], 2, sum), c(0.025, 0.975))
  CI_ANC[j, 2:3] = quantile(apply(simul_vec_anc_pos[j, , , drop = FALSE], 2, sum), c(0.025, 0.975))
}

# find decliens #2017(18) for vct max and 2021(22) min
1 - (quantile(
  x = (apply(
    simul_vec_vct_pos[22, , , drop = FALSE] -
      simul_vec_anc_pos[22, , , drop = FALSE], 2, sum
  )) / (apply(
    simul_vec_vct_pos[19, , , drop = FALSE] -
      simul_vec_anc_pos[19, , , drop = FALSE], 2, sum
  )),
  probs = c(0.025, 0.5, 0.975)
))


postests = data.frame(
  Year = vcttestvaluepos$year,
  test = c(rowSums(vcttestvaluepos[-1]), rowSums(anctestvaluepos[-1])),
  lci = c(CI_VCT$lci[CI_VCT$year > 2004],
          CI_ANC$lci[CI_ANC$year > 2004]),
  uci = c(CI_VCT$uci[CI_VCT$year > 2004],
          CI_ANC$uci[CI_ANC$year > 2004]),
  testtype = rep(c("VCT", "ANC"), each = length(vcttestvaluepos$year))
)

plot_pos_anc_vct = ggplot() +
  geom_ribbon(
    data = postests,
    aes(
      x = Year,
      ymin = lci / 1e6,
      ymax = uci / 1e6,
      group = testtype
    ),
    alpha = 0.2
  ) +
  geom_line(
    data = postests,
    mapping = aes(y = test / 1e6, x = Year, colour = testtype),
    linewidth = 1.1
  ) +
  labs(title = "Number of Positive HIV Tests by Year\nin 37 African Countries: 2015-2023",
       x = NULL,
       y = "Number of Positive HIV Tests (Millions)") +
  scale_color_manual(
    name = NULL,
    values = c("VCT" = "aquamarine3",
               "ANC" = "lightsalmon3"),
    labels = c("VCT" = "Non-Antenatal care\nHIV testing",
               "ANC" = "Antenatal care\nHIV testing")
    
  ) +
  theme_minimal() +
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
    legend.position = "none"  # Moves legend inside the plot (80% right, 20% up)
    
    
  ) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 3000, by = 500) / 1000,
    limits = c(0, max(postests$uci) / 1e6)
  ) +  # Formats numbers with commas
  scale_x_continuous(breaks = c(seq(
    from = 2015, to = 2023, by = 1
  )), limits = c(2015, 2023))  # Formats numbers with commas

plot_pos_anc_vct

ggsave(
  plot = plot_pos_anc_vct,
  paste0(path_out, "/Positive tests vct and anc 2015 to 2023.png"),
  width = 5,
  height = 5,
  dpi = 500
)

# --- positivity ----

vct_pos = rowSums(vcttestvaluepos[-1]) / rowSums(vcttestvalue[-1])
anc_pos = rowSums(anctestvaluepos[-1]) / rowSums(anctestvalue[-1])


CI_VCT = matrix(nrow = 31, ncol = 3)
CI_VCT = as.data.frame(CI_VCT)
colnames(CI_VCT) = c("year", "lci", "uci")
CI_VCT[, 1] = 2000:2030
CI_ANC = CI_VCT
for (j in 1:dim(simul_vec_vct)[1]) {
  numerator <- apply(simul_vec_vct_pos[j, , , drop = FALSE], 2, sum)
  denominator <- apply(simul_vec_vct[j, , , drop = FALSE], 2, sum)
  
  ratio <- ifelse(denominator == 0, 0, numerator / denominator)
  CI_VCT[j, 2:3] <- quantile(ratio, probs = c(0.025, 0.975))
  
  numerator <- apply(simul_vec_anc_pos[j, , , drop = FALSE], 2, sum)
  denominator <- apply(simul_vec_anc[j, , , drop = FALSE], 2, sum)
  
  ratio <- ifelse(denominator == 0, 0, numerator / denominator)
  CI_ANC[j, 2:3] <- quantile(ratio, probs = c(0.025, 0.975))
  
  
}

positivitytests = data.frame(
  Year = vcttestvaluepos$year,
  test = c(vct_pos, anc_pos),
  lci = c(CI_VCT$lci[CI_VCT$year > 2004],
          CI_ANC$lci[CI_ANC$year > 2004]),
  uci = c(CI_VCT$uci[CI_VCT$year > 2004],
          CI_ANC$uci[CI_ANC$year > 2004]),
  testtype = rep(c("VCT", "ANC"), each = length(vcttestvaluepos$year))
)


plot_positvity_anc_vct = ggplot() +
  geom_ribbon(
    data = positivitytests,
    aes(
      x = Year,
      ymin = lci * 100,
      ymax = uci * 100,
      group = testtype
    ),
    alpha = 0.2
  ) +
  geom_line(
    data = positivitytests,
    mapping = aes(y = test * 100, x = Year, colour = testtype),
    linewidth = 1.1,
    linetype = 4
  ) +
  labs(title = "Positivity of HIV Tests by Year\nin 37 African Countries: 2015-2023",
       x = NULL,
       y = "Percent (%) Positivity of HIV Tests") +
  scale_color_manual(
    name = NULL,
    values = c("VCT" = "aquamarine3",
               "ANC" = "lightsalmon3"),
    labels = c("VCT" = "Non-Antenatal care\nHIV Testing",
               "ANC" = "Antenatal care\nHIV Testing")
    
  ) +
  theme_minimal() +
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
    legend.position = "none"  # Moves legend inside the plot (80% right, 20% up)
    
    
  ) +
  #geom_vline(xintercept = 2018,linetype = "dotted",size = 0.7)+
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 6, by = 1),
    limits = c(0, max(positivitytests$uci[positivitytests$Year >= 2015]) * 100)
  ) +  # Formats numbers with commas
  scale_x_continuous(breaks = c(seq(
    from = 2015, to = 2023, by = 1
  )), limits = c(2015, 2023))  # Formats numbers with commas

plot_positvity_anc_vct

ggsave(
  plot = plot_positvity_anc_vct,
  paste0(path_out, "/Positivity vct and anc 2015 to 2023.png"),
  width = 5,
  height = 5,
  dpi = 500
)

#---- legend ----

tottests_legend = data.frame(
  x = 1:4,
  y = 1:4,
  test_type = rep(x = c("ANC", "VCT")),
  test_type2 = rep(x = c("ANC", "VCT")),
  test_type3 = rep(x = c("pos", "ivity"))
)

col = "forestgreen"
col2 = "red"

plot_total_anc_vct = ggplot() +
  geom_line(
    data = tottests_legend,
    mapping = aes(y = y, x = x, colour = test_type),
    linewidth = 1.1
  ) +
  scale_color_manual(
    name = "Positive\nTests",
    values = c("VCT" = "aquamarine3",
               "ANC" = "lightsalmon3"),
    breaks = c("VCT", "ANC"),
    labels = c("VCT" = "Non-ANC\nHIV Testing",
               "ANC" = "ANC\nHIV Testing")
    
  ) +
  guides(colour = guide_legend(
    order = 2,
    title.position = "left",
    title.hjust = 0,
    nrow = 2
  )) +
  ggnewscale::new_scale_colour() +
  geom_line(
    data = tottests_legend,
    mapping = aes(y = y, x = x, colour = test_type),
    linewidth = 1.1
  ) +
  scale_color_manual(
    name = "Total\nTests",
    values = c("VCT" = col,
               "ANC" = col2),
    breaks = c("VCT", "ANC"),
    labels = c("VCT" = "Non-ANC\nHIV Testing",
               "ANC" = "ANC\nHIV Testing")
    
  ) +
  guides(colour = guide_legend(
    order = 1,
    title.position = "left",
    title.hjust = 0,
    nrow = 2
  )) +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_total_anc_vct

ggsave(
  plot = plot_total_anc_vct,
  paste0(path_out, "/total tests legend.png"),
  width = 7,
  height = 5,
  dpi = 500
)

#---- ANC geo map ----

# set up geo
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
africa_anc <- world[world$CONTINENT == "Africa",]

#adjust names to match with those from package( ensure your match up too)
africa_anc$name[africa_anc$name == "Dem. Rep. Congo"] = "Democratic Republic of the Congo"
africa_anc$name[africa_anc$name == "S. Sudan"] = "South Sudan"
africa_anc$name[africa_anc$name == "Tanzania"] = "United Republic of Tanzania"
africa_anc$name[africa_anc$name == "Cabo Verde"] = "Cape Verde"
africa_anc$name[africa_anc$name == "Republic of the Congo"] = "Congo"

geo_anctestvalue = data.frame(year = 2016:2023, value = NA)

for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  if (is.null(make_country[[i]]$simul)) {
    next
  }
  fp <- make_country[[i]]$fp
  mod = make_country[[i]]$mod
  year_start = 2016
  sex  = "both"
  hivstatus = "all"
  test_type = "anctests"
  end_date <- 2023
  
  out_test <-
    expand.grid(
      year = year_start:end_date,
      outcome = "numbertests",
      agegrp = "15+",
      sex = sex,
      hivstatus = hivstatus
    )
  
  out_test$hivstatus <- as.character(out_test$hivstatus)
  
  if (test_type == "anctests") {
    out_test$value <-
      anc_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  } else if (test_type == "vcttests") {
    out_test$value <-
      vct_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  }
  out_test = out_test[c(1, 6)]
  names(out_test)[2] = cnt
  
  geo_anctestvalue = merge(geo_anctestvalue, out_test, by = "year")
  
  
}

geo_anctestvalue = select(geo_anctestvalue, !value)
geo_anctestvalue[dim(geo_anctestvalue)[1] + 1, 1] <-  "max"
geo_anctestvalue[dim(geo_anctestvalue)[1] + 1, 1] <-  "min"
geo_anctestvalue[dim(geo_anctestvalue)[1] + 1, 1] <-  "percent"
rownames(geo_anctestvalue) = c(geo_anctestvalue$year)
geo_anctestvalue = select(geo_anctestvalue, !year)

#last possible year before declines: 2020
geo_anctestvalue[dim(geo_anctestvalue)[1] - 2,] <-
  ifelse(apply(geo_anctestvalue[, ], 2, which.max) > 5,
         8,
         apply(geo_anctestvalue[, ], 2, which.max))

#finds minimum idx after decline
geo_anctestvalue[dim(geo_anctestvalue)[1] - 1, ] <-
  sapply(seq_along(geo_anctestvalue[dim(geo_anctestvalue)[1] - 1, ]), function(j) {
    start_idx <-
      unlist(geo_anctestvalue[dim(geo_anctestvalue)[1] - 2, ][j]) + 1
    if (start_idx > (nrow(geo_anctestvalue) - 3))
      return(NA)
    subvec <-
      geo_anctestvalue[unlist(start_idx):(nrow(geo_anctestvalue) - 3), j]
    return(which.min(subvec) + start_idx - 1)  # adjust to get global row index
  })

geo_anctestvalue[dim(geo_anctestvalue)[1], ] <-
  sapply(seq_along(geo_anctestvalue[dim(geo_anctestvalue)[1], ]), function(j) {
    max_idx <-
      unlist(geo_anctestvalue[dim(geo_anctestvalue)[1] - 2, ])[j]
    min_idx <-
      unlist(geo_anctestvalue[dim(geo_anctestvalue)[1] - 1, ])[j]
    if (is.na(min_idx))
      return(NA)
    #uses min and max idx to find the corresponding values and finds % decline
    maxvec <- geo_anctestvalue[max_idx, j]
    minvec <- geo_anctestvalue[min_idx, j]
    return(round(1 - minvec / maxvec, 2))
    
  })


# set no-decline to NA
geo_anctestvalue[11, ] <-
  ifelse(geo_anctestvalue[11, ] <= 0, NA, geo_anctestvalue[11, ])

#find number of countries with ANC declines
length(geo_anctestvalue[11,!is.na(geo_anctestvalue[11,])])

# create dataframe with country name and hiv_decline
africa_anc_data <- data.frame(name = names(geo_anctestvalue[, ]),
                              hiv_decline = unlist(geo_anctestvalue[11, ]))
# find median decline
median(africa_anc_data$hiv_decline, na.rm = T)
quantile(africa_anc_data$hiv_decline, na.rm = T)
names(make_country) # find south sudan
# find south sudans decline
1 - quantile(simul_vec_anc[19, , 31] / simul_vec_anc[18, , 31], c(0.025, 0.5, 0.975))

# Merge with map data
africa_map <- africa_anc %>%
  left_join(africa_anc_data, by = c("name" = "name"))

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
africa_map$hiv_decline[africa_map$category == "Gradient"]

plot_africa_maximum_anc_declines = ggplot() +
  # Gradient layer for hiv_decline
  geom_sf(
    data = africa_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline * 100),
    color = "black"
  ) +
  scale_fill_viridis_c(
    guide = F,
    option = "rocket",
    na.value = "white",
    name = "Maximum \npercent (%)\ndecline",
    direction = -1,
    limits = c(0, 100),
    breaks = c(0, 20, 40, 60, 80, 100)
    
  ) +
  
  # New fill scale for categorical values
  ggnewscale::new_scale_fill() +
  
  # Categorical layers (No Data & Selected Countries)
  geom_sf(
    data = africa_map %>% filter(category %in% c("No Data", "Selected Countries")),
    aes(fill = category),
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
    guide = F,
    name = NULL,
    values = c("pepfar" = "NA"),
    labels = c("pepfar" = "PEPFAR")
  ) +
  
  # Theme and labels
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14,
      legend.key = element_rect(size = 0.1),
    )
  ) +
  labs(title = "Maximum Decline in ANC Testing Volume\nin Percent (%) Over 2015-2023")

plot_africa_maximum_anc_declines

ggsave(
  plot = plot_africa_maximum_anc_declines,
  file = paste0(path_out, "/plot_africa_anc_maxdecline.png"),
  width = 7,
  height = 7,
  dpi = 700
)


#---- non-ANC geo map ----
geo_vcttestvalue = data.frame(year = 2016:2023, value = NA)

for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  if (is.null(make_country[[i]]$simul)) {
    #didnt_run = c(didnt_run,cnt)
    next
  }
  fp <- make_country[[i]]$fp
  mod = make_country[[i]]$mod
  year_start = 2016
  sex  = "both"
  hivstatus = "all"
  test_type = "vcttests"
  end_date <- 2023
  
  out_test <-
    expand.grid(
      year = year_start:end_date,
      outcome = "numbertests",
      agegrp = "15+",
      sex = sex,
      hivstatus = hivstatus
    )
  
  out_test$hivstatus <- as.character(out_test$hivstatus)
  
  if (test_type == "anctests") {
    out_test$value <-
      anc_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  } else if (test_type == "vcttests") {
    out_test$value <-
      vct_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  }
  out_test = out_test[c(1, 6)]
  names(out_test)[2] = cnt
  
  geo_vcttestvalue = merge(geo_vcttestvalue, out_test, by = "year")
  
  
}
geo_vcttestvalue = select(geo_vcttestvalue,!value)
geo_vcttestvalue[dim(geo_vcttestvalue)[1] + 1, 1] = "max"
geo_vcttestvalue[dim(geo_vcttestvalue)[1] + 1, 1] = "min"
geo_vcttestvalue[dim(geo_vcttestvalue)[1] + 1, 1] = "percent"
rownames(geo_vcttestvalue) = c(geo_vcttestvalue$year)
geo_vcttestvalue = select(geo_vcttestvalue,!year)

#last possible year before declines: 2020
geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 2, ] <-
  ifelse(apply(geo_vcttestvalue[,], 2, which.max) > 5,
         8,
         apply(geo_vcttestvalue[,], 2, which.max))

#finds minimum idx after decline
geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 1,] <-
  sapply(seq_along(geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 1,]), function(j) {
    start_idx <-
      unlist(geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 2,][j]) + 1
    if (start_idx > (nrow(geo_vcttestvalue) - 3))
      return(NA)
    subvec <-
      geo_vcttestvalue[unlist(start_idx):(nrow(geo_vcttestvalue) - 3), j]
    return(which.min(subvec) + start_idx - 1)  # adjust to get global row index
  })

geo_vcttestvalue[dim(geo_vcttestvalue)[1],] <-
  sapply(seq_along(geo_vcttestvalue[dim(geo_vcttestvalue)[1],]), function(j) {
    max_idx <-
      unlist(geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 2,])[j]
    min_idx <-
      unlist(geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 1,])[j]
    if (is.na(min_idx))
      return(NA)
    #uses min and max idx to find the corresponding values and finds % decline
    maxvec <- geo_vcttestvalue[max_idx, j]
    minvec <- geo_vcttestvalue[min_idx, j]
    return(round(1 - minvec / maxvec, 2))
    
  })

# set no-decline to NA
geo_vcttestvalue[11, ] <-
  ifelse(geo_vcttestvalue[11, ] <= 0, NA, geo_vcttestvalue[11, ])

length(geo_vcttestvalue[11, !is.na(geo_vcttestvalue[11, ])])

# create dataframe with country name and hiv_decline
africa_anc_data <- data.frame(name = names(geo_vcttestvalue[, ]),
                              hiv_decline = unlist(geo_vcttestvalue[11, ]))


# find median decline
median(africa_anc_data$hiv_decline, na.rm = T)
quantile(africa_anc_data$hiv_decline, na.rm = T)
names(make_country) # find south sudan
# find south sudans decline
1 - quantile(simul_vec_anc[19, , 31] / simul_vec_anc[18, , 31], c(0.025, 0.5, 0.975))


# Merge with map data
africa_map <- africa_anc %>%
  left_join(africa_anc_data, by = c("name" = "name"))

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

africa_map$name[!(africa_map$name %in% black_countries)]
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
  "Zimbabwe",
  "Burkina Faso",
  "Ghana",
  "Liberia",
  "Mali",
  "Sierra Leone",
  "Senegal",
  "Togo"
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

africa_map$name[africa_map$category == "Selected Countries"]

plot_africa_maximum_vct_declines = ggplot() +
  # Gradient layer for hiv_decline
  geom_sf(
    data = africa_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline * 100),
    color = "black"
  ) +
  scale_fill_viridis_c(
    guide = F,
    option = "rocket",
    na.value = "white",
    name = "Maximum \npercent (%)\ndecline",
    direction = -1,
    limits = c(0, 100),
    breaks = c(0, 20, 40, 60, 80, 100)
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
    guide = F,
    name = NULL,
    values = c("pepfar" = "NA"),
    labels = c("pepfar" = "PEPFAR")
    #guide = "bottom"  # Remove legend if unnecessary
  ) +
  
  # Theme and labels
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      # Center the title
      face = "bold",
      # Make the title bold
      size = 14,
      # Adjust the font size),
      legend.key = element_rect(size = 0.1),
      
    )
  ) +
  
  labs(title = "Maximum Decline in Non-ANC Testing Volume\nin Percent (%) Over 2015-2023")

plot_africa_maximum_vct_declines

ggsave(
  plot = plot_africa_maximum_vct_declines,
  file = paste0(path_out, "/plot_africa_vct_maxdecline.png"),
  width = 7,
  height = 7,
  dpi = 700
)



#---- pepfar vs non pepfar ----
geo_vcttestvalue = data.frame(year = 2016:2023, value = NA)

for (i in 1:length(make_country)) {
  cnt = names(make_country)[i]
  if (is.null(make_country[[i]]$simul)) {
    #didnt_run = c(didnt_run,cnt)
    next
  }
  fp <- make_country[[i]]$fp
  mod = make_country[[i]]$mod
  year_start = 2016
  sex  = "both"
  hivstatus = "all"
  test_type = "vcttests"
  end_date <- 2023
  
  out_test <-
    expand.grid(
      year = year_start:end_date,
      outcome = "numbertests",
      agegrp = "15+",
      sex = sex,
      hivstatus = hivstatus
    )
  
  out_test$hivstatus <- as.character(out_test$hivstatus)
  
  if (test_type == "anctests") {
    out_test$value <-
      anc_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  } else if (test_type == "vcttests") {
    out_test$value <-
      vct_total_tests(mod, add_ss_indices(out_test, fp$ss))
    
  }
  out_test = out_test[c(1, 6)]
  names(out_test)[2] = cnt
  
  geo_vcttestvalue = merge(geo_vcttestvalue, out_test, by = "year")
  
  
}
geo_vcttestvalue = select(geo_vcttestvalue,!value)
geo_vcttestvalue[dim(geo_vcttestvalue)[1] + 1, 1] = "max"
geo_vcttestvalue[dim(geo_vcttestvalue)[1] + 1, 1] = "min"
geo_vcttestvalue[dim(geo_vcttestvalue)[1] + 1, 1] = "percent"
rownames(geo_vcttestvalue) = c(geo_vcttestvalue$year)
geo_vcttestvalue = select(geo_vcttestvalue,!year)

#last possible year before declines: 2020
geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 2,] <-
  ifelse(apply(geo_vcttestvalue[, ], 2, which.max) > 5,
         8,
         apply(geo_vcttestvalue[, ], 2, which.max))
#finds minimum idx after decline
geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 1,] <-
  sapply(seq_along(geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 1,]), function(j) {
    start_idx <-
      unlist(geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 2,][j]) + 1
    if (start_idx > (nrow(geo_vcttestvalue) - 3))
      return(NA)
    subvec <-
      geo_vcttestvalue[unlist(start_idx):(nrow(geo_vcttestvalue) - 3), j]
    return(which.min(subvec) + start_idx - 1)  # adjust to get global row index
  })

geo_vcttestvalue[dim(geo_vcttestvalue)[1],] <-
  sapply(seq_along(geo_vcttestvalue[dim(geo_vcttestvalue)[1],]), function(j) {
    max_idx <-
      unlist(geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 2,])[j]
    min_idx <-
      unlist(geo_vcttestvalue[dim(geo_vcttestvalue)[1] - 1,])[j]
    if (is.na(min_idx))
      return(NA)
    #uses min and max idx to find the corresponding values and finds % decline
    maxvec <- geo_vcttestvalue[max_idx, j]
    minvec <- geo_vcttestvalue[min_idx, j]
    return(round(1 - minvec / maxvec, 2))
    
  })



# set no-decline to NA
geo_vcttestvalue[11, ] <-
  ifelse(geo_vcttestvalue[11, ] <= 0, NA, geo_vcttestvalue[11, ])

# create dataframe with country name and hiv_decline
africa_anc_data <- data.frame(name = names(geo_vcttestvalue[, ]),
                              hiv_decline = unlist(geo_vcttestvalue[11, ]))


# Merge with map data
africa_map <- africa_anc %>%
  left_join(africa_anc_data, by = c("name" = "name"))

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

africa_map$name[!(africa_map$name %in% black_countries)]
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

africa_map$name[africa_map$category == "Selected Countries"]

plot_africa_maximum_vct_declines = ggplot() +
  # Gradient layer for hiv_decline
  geom_sf(
    data = africa_map %>% filter(category == "Gradient"),
    aes(fill = hiv_decline * 100),
    color = "black"
  ) +
  scale_fill_viridis_c(
    guide = F,
    option = "viridis",
    na.value = "white",
    name = "Maximum \npercent (%)\ndecline",
    direction = -1,
    limits = c(0, 100),
    breaks = c(0, 20, 40, 60, 80, 100)
    
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
    labels = c("pepfar" = "PEPFAR")
    #guide = "bottom"  # Remove legend if unnecessary
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
      # Center the title
      face = "bold",
      # Make the title bold
      size = 12,
      # Adjust the font size
      legend.key = element_rect(size = 0.1),
    )
  ) +
  geom_sf(
    data = africa_map %>% filter(pepfar == "pepfar"),
    aes(color = pepfar),
    linewidth = 1.5,
    fill = NA
  ) +
  
  # Define the red outline color
  scale_color_manual(
    name = NULL,
    values = c("pepfar" = "red"),
    labels = c("pepfar" = "PEPFAR")
    #guide = "none"  # Remove legend if unnecessary
  ) +
  
  labs(title = "Maximum decline in non-ANC testing volume\nin percent (%) over the time period 2015-2023")

plot_africa_maximum_vct_declines

ggsave(
  plot = plot_africa_maximum_vct_declines,
  file = paste0(path_out, "/plot_africa_vct_maxdecline_with_pepfar"),
  width = 7,
  height = 7,
  dpi = 700
)
