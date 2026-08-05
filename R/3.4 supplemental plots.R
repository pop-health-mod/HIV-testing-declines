source("anc testing/1.0 simmod.R")
source("anc testing/1.1 tot test out.R")

# depricated, interest is program data not already assumption driven simulation data
# CI_cor = matrix(nrow = 37, ncol = 4)
# CI_cor = as.data.frame(CI_cor)
# colnames(CI_cor) = c("country", "median", "lci", "uci")
# CI_cor[, 1] = names(make_country)
# 
# 
# for (j in 1:dim(simul_vec_vct)[3]) {
#   #need to do this for now( later fix simul creation)
#   CI_cor[j, 2:4] <- sapply(1:3000, function(i) {
#     cor(
#       simul_vec_vct[16:24, i, j, drop = FALSE] - simul_vec_anc[16:24, i, j, drop =
#                                                                  FALSE],
#       simul_vec_vct_pos[16:24, i, j, drop = FALSE] - simul_vec_anc_pos[16:24, i, j, drop =
#                                                                          FALSE],
#       use = "pairwise.complete.obs",
#       method = "pearson"
#     )
#   }) %>% quantile(c(0.5, 0.025, 0.975), na.rm = T)
#   
# }
# 
# 
# 
# CI_cor = CI_cor %>% arrange(country)
# CI_cor$country = factor(CI_cor$country, levels = sort(unique(CI_cor$country), decreasing = T))
# # 5. Plot
# correlationplot = ggplot(CI_cor, aes(y = country)) +
#   geom_errorbarh(aes(xmin = lci, xmax = uci),
#                  size = 0.4,
#                  colour = "red") +
#   geom_point(aes(x = median)) +
#   
#   theme_minimal() +
#   scale_x_continuous(name = "Pearson's correlation") +
#   labs(title = "Pearson's Correlation: Total vs Positive HIV Tests") +
#   theme(
#     plot.title = element_text(size = 12, face = "bold", hjust = 1.3),
#     axis.title.x = element_text(size = 12),
#     axis.title.y = element_text(size = 0),
#     axis.text.x = element_text(size = 12, hjust = 0.5)
#   ) +
#   
#   geom_vline(xintercept = 0, linetype = 3)
# 
# 
# correlationplot
# path_out <- here::here("outputs/2026 ttd/")
# 
# ggsave(
#   plot = correlationplot,
#   file = paste0(path_out, "/correlationplot.png"),
#   width = 6,
#   height = 7,
#   dpi = 700
# )


CI_cor_vct = data.frame(country = NA, median = NA)
CI_cor_vct[1:37, 1] = names(make_country)

for (i in 1:length(make_country)) {
  #need to do this for now( later fix simul creation)
  CI_cor_vct[i, 2] <-
    cor(make_country[[i]]$prgm_dat[make_country[[i]]$prgm_dat$year %in% c(2015:2023), c(7, 8)],use = "complete.obs")[1, 2]
}
CI_cor_vct[CI_cor_vct$country %in% "Democratic Republic of the Congo", 1] = "DRC"
CI_cor_vct[CI_cor_vct$country == "United Republic of Tanzania", 1] = "Tanzania"

CI_cor_vct = CI_cor_vct %>% arrange(country)
CI_cor_vct$country = factor(CI_cor_vct$country, levels = sort(unique(CI_cor_vct$country), decreasing = T))


correlationplot = ggplot(CI_cor_vct, aes(y = country)) +
  #geom_errorbarh(aes(xmin = lci,xmax = uci),size = 0.4,colour = "red")+
  geom_point(aes(x = median)) +
  
  theme_minimal() +
  scale_x_continuous(
    name = "Pearson's correlation",
    limits = c(-1, 1),
    labels = c(-1, -0.5, 0, 0.5, 1)
  ) +
  labs(title = "Pearson's Correlation: Total vs Positive HIV Tests\npairwise by year, Non-ANC") +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 0),
    axis.text.x = element_text(size = 12, hjust = 0.5)
  ) +
  
  geom_vline(xintercept = 0, linetype = 3)


correlationplot
path_out <- here::here("outputs/Paper 2026/supplemental")

ggsave(
  plot = correlationplot,
  file = paste0(path_out, "/correlationplot_vct.png"),
  width = 6,
  height = 7,
  dpi = 700
)

# anc
CI_cor_anc = data.frame(country = NA, median = NA)
CI_cor_anc[1:37, 1] = names(make_country)
i=1
for (i in 1:length(make_country)) {
  
  CI_cor_anc[i, 2] <-
    cor(make_country[[i]]$prgm_dat[make_country[[i]]$prgm_dat$year %in% c(2015:2023), c(9, 10)],use = "complete.obs")[1, 2]
}
CI_cor_anc[CI_cor_anc$country %in% "Democratic Republic of the Congo", 1] = "DRC"
CI_cor_anc[CI_cor_anc$country == "United Republic of Tanzania", 1] = "Tanzania"

CI_cor_anc = CI_cor_anc %>% arrange(country)
CI_cor_anc$country = factor(CI_cor_anc$country, levels = sort(unique(CI_cor_anc$country), decreasing = T))


correlationplot = ggplot(CI_cor_anc, aes(y = country)) +
  #geom_errorbarh(aes(xmin = lci,xmax = uci),size = 0.4,colour = "red")+
  geom_point(aes(x = median)) +
  
  theme_minimal() +
  scale_x_continuous(
    name = "Pearson's correlation",
    limits = c(-1, 1),
    labels = c(-1, -0.5, 0, 0.5, 1)
  ) +
  labs(title = "Pearson's Correlation: Total vs Positive HIV Tests\npairwise by year, ANC") +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 0),
    axis.text.x = element_text(size = 12, hjust = 0.5)
  ) +
  
  geom_vline(xintercept = 0, linetype = 3)


correlationplot
path_out <- here::here("outputs/Paper 2026/supplemental")

ggsave(
  plot = correlationplot,
  file = paste0(path_out, "/correlationplot_anc.png"),
  width = 6,
  height = 7,
  dpi = 700
)



#---- propprtion plots----

path_anc <- here::here("anc testing")
path_out_data <- "outputs"
hivdemo_proj_list <- readRDS(paste0(path_anc, "/data/hivdemo_proj_dt_cnt.rds"))

pmtct_list <- readRDS(paste0(path_anc, "/data/pmtct_list_cnt.rds"))

# long simulation, set rerun_init to T if this is the first time with a new simulation
rerun_init = F
if (rerun_init) {
  anc_prop = data.frame(year = 2005:2030, value = NA)
  simul_vec_anc_prop = array(0, dim = c(31, 3000, 39))
  prop_anc = list()
  for (l in 1:length(make_country)) {
    cnt = names(make_country)[l]
    if (is.null(make_country[[l]]$simul)) {
      #didnt_run = c(didnt_run,cnt)
      next
    }
    print(cnt)
    pmtct = pmtct_list[[cnt]]
    hivdemo_proj = hivdemo_proj_list[[cnt]]
    
    fp <- make_country[[l]]$fp
    mod = make_country[[l]]$mod
    samp = make_country[[l]]$samp
    
    simul_prop = array(0, dim = c(31, 3000))
    for (i in 1:3000) {
      fp = create_anc_param(samp[i,], fp, pmtct, hivdemo_proj)
      mod = simmod_anc_t(fp, VERSION = "C")
      
      vct_prop = get_out_diagnoses(mod, fp, testtype = "vct", "female")$value /
        (
          get_out_diagnoses(mod, fp, testtype = "anc", "female")$value + get_out_diagnoses(mod, fp, testtype = "vct", "female")$value
        )
      simul_prop[, i] = vct_prop
      
    }
    country_prop = as.data.frame(matrix(nrow = 31, ncol = 5))
    names(country_prop) = c("country", "year", "est", "l_ci", "u_ci")
    country_prop$country = cnt
    country_prop$year = 2000:2030
    for (j in 1:31) {
      country_prop[j, 3:5] =  (quantile(simul_prop[j,], c(0.5, 0.025, 0.975)))
      
    }
    prop_anc[[cnt]] = country_prop
    simul_prop
    simul_vec_anc_prop[, , l] = as.matrix(simul_prop)
    
  }
  saveRDS(object = prop_anc, paste0(path_out_data, "/prop anc diagnoses.rds"))
  
  saveRDS(object = simul_vec_anc_prop, paste0(path_out_data, "/simul prop anc diagnoses.rds"))
}

simul_vec_anc_prop = readRDS(paste0(path_out_data, "/simul prop anc diagnoses.rds"))
prop_anc = readRDS(paste0(path_out_data, "/prop anc diagnoses.rds"))

prop_anc_df <- dplyr::bind_rows(prop_anc, .id = "country")

prop_anc_df$country[prop_anc_df$country == "United Republic of Tanzania"] = "Tanzania"
prop_anc_df$country[prop_anc_df$country == "Democratic Republic of the Congo"] = "DRC"

prop_anc_df$testtype = "vct"
prop_vct_df = prop_anc_df
prop_vct_df$est  = 1 - prop_anc_df$est
prop_vct_df$l_ci  = 1 - prop_anc_df$u_ci
prop_vct_df$u_ci  = 1 - prop_anc_df$l_ci
prop_vct_df$testtype = "anc"

prop_df = rbind(prop_vct_df, prop_anc_df)
prop_df$testtype <-
  factor(prop_df$testtype, levels = c("vct", "anc"))  # anc on bottom, vct on top

path_out = here::here("outputs/Paper 2026/supplemental")

prop_anc_plot = ggplot() +
  geom_area(
    prop_df,
    mapping = aes(
      x = year,
      y = est,
      group = testtype,
      fill = testtype
    ),
    alpha = 0.7
  ) +
  geom_ribbon(
    data = subset(prop_df, testtype == "anc"),
    mapping = aes(x = year, ymax = l_ci, ymin = u_ci),
    alpha = 0.5
  ) +
  geom_line(
    data = subset(prop_df, testtype == "anc"),
    mapping = aes(x = year, y = est),
    colour = "black"
  ) +
  scale_x_continuous(limits = c(2015, 2023), ) +
  labs(title = "Proportion of Diagnoses That are Reached by\nEach Testing Modality among WLHIV 2015-2023",
       x = NULL,
       y = "Proportion of New Diagnoses from ANC") +
  scale_fill_manual(
    name = "Testing\nModality",
    values = c("vct" = "aquamarine4",
               "anc" = "salmon"),
    labels = c("vct" = "Non-ANC",
               "anc" = "ANC")
  ) +
  geom_hline(yintercept = c(0.25, 0.75), linetype = 3) +
  geom_hline(
    yintercept = c(0.5),
    linetype = 2,
    colour = "black"
  ) +
  facet_wrap( ~ country, ncol = 8) +
  theme_minimal() +
  theme(
    #legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    # Center the title,
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(
      size = 13,
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    axis.text.y = element_text(size = 15),
    axis.ticks.x = element_line(),
    axis.ticks.length = unit(0.15, "cm")
  ) +
  theme(
    legend.position = c(0.75, 0.02),
    # x, y in [0,1] relative to panel area
    legend.justification = c(0.5, 0),
    # anchor point of the legend box
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 1))


ggsave(
  plot = prop_anc_plot,
  file = paste0(path_out, "/prop_anc 1 21.png"),
  width = 12,
  height = 8.5,
  dpi = 500,
  scale = 1
)


#---- proportion pooled ----
# match weighting to status awareness
# samp is identical so can utilize already calculated weights from unaware
library(data.table)
weighted_prop = list()
for (j in 1:length(make_country)) {
  cnt = make_country[[j]]$cnt
  aware = aware_agg_simul_femalefull[[cnt]]
  if (is.null(make_country[[cnt]]$simul$unaware$unaware)) {
    next
  }
  
  
  weight <-
    apply(subset(
      make_country[[cnt]]$simul$unaware$unaware,
      (year %in% 2015:2023 &
         sex == "female" &
         agegr == "15-49")[6:3005]
    ),
    1,
    median,
    na.rm = TRUE) %>%
    as.numeric() %>%
    rep(., each = 3000)
  
  
  # x is 31 x 3000 for one country j
  x <- simul_vec_anc_prop[, , j]
  
  dt <- as.data.table(as.table(x))
  setnames(dt, c("year_idx", "sample", "value"))
  
  dt[, year := rep(2000:2030, times = ncol(x))][, year_idx := NULL]
  dt[, sample := rep(1:3000, each  = 31)]# columns: sample (1..3000), value, year (2005..2030)
  dt <- dt[year >= 2015 & year <= 2023]
  setorder(dt, year, sample)
  
  dt$w = weight
  weighted_prop[[cnt]] = dt
}


weighted_prop



all_dt <-
  rbindlist(weighted_prop,
            idcol = "country",
            use.names = TRUE,
            fill = TRUE)

weighted_ANC_prop <-
  all_dt[, .(mean = sum(value * w, na.rm = TRUE) / sum(w, na.rm = TRUE)), by = .(year, sample)]

setorder(weighted_ANC_prop, year, sample)

weighted_ANC_prop_CrI <- weighted_ANC_prop[, .(
  est = quantile(mean, 0.5,   na.rm = TRUE),
  lci = quantile(mean, 0.025, na.rm = TRUE),
  uci = quantile(mean, 0.975, na.rm = TRUE)
), by = year]

setorder(weighted_ANC_prop_CrI, year)


anc_pooled_prop = ggplot(weighted_ANC_prop_CrI) +
  geom_ribbon(
    mapping = aes(
      x = year,
      ymax = 1 - lci,
      ymin = 1 - uci
    ),
    fill = "salmon",
    alpha = 0.5
  ) +
  geom_line(mapping = aes(x = year, y = 1 - est),
            colour = "salmon") +
  scale_x_continuous(limits = c(2015, 2023),
                     breaks = 2015:2023) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Proportion of Diagnoses That are Reached by\nthe ANC Testing Modality among WLHIV:\n2015-2023 Pooled among Countries with a Decline",
       x = NULL,
       y = "Proportion of New Diagnoses from ANC") +
  geom_hline(yintercept = c(0.25, 0.75), linetype = 3) +
  geom_hline(
    yintercept = c(0.5),
    linetype = 2,
    colour = "black"
  ) +
  theme_minimal() +
  theme(
    #legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    # Center the title,
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(
      size = 12,
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    axis.text.y = element_text(size = 12),
    axis.ticks.x = element_line(),
    axis.ticks.length = unit(0.15, "cm")
  )


ggsave(
  plot = anc_pooled_prop,
  file = paste0(path_out, "/pooled_prop_anc.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)
