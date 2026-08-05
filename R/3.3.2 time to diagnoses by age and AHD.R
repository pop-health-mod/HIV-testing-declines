source(here::here("0.6 time dx functions.R"))
source(here::here("0.4 functions.R"))
source(here::here("1.1 tot test out.R"))
source(here::here("1.0 simmod.R"))
library(tidyverse)
library(scales)

path_out <- here::here("outputs/paper 2026/AHD/TDX")
counter_years <-  read_rds("data/counter_years.rds")
# observed
out_dir <- here::here("outputs/AHD")

rd <- function(tag) {
  f <- file.path(out_dir, paste0(tag, ".rda"))
  if (file.exists(f))
    readRDS(f)
  else
    list()
}

## ---- factual ----
simul_tdxB_unpool <- rd("both time to dx all 1")

## ---- counterfactual ----
simul_tdxB_unpool_counter <- rd("both time to dx counter 1")

# extract pooled estimats for both
simul_tdxB   <- extract_pooled(simul_tdxB_unpool)
simul_tdxBcount   <- extract_pooled(simul_tdxB_unpool_counter)

tdx_male_all <- pool_age_groups(simul_tdxB_unpool,sex_grps = "male")
tdx_female_all <- pool_age_groups(simul_tdxB_unpool,sex_grps = "female")

tdx_male_all_c_pooled <- pool_age_groups(simul_tdxB_unpool_counter,sex_grps = "male")
tdx_female_all_c_pooled <- pool_age_groups(simul_tdxB_unpool_counter,sex_grps = "female")


# male
tdx_male_15_24 <- pool_age_groups(simul_tdxB_unpool, sex_grps = "male", age_grps = "15-24")
tdx_male_25_34 <- pool_age_groups(simul_tdxB_unpool, sex_grps = "male", age_grps = "25-34")
tdx_male_35_49 <- pool_age_groups(simul_tdxB_unpool, sex_grps = "male", age_grps = "35-49")
tdx_male_50_99 <- pool_age_groups(simul_tdxB_unpool, sex_grps = "male", age_grps = "50-99")
# female
tdx_female_15_24 <- pool_age_groups(simul_tdxB_unpool, sex_grps = "female", age_grps = "15-24")
tdx_female_25_34 <- pool_age_groups(simul_tdxB_unpool, sex_grps = "female", age_grps = "25-34")
tdx_female_35_49 <- pool_age_groups(simul_tdxB_unpool, sex_grps = "female", age_grps = "50-99")
tdx_female_50_99 <- pool_age_groups(simul_tdxB_unpool, sex_grps = "female", age_grps = "50-99")


# male
tdx_male_15_24_c <- pool_age_groups(simul_tdxB_unpool_counter, sex_grps = "male", age_grps = "15-24")
tdx_male_25_34_c <- pool_age_groups(simul_tdxB_unpool_counter, sex_grps = "male", age_grps = "25-34")
tdx_male_35_49_c <- pool_age_groups(simul_tdxB_unpool_counter, sex_grps = "male", age_grps = "35-49")
tdx_male_50_99_c <- pool_age_groups(simul_tdxB_unpool_counter, sex_grps = "male", age_grps = "50-99")
# female
tdx_female_15_24_c <- pool_age_groups(simul_tdxB_unpool_counter, sex_grps = "female", age_grps = "15-24")
tdx_female_25_34_c <- pool_age_groups(simul_tdxB_unpool_counter, sex_grps = "female", age_grps = "25-34")
tdx_female_35_49_c <- pool_age_groups(simul_tdxB_unpool_counter, sex_grps = "female", age_grps = "50-99")
tdx_female_50_99_c <- pool_age_groups(simul_tdxB_unpool_counter, sex_grps = "female", age_grps = "50-99")

#isolate to only countries with a decline

## ---- initialization ----
tdx_agg_simul_male_subset_all = list()
tdx_agg_simul_female_subset_all = list()

tdx_agg_simul_male_subset_15_24 = list()
tdx_agg_simul_female_subset_15_24 = list()

tdx_agg_simul_male_subset_25_34 = list()
tdx_agg_simul_female_subset_25_34 = list()

tdx_agg_simul_male_subset_35_49 = list()
tdx_agg_simul_female_subset_35_49 = list()

tdx_agg_simul_male_subset_50_99 = list()
tdx_agg_simul_female_subset_50_99 = list()

## ---- loop ----
for (i in 1:length(tdx_male_all_c_pooled)) {
  cnt = names(tdx_male_all_c_pooled)[i]
  tdx_agg_simul_male_subset_all[[cnt]] = tdx_male_all[[cnt]]
  tdx_agg_simul_female_subset_all[[cnt]] = tdx_female_all[[cnt]]
  
  tdx_agg_simul_male_subset_15_24[[cnt]] = tdx_male_15_24[[cnt]]
  tdx_agg_simul_female_subset_15_24[[cnt]] = tdx_female_15_24[[cnt]]
  
  tdx_agg_simul_male_subset_25_34[[cnt]] = tdx_male_25_34[[cnt]]
  tdx_agg_simul_female_subset_25_34[[cnt]] = tdx_female_25_34[[cnt]]
  
  tdx_agg_simul_male_subset_35_49[[cnt]] = tdx_male_35_49[[cnt]]
  tdx_agg_simul_female_subset_35_49[[cnt]] = tdx_female_35_49[[cnt]]
  
  tdx_agg_simul_male_subset_50_99[[cnt]] = tdx_male_50_99[[cnt]]
  tdx_agg_simul_female_subset_50_99[[cnt]] = tdx_female_50_99[[cnt]]
}

tdx_agg_simul_male_diff = list()
tdx_agg_simul_female_diff = list()
# calculate AHD difference
for (i in 1:length(tdx_agg_simul_male_subset_all)) {
  cnt = names(tdx_agg_simul_male_subset_all)[i]
  print(cnt)
  if (!is.null(tdx_male_all_c_pooled[[cnt]])) {
    tryCatch(
      expr = {
        yearsim = 2015:2023
        
        diff_men = subset(tdx_agg_simul_male_subset_all[[cnt]]$out_simul_tdx_all, year %in% yearsim)
        diff_women = subset(tdx_agg_simul_female_subset_all[[cnt]]$out_simul_tdx_all, year %in% yearsim)
        
        diff_men$prb350cd4 = (
          subset(tdx_male_all_c_pooled[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 11] -
            subset(tdx_agg_simul_male_subset_all[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 11]
        )
        diff_women$prb350cd4 = (
          subset(tdx_female_all_c_pooled[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 11] -
            subset(tdx_agg_simul_female_subset_all[[cnt]]$out_simul_tdx_all, year %in% yearsim)[, 11]
        )
        
        
        tdx_agg_simul_male_diff[[cnt]]$out_simul_tdx_all = diff_men
        tdx_agg_simul_female_diff[[cnt]]$out_simul_tdx_all = diff_women
        
        
      },
      error = function(e) {
        message("Caught an error: ", e$message)
        
      },
      finally = {
        
      }
    )
  }
  
}


## ---- Agg_simul_pool_time_dx_prev_2 ----
# all
male_agg = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_subset_all, sex = "male")
female_agg = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_subset_all, sex = "female")
male_agg_counter = Agg_simul_pool_time_dx_prev_2(tdx_male_all_c_pooled, sex = "male")
female_agg_counter = Agg_simul_pool_time_dx_prev_2(tdx_female_all_c_pooled, sex = "female")

# 15-24
male_agg_15_24 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_subset_15_24, sex = "male", age = "15-24")
female_agg_15_24 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_subset_15_24, sex = "female", age = "15-24")
male_agg_counter_15_24 = Agg_simul_pool_time_dx_prev_2(tdx_male_15_24_c, sex = "male", age = "15-24")
female_agg_counter_15_24 = Agg_simul_pool_time_dx_prev_2(tdx_female_15_24_c, sex = "female", age = "15-24")

# 25-34
male_agg_25_34 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_subset_25_34, sex = "male", age = "25-34")
female_agg_25_34 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_subset_25_34, sex = "female", age = "25-34")
male_agg_counter_25_34 = Agg_simul_pool_time_dx_prev_2(tdx_male_25_34_c, sex = "male", age = "25-34")
female_agg_counter_25_34 = Agg_simul_pool_time_dx_prev_2(tdx_female_25_34_c, sex = "female", age = "25-34")

# 35-49
male_agg_35_49 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_subset_35_49, sex = "male", age = "35-49")
female_agg_35_49 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_subset_35_49, sex = "female", age = "35-49")
male_agg_counter_35_49 = Agg_simul_pool_time_dx_prev_2(tdx_male_35_49_c, sex = "male", age = "35-49")
female_agg_counter_35_49 = Agg_simul_pool_time_dx_prev_2(tdx_female_35_49_c, sex = "female", age = "35-49")

#50 - 99
male_agg_50_99 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_subset_50_99, sex = "male", age = "50-99")
female_agg_50_99 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_subset_50_99, sex = "female", age = "50-99")
male_agg_counter_50_99 = Agg_simul_pool_time_dx_prev_2(tdx_male_50_99_c, sex = "male", age = "50-99")
female_agg_counter_50_99 = Agg_simul_pool_time_dx_prev_2(tdx_female_50_99_c, sex = "female", age = "50-99")


# all AHD diff
male_diffAHD = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_diff, sex = "male")
female_diffAHD = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_diff, sex = "female")


# ---- graph by age ----
# start_year = 2015
# end_year   = 2023
# sex   <- "female"   # Options: "male", "female"
# if (sex == "female") {
#   col  = "firebrick3"; col2 = "steelblue"; sex2 = "male"
# } else {
#   col  = "steelblue";  col2 = "firebrick3"; sex2 = "female"
# }
# 
# age_grps <- c("15_24", "25_34", "35_49", "50_99")
# 
# # stack the four age-specific aggregates for each of the 4 required objects
# stack_ages <- function(prefix) {
#   do.call(rbind, lapply(age_grps, function(ag) {
#     d <- get(paste0(prefix, "_", ag))
#     d$age_grp <- ag
#     d
#   }))
# }
# df_sex = rbind(tdx_agg_simul_female_subset_15_24,
#                tdx_agg_simul_female_subset_25_34,
#                tdx_agg_simul_female_subset_25_34)
# 
# 
# # tag each frame with sex + observed/counterfactual, then bind
# df_all <- bind_rows(
#   df_sex    %>% mutate(sex = sex,  type = "Observed"),
#   df_sex_c  %>% mutate(sex = sex,  type = "Counterfactual"),
#   df_sex2   %>% mutate(sex = sex2, type = "Observed"),
#   df_sex2_c %>% mutate(sex = sex2, type = "Counterfactual")
# ) %>%
#   mutate(
#     sex     = factor(tools::toTitleCase(sex), levels = c("Male", "Female")),
#     type    = factor(type, levels = c("Observed", "Counterfactual")),
#     age_grp = factor(age_grp,
#                      levels = c("15_24", "25_34", "35_49", "50_99"),
#                      labels = c("15–24", "25–34", "35–49", "50–99"))
#   )
# 
# sex_cols <- c(Male = "steelblue", Female = "firebrick3")
# 
# plot <- ggplot(df_all, aes(x = year, group = interaction(sex, type))) +
#   geom_ribbon(aes(ymin = time_dx_lci, ymax = time_dx_uci, fill = sex),
#               alpha = 0.18, colour = NA) +
#   geom_line(aes(y = time_dx, colour = sex, linetype = type), linewidth = 1) +
#   
#   facet_wrap(~ age_grp, nrow = 2) +
#   
#   scale_colour_manual(values = sex_cols, name = "Sex") +
#   scale_fill_manual(values = sex_cols, guide = "none") +
#   scale_linetype_manual(values = c(Observed = 1, Counterfactual = 2),
#                         name = "Scenario") +
#   scale_x_continuous(breaks = seq(start_year, end_year, 2),
#                      limits = c(start_year, end_year)) +
#   scale_y_continuous(limits = c(0, 6)) +
#   
#   labs(
#     title = paste0("Pooled Median Time to Diagnosis or AIDS Death\n",
#                    "by Sex and Age Group in Countries With a Decline: ",
#                    start_year, "–", end_year),
#     x = NULL,
#     y = "Years to Diagnosis or AIDS Death"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     plot.title      = element_text(hjust = 0.5, size = 15, face = "bold"),
#     axis.title.y    = element_text(size = 14),
#     axis.text.x     = element_text(size = 11, angle = 45, vjust = 1, hjust = 1),
#     axis.text.y     = element_text(size = 12),
#     strip.text      = element_text(size = 13, face = "bold"),
#     panel.spacing   = unit(1.1, "lines"),
#     legend.position = "bottom",
#     legend.box      = "horizontal",
#     legend.title    = element_text(face = "bold")
#   )
# plot
# 
# ggsave(plot = plot,
#        file = paste0(path_out, "/_tdx_paper male_female by age.png"),
#        width = 9, height = 7, dpi = 500, scale = 1)


# ---- graphs by age 15-24 vs 25-99 ----
tdx_agg_simul_male_subset_25_99 = list()
tdx_agg_simul_female_subset_25_99 = list()

tdx_agg_simul_male_subset_25_99counter = list()
tdx_agg_simul_female_subset_25_99counter = list()

for (cnt in names(tdx_agg_simul_male_subset_25_34)) {
  x = tdx_agg_simul_male_subset_25_34[[cnt]]$out_simul_tdx_all
  y = tdx_agg_simul_male_subset_35_49[[cnt]]$out_simul_tdx_all
  z = tdx_agg_simul_male_subset_50_99[[cnt]]$out_simul_tdx_all
  
  groups_all <- data.table::rbindlist(list(x,y,z))
  
  tdx_agg_simul_male_subset_25_99[[cnt]]$out_simul_tdx_all <- groups_all[, .(
    time_dx_avg = weighted.mean(time_dx_avg , w),
    time_dx_med = weighted.mean(time_dx_med, w),
    prb6mo      = weighted.mean(prb6mo,   w),
    prb1yr      = weighted.mean(prb1yr,   w),
    prb2yr      = weighted.mean(prb2yr,   w),
    prb5yr      = weighted.mean(prb5yr,   w),
    prb500cd4   = weighted.mean(prb500cd4, w),
    prb350cd4   = weighted.mean(prb350cd4, w),
    propdx      = weighted.mean(propdx,   w),
    w           = sum(w)
  ), by = .(year, sex,sampi)]
  
  xf = tdx_agg_simul_female_subset_25_34[[cnt]]$out_simul_tdx_all
  yf = tdx_agg_simul_female_subset_35_49[[cnt]]$out_simul_tdx_all
  zf = tdx_agg_simul_female_subset_50_99[[cnt]]$out_simul_tdx_all
  
  groups_all <- data.table::rbindlist(list(xf,yf,zf))
  
  tdx_agg_simul_female_subset_25_99[[cnt]]$out_simul_tdx_all <- groups_all[, .(
    time_dx_avg = weighted.mean(time_dx_avg , w),
    time_dx_med = weighted.mean(time_dx_med, w),
    prb6mo      = weighted.mean(prb6mo,   w),
    prb1yr      = weighted.mean(prb1yr,   w),
    prb2yr      = weighted.mean(prb2yr,   w),
    prb5yr      = weighted.mean(prb5yr,   w),
    prb500cd4   = weighted.mean(prb500cd4, w),
    prb350cd4   = weighted.mean(prb350cd4, w),
    propdx      = weighted.mean(propdx,   w),
    w           = sum(w)
  ), by = .(year, sex,sampi)]
  
  
  x = tdx_male_25_34_c[[cnt]]$out_simul_tdx_all
  y = tdx_male_35_49_c[[cnt]]$out_simul_tdx_all
  z = tdx_male_50_99_c[[cnt]]$out_simul_tdx_all
  
  groups_all <- data.table::rbindlist(list(x,y,z))
  
  tdx_agg_simul_male_subset_25_99counter[[cnt]]$out_simul_tdx_all <- groups_all[, .(
    time_dx_avg = weighted.mean(time_dx_avg , w),
    time_dx_med = weighted.mean(time_dx_med, w),
    prb6mo      = weighted.mean(prb6mo,   w),
    prb1yr      = weighted.mean(prb1yr,   w),
    prb2yr      = weighted.mean(prb2yr,   w),
    prb5yr      = weighted.mean(prb5yr,   w),
    prb500cd4   = weighted.mean(prb500cd4, w),
    prb350cd4   = weighted.mean(prb350cd4, w),
    propdx      = weighted.mean(propdx,   w),
    w           = sum(w)
  ), by = .(year, sex,sampi)]
  
  
  xf = tdx_female_25_34_c[[cnt]]$out_simul_tdx_all
  yf = tdx_female_35_49_c[[cnt]]$out_simul_tdx_all
  zf = tdx_female_50_99_c[[cnt]]$out_simul_tdx_all
  
  groups_all <- data.table::rbindlist(list(xf,yf,zf))
  
  tdx_agg_simul_female_subset_25_99counter[[cnt]]$out_simul_tdx_all <- groups_all[, .(
    age = "25-34+35-49+50-99",
    time_dx_avg = weighted.mean(time_dx_avg , w),
    time_dx_med = weighted.mean(time_dx_med, w),
    prb6mo      = weighted.mean(prb6mo,   w),
    prb1yr      = weighted.mean(prb1yr,   w),
    prb2yr      = weighted.mean(prb2yr,   w),
    prb5yr      = weighted.mean(prb5yr,   w),
    prb500cd4   = weighted.mean(prb500cd4, w),
    prb350cd4   = weighted.mean(prb350cd4, w),
    propdx      = weighted.mean(propdx,   w),
    w           = sum(w)
  ), by = .(year, sex,sampi)]
}


male_agg_25_99 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_subset_25_99, sex = "male", age = c("25-34+35-49+50-99"))
female_agg_25_99 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_subset_25_99, sex = "female",age = c("25-34+35-49+50-99"))
male_agg_counter_25_99 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_male_subset_25_99counter, sex = "male", age = c("25-34+35-49+50-99"))
female_agg_counter_25_99 = Agg_simul_pool_time_dx_prev_2(tdx_agg_simul_female_subset_25_99counter, sex = "female", age = c("25-34+35-49+50-99"))



library(patchwork)

start_year <- 2015
end_year   <- 2023

age_lvls <- c("Female 15-24", "Female 25-99", "Male 15-24", "Male 25-99")
age_cols <- c("Female 15-24" = "goldenrod",  "Female 25-99" = "darkorange2",
              "Male 15-24"   = "goldenrod",  "Male 25-99"   = "darkorange2")
lty_lvls <- c("Observed", "Counterfactual")
lty_vals <- c("Observed" = 1, "Counterfactual" = 2)
ci_lab   <- "95% Credible Interval"

# ---- panel builder: NO legend (legend is added separately below) 
make_ttd_plot <- function(sex, show_ylab = TRUE) {
  if (sex == "female") { col_young <- "goldenrod"; col_old <- "darkorange2";alpha_old = c(0.3,0.3)
  } else               { col_young <- "goldenrod"; col_old <- "darkorange2";alpha_old = c(0.3,0.3) }
  
  ty        <- tools::toTitleCase(sex)
  lab_young <- factor(paste(ty, "15-24"), levels = age_lvls)
  lab_old   <- factor(paste(ty, "25-99"), levels = age_lvls)
  obs       <- factor("Observed",       levels = lty_lvls)
  cf        <- factor("Counterfactual", levels = lty_lvls)
  
  young         <- paste0(sex, "_agg_15_24")
  young_counter <- paste0(sex, "_agg_counter_15_24")
  old           <- paste0(sex, "_agg_25_99")
  old_counter   <- paste0(sex, "_agg_counter_25_99")
  
  ggplot() +
    geom_ribbon(data = get(old), aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci, group = 1),
                fill = col_old, alpha = alpha_old[2]) +
    geom_line(data = get(old), aes(x = year, y = time_dx, colour = lab_old, linetype = obs),
              linewidth = 1) +
    geom_ribbon(data = get(old_counter), aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci, group = 1),
                fill = col_old, alpha = alpha_old[2]) +
    geom_line(data = get(old_counter), aes(x = year, y = time_dx, colour = lab_old, linetype = cf),
              linewidth = 1) +
    geom_ribbon(data = get(young), aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci, group = 1),
                fill = col_young, alpha = alpha_old[1]) +
    geom_line(data = get(young), aes(x = year, y = time_dx, colour = lab_young, linetype = obs),
              linewidth = 1) +
    geom_ribbon(data = get(young_counter), aes(x = year, ymin = time_dx_lci, ymax = time_dx_uci, group = 1),
                fill = col_young, alpha = alpha_old[1]) +
    geom_line(data = get(young_counter), aes(x = year, y = time_dx, colour = lab_young, linetype = cf),
              linewidth = 1) +
    scale_colour_manual(values = age_cols, drop = FALSE) +
    scale_linetype_manual(values = lty_vals, drop = FALSE) +
    theme_minimal() +
    labs(title = ifelse(ty == "Female","Women","Men"), x = NULL,
         y = if (show_ylab) "Years to Diagnosis or AIDS Death" else NULL) +
    scale_x_continuous(breaks = seq(start_year, end_year, 1), limits = c(start_year, 2023)) +
    coord_cartesian(ylim = c(0, 4.5)) +
    theme(
      legend.position = "none",
      plot.title   = element_text(hjust = 0.5, size = 15, face = "bold"),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x  = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
      axis.text.y  = element_text(size = 15)
    )
}
# ---- assemble: two panels on top, single legend row beneath 
p_female <- make_ttd_plot("female", show_ylab = TRUE)
p_male   <- make_ttd_plot("male",   show_ylab = FALSE)

# ---- one dummy plot whose ONLY purpose is to emit the full legend 
leg_df <- expand.grid(grp = factor(age_lvls, levels = age_lvls),
                      lty = factor(lty_lvls, levels = lty_lvls))
leg_df$grp = rep(c("15-24","25-99"),4)
leg_df$x <- start_year; leg_df$y <- 0; leg_df$ci <- ci_lab

age_cols <- c("15-24" = "goldenrod",  "25-99" = "darkorange2")

p_leg <- ggplot(leg_df[c(1,2,5,6),], aes(x, y)) +
  # alpha here is LEGEND-ONLY (p_leg is never drawn), so bump it for a
  # clearly visible band swatch without touching the 0.25 panel ribbons
  geom_ribbon(aes(ymin = y, ymax = y, fill = grp), alpha = 0.40) +
  geom_line(aes(colour = grp, linetype = lty), linewidth = 1) +
  scale_colour_manual(values = age_cols[1:2], drop = FALSE, name = NULL) +
  scale_fill_manual(  values = age_cols[1:2], drop = FALSE, name = NULL) +
  scale_linetype_manual(values = lty_vals, drop = FALSE, name = NULL) +
  guides(
    # IDENTICAL guide spec on colour AND fill => the two merge into a single
    # guide; each key then overlays the line (colour) on the band (fill)
    colour   = guide_legend(order = 1, nrow = 1, byrow = TRUE,
                            override.aes = list(linetype = 1, linewidth = 1)),
    fill     = guide_legend(order = 1, nrow = 1, byrow = TRUE,
                            override.aes = list(linetype = 1, linewidth = 1)),
    linetype = guide_legend(order = 2, nrow = 1,
                            override.aes = list(colour = "black", linewidth = 0.8))
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.box = "horizontal")
# ---- robust guide-box extractor (works pre- and post-ggplot2 3.5) 
get_legend_grob <- function(p) {
  g   <- ggplotGrob(p)
  nm  <- vapply(g$grobs, function(z) z$name, character(1))
  idx <- grep("guide-box", nm)
  keep <- idx[!vapply(g$grobs[idx], inherits, logical(1), "zeroGrob")]
  if (length(keep) == 0) keep <- idx            # fallback for older ggplot2
  g$grobs[[keep[1]]]
}
legend <- get_legend_grob(p_leg)


panel <- (p_female | p_male) / wrap_elements(legend) +
  plot_layout(heights = c(1, 0.18)) +
  plot_annotation(
    title = paste0(
      "Pooled Median Time to Diagnosis or AIDS Death by Age Group\n",
      "in Countries With a Decline: ", start_year, "-", end_year
    ),
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  )
panel

ggsave(
  plot  = panel,
  file  = paste0(path_out, "/_tdx_paper male_female panel 15_24 vs 25_99.png"),
  width = 10, height = 6, dpi = 500, scale = 1
)


# ----plot male_female AHD----
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
      ymin = 1 - prb350cd4_lci,
      ymax = 1 - prb350cd4_uci,
      group = 1
    ),
    fill = col,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_name),
    aes(y = 1 - prb350cd4, x = year),
    color = col,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_name2),
    aes(
      x = year,
      ymin = 1 - prb350cd4_lci,
      ymax = 1 - prb350cd4_uci,
      group = 1
    ),
    fill = col,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_name2),
    aes(y = 1 - prb350cd4, x = year),
    color = col,
    linewidth = 1,
    linetype = 2
  ) +
  
  
  geom_ribbon(
    data = get(var_namea),
    aes(
      x = year,
      ymin = 1 - prb350cd4_lci,
      ymax = 1 - prb350cd4_uci,
      group = 1
    ),
    fill = col2,
    alpha = ifelse(sex == "female", 0.2, 0.3)
  ) +
  geom_line(
    data = get(var_namea),
    aes(y = 1 - prb350cd4, x = year),
    color = col2,
    linewidth = 1
  ) +
  geom_ribbon(
    data = get(var_namea2),
    aes(
      x = year,
      ymin = 1 - prb350cd4_lci,
      ymax = 1 - prb350cd4_uci,
      group = 1
    ),
    fill = col2,
    alpha = 0.2
  ) +
  geom_line(
    data = get(var_namea2),
    aes(y = 1 - prb350cd4, x = year),
    color = col2,
    linewidth = 1,
    linetype = 2
  ) +
  theme_minimal() +
  labs(
    title = paste0(
      "Pooled Probability of Diagnosis with AHD\n",
      "by Gender in Countries With a Decline:\n",
      start_year,
      "-",
      end_year
    ),
    x = NULL,
    y = "Probability of Diagnosis with AHD"
  ) +
  scale_x_continuous(breaks = seq(start_year, end_year, 1),
                     limits = c(start_year, 2023)) +
  scale_y_continuous(limits = c(0, 0.5)) +
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
  file = paste0(path_out, "/_tdx_paper male_female AHD.png"),
  width = 5,
  height = 5,
  dpi = 500,
  scale = 1
)


# ---- plot ahd bar ----
mk <- function(obj, sex_lab, scen_lab) {
  get(obj) %>%
    transmute(
      year,
      sex      = sex_lab,
      scenario = scen_lab,
      ahd      = 1 - prb350cd4,
      ahd_lci  = 1 - prb350cd4_uci,   # bounds flip under 1 - x
      ahd_uci  = 1 - prb350cd4_lci
    )
}

df_ahd <- bind_rows(
  mk("male_agg",           "Male",   "Observed"),
  mk("male_agg_counter",   "Male",   "Counterfactual"),
  mk("female_agg",         "Female", "Observed"),
  mk("female_agg_counter", "Female", "Counterfactual")
) %>%
  mutate(
    sex      = factor(sex,      levels = c("Male", "Female")),
    scenario = factor(scenario, levels = c("Observed", "Counterfactual"))
  )


bar_w  <- 0.40
offset <- 0.225

lvls <- c("Men: additional due\nto declines",   "Men: No declines",
          "Women: additional due\nto declines", "Women: No declines")

df_seg <- df_ahd %>%
  select(year, sex, scenario, ahd) %>%
  pivot_wider(names_from = scenario, values_from = ahd) %>%
  mutate(
    excess   = pmax(Observed - Counterfactual, 0),
    baseline = Observed - excess,
    year_num = as.integer(as.character(year)),
    xc = year_num + ifelse(sex == "Male", -offset, offset)
  ) %>%
  pivot_longer(c(baseline, excess), names_to = "seg", values_to = "p") %>%
  mutate(seg = factor(seg, levels = c("baseline", "excess"))) %>%
  arrange(year_num, sex, seg) %>%
  group_by(year_num, sex) %>%
  mutate(ymax = cumsum(p), ymin = ymax - p) %>%
  ungroup() %>%
  mutate(fill_grp = factor(case_when(
    seg == "excess" ~ paste0(ifelse(sex == "Female","Women","Men"), ": additional due\nto declines"),
    TRUE            ~ paste0(ifelse(sex == "Female","Women","Men"), ": No declines")), levels = lvls))

fill_vals <- c(
  "Men: additional due\nto declines"   = "steelblue4",
  "Men: No declines"         = "#C3D2E6",
  "Women: additional due\nto declines" = "firebrick",
  "Women: No declines"       = "#EBC7BE")

plot_bar <- ggplot(df_seg) +
  geom_rect(aes(xmin = xc - bar_w/2, xmax = xc + bar_w/2,
                ymin = ymin, ymax = ymax, fill = fill_grp),
            colour = "white", linewidth = 0.3) +
  scale_x_continuous(breaks = sort(unique(df_seg$year_num)),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.30, 0.05),
                     limits = c(0, 0.31),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = fill_vals, name = NULL,
                    guide = guide_legend(ncol = 2, byrow = FALSE,
                                         keywidth  = unit(14, "pt"),
                                         keyheight = unit(14, "pt"))) +
  labs(title    = "Pooled Composition of Diagnoses\n with AHD by Gender 2015-2023",
       x = NULL, y = "Share of people diagnosed") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title.y       = element_text(size = 17, colour = "grey25", margin = margin(r = 8)),
    axis.text          = element_text(size = 17),
    axis.text.x        = element_text(size = 17, angle = 45,hjust = 1,vjust=1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
    axis.line.x        = element_line(colour = "grey75", linewidth = 0.4),
    axis.ticks.x       = element_line(colour = "grey75", linewidth = 0.3),
    legend.position    = "none",
    legend.text        = element_text(size = 15),
    legend.margin      = margin(t = 6),
  )

plot_bar
ggsave(plot = plot_bar,
       file = paste0(path_out, "/_tdx_paper stacked bar sex dodged.png"),
       width = 5, height = 5, dpi = 500, scale = 1)

plot_bar <- ggplot(df_seg) +
  geom_rect(aes(xmin = xc - bar_w/2, xmax = xc + bar_w/2,
                ymin = ymin, ymax = ymax, fill = fill_grp),
            colour = "white", linewidth = 0.3) +
  scale_x_continuous(breaks = sort(unique(df_seg$year_num)),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.30, 0.05),
                     limits = c(0, 0.31),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = fill_vals, name = NULL,
                    guide = guide_legend(ncol = 2, byrow = FALSE,
                                         keywidth  = unit(14, "pt"),
                                         keyheight = unit(14, "pt"))) +
  labs(title    = "Pooled Composition of Diagnoses\n with AHD by Gender 2015-2023",
       x = NULL, y = "Share of people diagnosed") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title.y       = element_text(size = 17, colour = "grey25", margin = margin(r = 8)),
    axis.text          = element_text(size = 17),
    axis.text.x        = element_text(size = 17, angle = 45,hjust = 1,vjust=1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
    axis.line.x        = element_line(colour = "grey75", linewidth = 0.4),
    axis.ticks.x       = element_line(colour = "grey75", linewidth = 0.3),
    legend.position    = "bottom",
    legend.text        = element_text(size = 15),
    legend.margin      = margin(t = 6),
  )

plot_bar
ggsave(plot = plot_bar,
       file = paste0(path_out, "/_tdx_paper stacked bar sex dodged legend.png"),
       width = 8, height = 5, dpi = 500, scale = 1)


