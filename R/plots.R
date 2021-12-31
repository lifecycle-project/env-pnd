################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Produces figures for manuscript 
## Date: 3rd August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(metafor)
library(here)
library(dsTim)

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

word_land_half <- 29.7 /2
################################################################################
# Figure S3: Missingness forest plots  
################################################################################
miss.pdata <- single_miss.fit %>% 
  map(
     dh.lmTab,
      type = "ipd", 
      ci_format = "separate", 
      direction = "wide", 
      round = 50) %>%
  bind_rows(.id = "exposure") %>% 
  dplyr::filter(variable == "env_pnd$ppd1") %>%
  mutate(across(est:uppci, ~exp(.x))) %>%
  left_join(., exp_full_names, by = "exposure")


png(
  file = here("figures", "miss_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

  forest(
    x = miss.pdata %>% pull(est), 
    ci.lb = miss.pdata %>% pull(lowci),
    ci.ub = miss.pdata %>% pull(uppci), 
    slab = miss.pdata %>% pull(fullname),
    xlab = "Associations between postnatal depression and probability of being complete case", 
    cex = 0.8, 
    cex.axis = 0.8,
    header = c("Exposure", "Estimate [95% CI]"), 
    refline = 1, 
    xlim = c(0.8, 1.2),
    alim = c(0.9, 1.1), 
    steps = 5, 
    digits = c(3, 2))

dev.off()


################################################################################
# SINGLE EXPOSURE MODELS  
################################################################################
single.pdata <- single.fit$ipd %>%
  map(
    dh.lmTab,
    type = "ipd", 
    ci_format = "separate", 
    direction = "wide", 
    family = "binomial",
    round = 50) %>%
  bind_rows(.id = "name") %>% 
  mutate(across(est:uppci, ~exp(.x))) %>%
  left_join(., exp_ref, by = "name")

forestWrap <- function(
  coefs, fit, title, x_limits, axis_limits, other_labs, digits = 3, scale = 0.7,
  y_tit = "Environmental exposure", x_tit = "OR [95% CI]"){
  
  forest(
    x = coefs %>% pull(est), 
    ci.lb = coefs %>% pull(lowci),
    ci.ub = coefs %>% pull(uppci), 
    slab = coefs %>% pull(full_name), 
    xlab = title, 
    cex = scale, 
    cex.lab = scale,
    cex.axis = scale,
    ilab =  cbind(
      fit %>% map_int(function(x){x$Nvalid}),
      fit %>% map(function(x){length(x$disclosure.risk)})),
    ilab.xpos = other_labs,
    header = c(y_tit, x_tit), 
    refline = 1, 
    xlim = x_limits,
    alim = axis_limits, 
    steps = 7, 
    digits = c(digits, 2))
}
################################################################################
# Figure 4: natural spaces   
################################################################################
green.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "ndvi300_preg_iqr_p", "greenyn300_preg1", "blueyn300_preg1"))
 
png(
  file = here("figures", "single_nat.png"), 
  width = word_full, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

green.pdata %>%
  forestWrap(
    fit = single.fit$ipd[c("ndvi", "green", "blue")],
    other_labs = c(subj_head, stud_head),
    x_limits = c(-3.0, 2.6), 
    axis_limits = c(0.2, 1.4), 
    digits = 2, 
    title = "Odds ratio for postnatal depression per IQR change in exposure")

text(subj_head, 5, "N subjects", cex = 0.7, font = 2)
text(stud_head, 5, "N studies", cex = 0.7, font = 2)

dev.off()

################################################################################
# Figure 5: Ambient air polution  
################################################################################
pol.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "no2_preg_iqr_p", "pm25_preg_iqr_p", "pm10_preg_iqr_p"))

png(
  file = here("figures", "single_pol.png"), 
  width = word_full, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

pol.pdata %>%
  forestWrap(
    fit = single.fit$ipd[c("no2", "pm25", "pm10")],
    other_labs = c(subj_head, stud_head),
    x_limits = c(-3.0, 2.6), 
    axis_limits = c(0.2, 1.4), 
    digits = 2, 
    title = "Odds ratio for postnatal depression per IQR change in exposure")

text(subj_head, 5, "N subjects", cex = 0.7, font = 2)
text(stud_head, 5, "N studies", cex = 0.7, font = 2)

dev.off()


################################################################################
# Figure 6: Road traffic noise  
################################################################################
noise.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "lden_c_preg2", "lden_c_preg3", "lden_c_preg4", "lden_c_preg5")) %>%
  mutate(full_name = case_when(
    variable == "lden_c_preg2" ~ "55-55.9 dB", 
    variable == "lden_c_preg3" ~ "60-64.9 dB", 
    variable == "lden_c_preg4" ~ "65-69.9 dB", 
    variable == "lden_c_preg5" ~ ">70 dB"))

n_cat_lden <- descriptives$categorical %>%
  dplyr::filter(
    variable == "lden_c_preg" & cohort == "combined" & !is.na(category) & 
      category != 1) %>%
  pull(value)

png(
  file = here("figures", "single_noise.png"), 
  width = word_full, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

forest(
  x = noise.pdata %>% pull(est), 
  ci.lb = noise.pdata %>% pull(lowci),
  ci.ub = noise.pdata %>% pull(uppci), 
  slab = noise.pdata %>% pull(full_name), 
  xlab = "Odds ratio for postnatal depression per IQR change in exposure", 
  cex = 0.7, 
  cex.lab = 0.7,
  cex.axis = 0.7,
  ilab =  cbind(
    n_cat_lden,
    single.fit$ipd[c("lden", "lden", "lden", "lden")] %>% 
      map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(subj_head, stud_head),
  header = c("Environmental exposure", "OR [95% CI]"), 
  refline = 1, 
  xlim = c(-3.0, 2.6),
  alim = c(0.2, 1.6), 
  steps = 7, 
  digits = c(2, 2))

text(subj_head, 6, "N subjects", cex = 0.7, font = 2)
text(stud_head, 6, "N studies", cex = 0.7, font = 2)

dev.off()

################################################################################
# Figure 7: Built environment  
################################################################################
built.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "frichness300_preg_iqr_p", "walkability_mean_preg_iqr_p", 
    "popdens_preg_iqr_p"))

png(
  file = here("figures", "single_built.png"), 
  width = word_full, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

built.pdata %>%
  forestWrap(
    fit = single.fit$ipd[c("fac_rich", "walk", "pop_dens")],
    other_labs = c(subj_head, stud_head),
    x_limits = c(-3.0, 2.6), 
    axis_limits = c(0.2, 1.4), 
    digits = 2, 
    title = "Odds ratio for postnatal depression per IQR change in exposure")

text(subj_head, 5, "N subjects", cex = 0.7, font = 2)
text(stud_head, 5, "N studies", cex = 0.7, font = 2)

dev.off()

################################################################################
# Prepare data for SLMA plots  
################################################################################
single_slma.pdata <- exp_ref$name %>%
  map(function(x){
    dt.forestDataSLMA(
      obj = single.fit$slma[[x]], 
      mod = single_works.mod[[x]])
  }) %>%
  set_names(exp_ref$name) %>%
  bind_rows(.id = "name") %>%
  left_join(., exp_ref, by = "name") %>%
  dplyr::select(-exposure)

################################################################################
# Figure S4: Green exposures SLMA  
################################################################################
green_slma.pdata <- single_slma.pdata %>%
  dplyr::filter(variable %in% c(
    "ndvi300_preg_iqr_p", "greenyn300_preg1", "blueyn300_preg1")) %>%
  mutate(across(c(beta, ci_5, ci_95), ~exp(.x))) %>%
  left_join(., names_neat, by = "cohort")
  

ndvi_slma_ns <- single.fit$slma[c("ndvi", "green", "blue")] %>%
  map(slmaN) %>%
  unlist %>%
  as.numeric

png(
  file = here("figures", "ndvi_slma_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = green_slma.pdata  %>% pull(beta), 
  ci.lb = green_slma.pdata %>% pull(ci_5),
  ci.ub = green_slma.pdata %>% pull(ci_95), 
  slab = green_slma.pdata %>% pull(cohort_neat), 
  xlab = "Odds ratio for postnatal depression per IQR change in exposure", 
  ilab = ndvi_slma_ns,
  ilab.x = -3.5,
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 1, 
  xlim = c(-8, 6),
  ylim = c(0, 37),
  alim = c(-2, 4), 
  steps = 7, 
  digits = c(2, 2), 
  rows = c(31:24, 20:13, 8:1), 
  col = c(
    rep("black", 7), "#800000",
    rep("black", 7), "#800000", 
    rep("black", 7), "#800000"))

text(-3.5, 36, "N subjects", cex = 0.6, font = 2)

text(-7.55, 32.3, "NDVI", cex = 0.6, font = 2)
text(-6.35, 21.3, "Major green space within 300m", cex = 0.6, font = 2)
text(-6.40, 9.3, "Major blue space within 300m", cex = 0.6, font = 2)

dev.off()

################################################################################
# Figure 4b: ambient air polution   
################################################################################
pol_slma.pdata <- single_slma.pdata %>%
  dplyr::filter(variable %in% c(
    "no2_preg_iqr_p", "pm25_preg_iqr_p", "pm10_preg_iqr_p")) %>%
  mutate(across(c(beta, ci_5, ci_95), ~exp(.x))) %>%
  left_join(., names_neat, by = "cohort")

pol_slma_ns <- single.fit$slma[c("no2", "pm25", "pm10")] %>%
  map(slmaN) %>%
  unlist %>%
  as.numeric

png(
  file = here("figures", "pol_slma_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = pol_slma.pdata  %>% pull(beta), 
  ci.lb = pol_slma.pdata %>% pull(ci_5),
  ci.ub = pol_slma.pdata %>% pull(ci_95), 
  slab = pol_slma.pdata %>% pull(cohort_neat), 
  xlab = "Odds ratio for postnatal depression per IQR change in exposure", 
  ilab = pol_slma_ns,
  ilab.x = -3.5,
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 1, 
  xlim = c(-8, 6),
  ylim = c(0, 37),
  alim = c(-2, 4), 
  steps = 7, 
  digits = c(2, 2), 
  rows = c(31:24, 20:13, 8:2), 
  col = c(
    rep("black", 7), "#800000",
    rep("black", 7), "#800000", 
    rep("black", 6), "#800000"))

text(-3.5, 36, "N subjects", cex = 0.6, font = 2)

text(-7.59, 32.3, "NO2", cex = 0.6, font = 2)
text(-7.49, 21.3, "PM2.5", cex = 0.6, font = 2)
text(-7.50, 9.3, "PM10", cex = 0.6, font = 2)

dev.off()


################################################################################
# Figure 4c: road traffic noise   
################################################################################
noise_slma.pdata <- single_slma.pdata %>%
  dplyr::filter(variable %in% c(
    "lden_c_preg2", "lden_c_preg3", "lden_c_preg4", "lden_c_preg5")) %>%
  mutate(across(c(beta, ci_5, ci_95), ~exp(.x))) %>%
  left_join(., names_neat, by = "cohort") %>%
  mutate(full_name = case_when(
    variable == "lden_c_preg2" ~ "55-55.9 dB", 
    variable == "lden_c_preg3" ~ "60-64.9 dB", 
    variable == "lden_c_preg4" ~ "65-69.9 dB", 
    variable == "lden_c_preg5" ~ ">70 dB")) 

noise_slma.pdata %>% print(n = Inf)

lden_slma_ns <- descriptives$categorical %>%
    dplyr::filter(variable == "lden_c_preg" & !is.na(category) & category != 1) %>%
    dplyr::filter(cohort %in% single.mod$lden$cohorts) %>%
  pull(value)
  
png(
  file = here("figures", "noise_slma_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = noise_slma.pdata  %>% pull(beta), 
  ci.lb = noise_slma.pdata %>% pull(ci_5),
  ci.ub = noise_slma.pdata %>% pull(ci_95), 
  slab = noise_slma.pdata %>% pull(full_name), 
  xlab = "Odds ratio for postnatal depression per IQR change in exposure", 
  ilab = lden_slma_ns,
  ilab.x = -3.5,
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Noise level", "Estimate [95% CI]"), 
  refline = 1, 
  xlim = c(-8, 6),
  ylim = c(0, 51),
  alim = c(-2, 4), 
  steps = 7, 
  digits = c(2, 2), 
  rows = c(46:43, 40:37, 34:31, 28:25, 22:19, 16:13, 10:7, 4:1), 
  col = c(rep("black", 28), rep("#800000", 4)))
  
text(-3.5, 50, "N subjects", cex = 0.6, font = 2)

text(-7.40, 47.3, "ALSPAC", cex = 0.6, font = 2)
text(-7.19, 41.3, "EDEN Nancy", cex = 0.6, font = 2)
text(-7.13, 35.3, "EDEN Poitiers", cex = 0.6, font = 2)
text(-7.51, 29.3, "GENR", cex = 0.6, font = 2)
text(-7.12, 23.3, "INMA Sabadell", cex = 0.6, font = 2)
text(-7.54, 17.3, "MoBa", cex = 0.6, font = 2)
text(-7.44, 11.3, "NINFEA", cex = 0.6, font = 2)
text(-7.12, 5.3, "Pooled results", cex = 0.6, font = 2)

dev.off()


################################################################################
# Figure 4d: built environment   
################################################################################
built_slma.pdata <- single_slma.pdata %>%
  dplyr::filter(variable %in% c(
    "frichness300_preg_iqr_p", "walkability_mean_preg_iqr_p", 
    "popdens_preg_iqr_p")) %>%
  mutate(across(c(beta, ci_5, ci_95), ~exp(.x))) %>%
  left_join(., names_neat, by = "cohort")

built_slma_ns <- single.fit$slma[c("fac_rich", "walk", "pop_dens")] %>%
  map(slmaN) %>%
  unlist %>%
  as.numeric

png(
  file = here("figures", "built_slma_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = built_slma.pdata  %>% pull(beta), 
  ci.lb = built_slma.pdata %>% pull(ci_5),
  ci.ub = built_slma.pdata %>% pull(ci_95), 
  slab = built_slma.pdata %>% pull(cohort_neat), 
  xlab = "Odds ratio for postnatal depression per IQR change in exposure", 
  ilab = built_slma_ns,
  ilab.x = -3.5,
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 1, 
  xlim = c(-8, 6),
  ylim = c(0, 33),
  alim = c(-2, 4), 
  steps = 7, 
  digits = c(2, 2),
  rows = c(28:21, 18:11, 8:1), 
  col = rep(c(rep("black", 7), "#800000"), 3))

text(-3.5, 32, "N subjects", cex = 0.6, font = 2)

text(-7.02, 29.3, "Facility richness", cex = 0.6, font = 2)
text(-7.27, 19.3, "Walkability", cex = 0.6, font = 2)
text(-6.92, 9.3, "Population density", cex = 0.6, font = 2)

dev.off()


################################################################################
# October GA  
################################################################################

## ---- Air polution -----------------------------------------------------------
pol.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "no2_preg_iqr_p", "pm25_preg_iqr_p", "pm10_preg_iqr_p"))

png(
  file = here("figures", "single_pol_ga.png"), 
  width = word_full, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

pol.pdata %>%
  forestWrap(
    fit = single.fit$ipd[c("no2", "pm25", "pm10")],
    other_labs = c(subj_head, stud_head),
    x_limits = c(-3.0, 2.6), 
    axis_limits = c(0.2, 1.4), 
    digits = 2, 
    title = "Odds ratio for postnatal depression per IQR change in exposure", 
    scale = 1 ,
    y_tit = "Exposure")

text(subj_head, 5, "N subjects", cex = 1, font = 2)
text(stud_head, 5, "N studies", cex = 1, font = 2)

dev.off()

## ---- Road traffic noise -----------------------------------------------------
noise.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "lden_c_preg2", "lden_c_preg3", "lden_c_preg4", "lden_c_preg5")) %>%
  mutate(full_name = case_when(
    variable == "lden_c_preg2" ~ "55-55.9 dB", 
    variable == "lden_c_preg3" ~ "60-64.9 dB", 
    variable == "lden_c_preg4" ~ "65-69.9 dB", 
    variable == "lden_c_preg5" ~ ">70 dB"))

n_cat_lden <- descriptives$categorical %>%
  dplyr::filter(
    variable == "lden_c_preg" & cohort == "combined" & !is.na(category) & 
      category != 1) %>%
  pull(value)

png(
  file = here("figures", "single_noise_ga.png"), 
  width = word_full, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

forest(
  x = noise.pdata %>% pull(est), 
  ci.lb = noise.pdata %>% pull(lowci),
  ci.ub = noise.pdata %>% pull(uppci), 
  slab = noise.pdata %>% pull(full_name), 
  xlab = "Odds ratio for postnatal depression vs ref <55dB", 
  cex = 1, 
  cex.lab = 1,
  cex.axis = 1,
  ilab =  cbind(
    n_cat_lden,
    single.fit$ipd[c("lden", "lden", "lden", "lden")] %>% 
      map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(subj_head, stud_head),
  header = c("Noise category", "OR [95% CI]"), 
  refline = 1, 
  xlim = c(-3.0, 2.6),
  alim = c(0.2, 1.6), 
  steps = 7, 
  digits = c(2, 2))

text(subj_head, 6, "N subjects", cex = 1, font = 2)
text(stud_head, 6, "N studies", cex = 1, font = 2)

dev.off()

## ---- Natural spaces ---------------------------------------------------------
green.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "ndvi300_preg_iqr_p", "greenyn300_preg1", "blueyn300_preg1")) %>%
  mutate(full_name = c("NDVI", "Major green space", "Major blue space"))

png(
  file = here("figures", "single_nat_ga.png"), 
  width = word_full, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

green.pdata %>%
  forestWrap(
    fit = single.fit$ipd[c("ndvi", "green", "blue")],
    other_labs = c(subj_head, stud_head),
    x_limits = c(-3.0, 2.6), 
    axis_limits = c(0.2, 1.4), 
    digits = 2, 
    title = "Odds ratio for postnatal depression per IQR change in exposure", 
    scale = 1 ,
    y_tit = "Exposure")

text(subj_head, 5, "N subjects", cex = 1, font = 2)
text(stud_head, 5, "N studies", cex = 1, font = 2)

dev.off()


################################################################################
# Figure 8: Volcano plot  
################################################################################


tab2_cat.vars <- c(
  "ppd", "edu_m_0", "areases_tert_preg", "ethn3_m", "parity_bin", "sex",  
  "birth_month")

tab2_cont.vars <- c("agebirth_m_y", "birth_year")