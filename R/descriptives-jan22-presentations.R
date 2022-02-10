################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Produce descriptive statistics   
## Date: 28th May 21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(dsHelper)
library(ggplot2)
library(here)


conns <- datashield.login(logindata, restore = "env_pnd_16")

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")
################################################################################
# 1. Define variables  
################################################################################
green.vars <- c(
  "ndvi300_preg", "ndvi300_1", "greenyn300_preg", "blueyn300_preg", 
  "greenyn300_1", "blueyn300_1")

pol.vars <- c("no2_preg", "no2_1", "pm25_preg", "pm25_1", "pm10_preg", "pm10_1")

noise.vars <- c("lden_preg", "lden_1", "lden_c_preg", "lden_c_1")

lu.vars <- c(
  "frichness300_preg", "frichness300_1", "walkability_mean_preg", 
  "walkability_mean_1", "popdens_preg", "popdens_1")

out.vars <- "ppd"

cov.vars <- c(
  "edu_m_0", "areases_tert_preg", "ethn3_m", "agebirth_m_y", "parity_bin", 
  "sex", "birth_month", "birth_year", "prepreg_dep", "prepreg_psych")

all.vars <- c(green.vars, pol.vars, noise.vars, lu.vars, out.vars,cov.vars)

################################################################################
# 2. Descriptives 
################################################################################
desc_green <- dh.getStats(
  df = "analysis_df", 
  vars = green.vars
)

desc_pol <- dh.getStats(
  df = "analysis_df", 
  vars = pol.vars
)

desc_noise <- dh.getStats(
  df = "analysis_df", 
  vars = noise.vars
)

desc_lu <- dh.getStats(
  df = "analysis_df", 
  vars = lu.vars
)

desc_out <- dh.getStats(
  df = "analysis_df", 
  vars = out.vars
)

desc_cov <- dh.getStats(
  df = "analysis_df", 
  vars = cov.vars
)

descriptives <- list(
  desc_green, desc_pol, desc_noise, desc_lu, desc_out, desc_cov) %>%
  pmap(bind_rows)

save.image()

################################################################################
# 3. Number of observations per cohort
################################################################################
descriptives$categorical %>%
  dplyr::select(cohort, cohort_n) %>%
  distinct(cohort, cohort_n) %>%
  dplyr::filter(cohort != "combined")

################################################################################
# 4. Available exposures
################################################################################
preg.vars <- c(
  "no2_preg", "pm25_preg", "pm10_preg", "lden_c_preg", "ndvi300_preg", 
  "greenyn300_preg", "blueyn300_preg", "frichness300_preg", 
  "walkability_mean_preg", "popdens_preg")

avail_exp <- dh.anyData(
  df = "analysis_df", 
  vars = preg.vars
)

avail_exp.tab <- avail_exp %>%
  pivot_longer(
    cols = alspac:rhea,
    names_to = "cohort",
    values_to = "available") %>%
  pivot_wider(
    names_from = variable,
    values_from = available)

avail_exp.tab %>% dplyr::select(cohort, no2_preg:frichness300_preg)
avail_exp.tab %>% dplyr::select(cohort, walkability_mean_preg, popdens_preg)


################################################################################
# Descriptives prep
################################################################################
descriptives$continuous <- descriptives$continuous %>%
  mutate(cohort = factor(cohort), ord = TRUE)

## ---- Wrapper for continuous plots -------------------------------------------
contExpPlots <- function(var, title, col){
  
  descriptives$continuous %>%
    dplyr::filter(
      variable == var & !cohort == "combined") %>%
    left_join(., names_neat, by = "cohort") %>%
    ggplot(aes(x = fct_rev(cohort_neat), y = perc_50)) +
    geom_boxplot(
      aes(
        ymin = perc_5, 
        lower = perc_25, 
        middle = perc_50, 
        upper = perc_75, 
        ymax = perc_95), 
      stat = "identity", 
      width = 0.2,
      fill = col, 
      alpha = 0.3) +
    ylab(title) +
    coord_flip() +
    theme_std + 
    theme_word +
    theme(axis.title.y=element_blank())
  
}

## ---- Wrapper for categorical plots ------------------------------------------
catExpPlots <- function(var, title, col){

descriptives$categorical %>%
  filter(
    variable == var & !cohort == "combined" & category == 1) %>%
  mutate(
    category = 
      factor(
        case_when(
          category == 0 ~ "No", 
          category == 1 ~ "Yes", 
          category == "missing" ~ "Missing"), 
        levels = c("No", "Yes", "Missing"), 
        ordered = TRUE)) %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  left_join(., names_neat, by = "cohort") %>%
  ggplot(aes(x = fct_rev(cohort_neat), y = perc_total)) +
  geom_bar(
    position = "stack", 
    stat = "identity", 
    fill = col, 
    alpha = 0.5) +
  ylab(title) +
  coord_flip() +
  theme_std +  
  theme_word +
  theme(axis.title.y=element_blank())
}

## ---- KU palette -------------------------------------------------------------
col_1 <- "#D49F3A"
col_2 <- "#42759B"
col_3 <- "#A31D20"
col_4 <- "#779921"
col_5 <- "#79ADB1"
  
################################################################################
# Descriptives: Air polution
################################################################################

## ---- Plots ------------------------------------------------------------------
no2.plot <- contExpPlots(
  var = "no2_preg", 
  title = "N02 (ug / m3)", 
  col = col_1)

pm25.plot <- contExpPlots(
  var = "pm25_preg", 
  title = "PM2.5 (ug / m3)", 
  col = col_2)

pm10.plot <- contExpPlots(
  var = "pm10_preg", 
  title = "PM10 (ug / m3)", 
  col = col_3)

## ---- Save these -------------------------------------------------------------
ggsave(
  plot = no2.plot,
  filename = here("figures/jan22-pres", "no2.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = pm25.plot,
  filename = here("figures/jan22-pres", "pm25.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = pm10.plot,
  filename = here("figures/jan22-pres", "pm10.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

################################################################################
# Descriptives: Green spaces
################################################################################

## ---- Plots ------------------------------------------------------------------
ndvi.plot <- contExpPlots(
  var = "ndvi300_preg", 
  title = "NDVI (range = 0 - 1)", 
  col = col_4)

greenyn.plot <- catExpPlots(
  var = "greenyn300_preg", 
  title = "Proximity to major green space within 300 m", 
  col = col_4)

blueyn.plot <- catExpPlots(
  var = "blueyn300_preg", 
  title = "Proximity to major blue space within 300 m", 
  col = col_2)

## ---- Save files -------------------------------------------------------------
ggsave(
  plot = ndvi.plot,
  filename = here("figures/jan22-pres", "ndvi.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = greenyn.plot,
  filename = here("figures/jan22-pres", "greenyn.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = blueyn.plot,
  filename = here("figures/jan22-pres", "blueyn.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

################################################################################
# Descriptives: Road traffic noise
################################################################################

## ---- Plot -------------------------------------------------------------------
new_levels <- c("<55 dB", "55-55.9 dB", "60-64.9 dB", "65-69.9 dB", ">70 dB")

lden.pdata <- descriptives$categorical %>%
  filter(
    variable == "lden_c_preg" & !cohort == "combined" & !is.na(category)) %>%
  mutate(
    category = factor(        
      case_when(
          category == 1 ~ new_levels[1], 
          category == 2 ~ new_levels[2], 
          category == 3 ~ new_levels[3], 
          category == 4 ~ new_levels[4], 
          category == 5 ~ new_levels[5]), 
        levels = new_levels,
        ordered = TRUE)) %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  left_join(., names_neat, by = "cohort")

lden.plot <- lden.pdata %>%
  ggplot(aes(x = fct_rev(cohort_neat), y = perc_total, fill = fct_rev(category))) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.5) +
  ylab("Percentage") +
  coord_flip() +
  theme_std +  
  theme_word + 
  scale_fill_manual(values = c(col_3, col_1, col_4, col_5, col_2)) +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.title.y = element_blank()) +
  guides(fill = guide_legend(reverse=T))

## ---- Save this --------------------------------------------------------------
ggsave(
  plot = lden.plot,
  filename = here("figures/jan22-pres", "lden.png"),
  h = 12.98, w = 30.58, 
  units="cm", 
  dpi=1200, type="cairo")

################################################################################
# Descriptives: Built environment
################################################################################

## ---- Plots ------------------------------------------------------------------
pop_dens.plot <- contExpPlots(
  var = "popdens_preg", 
  title = "Population density (people / km2)", 
  col = col_1)

walk.plot <- contExpPlots(
  var = "walkability_mean_preg", 
  title = "Walkability index (range = 0 - 1)", 
  col = col_2)

facility.plot <- contExpPlots(
  var = "frichness300_preg", 
  title = "Facility richness (range = 0 - 1)", 
  col = col_3)

## ---- Save these -------------------------------------------------------------
ggsave(
  plot = pop_dens.plot,
  filename = here("figures/jan22-pres", "pop_dens.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = walk.plot,
  filename = here("figures/jan22-pres", "walk.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = facility.plot,
  filename = here("figures/jan22-pres", "facility.png"),
  h = 12.78, w = 10.01, 
  units="cm", 
  dpi=1200, type="cairo")

save.image()

################################################################################
# Descriptives: Postpartum depression
################################################################################

## ---- Plots ------------------------------------------------------------------
ppd.plot <- catExpPlots(
  var = "ppd", 
  title = "Percentage meeting threshold for post-partum depression",
  col = col_5)

## ---- Save plot --------------------------------------------------------------
ggsave(
  plot = ppd.plot,
  filename = here("figures/jan22-pres", "ppd.png"),
  h = 12.98, w = 30.58, 
  units="cm", 
  dpi=1200, type="cairo")

################################################################################
# Plot preparation
################################################################################
################################################################################
# Single exposures
################################################################################
tidyPdata <- function(fit, label, family = "binomial", exp = TRUE){
  
  coefs <- fit %>%
    map(
      dh.lmTab,
      type = "glm_ipd", 
      ci_format = "separate", 
      direction = "wide", 
      family = family,
      digits = 50, 
      exp = exp) %>%
    bind_rows(.id = "name") %>% 
    mutate(adjustment = label) %>%
    left_join(., exp_ref, by = "name")
  
  meta <- tibble(
    name = names(fit),
    n_sub = fit %>% map_int(function(x){x$Nvalid}),
    n_stud = fit %>% map_int(function(x){length(x$disclosure.risk)})
  )
  
  out <- left_join(coefs, meta, by = "name") 
}
  
## ---- Adjustment 1 -----------------------------------------------------------
single_m1.pdata <- tidyPdata(single_m1.fit$ipd, "model_1")

## ---- Adjustment 2 -----------------------------------------------------------
single_m2.pdata <- tidyPdata(single_m2.fit$ipd, "model_2")

## ---- Combine ----------------------------------------------------------------
single.pdata <- bind_rows(single_m1.pdata, single_m2.pdata) %>%
  mutate(full_name = ifelse(adjustment == "model_1", full_name, ""))

################################################################################
# Exploring confounding
################################################################################
edu.pdata <- tidyPdata(
  fit = edu.fit, 
  label = "confounding", 
  family = "gaussian",
  exp = FALSE)

area.pdata <- tidyPdata(
  fit = area.fit, 
  label = "confounding", 
  family = "gaussian",
  exp = FALSE)


################################################################################
# Joint exposures
################################################################################



################################################################################
# Wrapper function
################################################################################
forestWrap <- function(
  coefs, fit, title, x_limits, axis_limits, other_labs, digits = 3, scale = 0.7,
  y_tit = "Environmental exposure", x_tit = "OR [95% CI]",
  colour = rep("#000000"), length = coefs){
  
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
      coefs %>% pull(n_sub),
      coefs %>% pull(n_stud)),
    ilab.xpos = other_labs,
    header = c(y_tit, x_tit), 
    refline = 1, 
    xlim = x_limits,
    alim = axis_limits, 
    steps = 7, 
    digits = c(digits, 2),
    col = colour, 
    psize = 1)
}


################################################################################
# Plots: Single exposure
################################################################################
################################################################################
# Air pollution
################################################################################
pol.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "no2_preg_iqr_p", "pm25_preg_iqr_p", "pm10_preg_iqr_p")) %>%
  arrange(name)

png(
  file = here("figures/jan22-pres", "single_pol.png"), 
  width = 30.58*0.66, 
  height = 12.98, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

pol.pdata %>%
  forestWrap(
    fit = single.fit$ipd[c("no2", "pm25", "pm10")],
    other_labs = c(subj_head, stud_head),
    y_tit = "Pollutant",
    x_limits = c(-3.0, 2.6), 
    axis_limits = c(0.2, 1.4), 
    digits = 2, 
    title = "Odds ratio for postnatal depression per IQR change in exposure",
    colour = rep(c(col_1, col_2), 3))

text(subj_head, 8, "N subjects", cex = 0.7, font = 2)
text(stud_head, 8, "N studies", cex = 0.7, font = 2)

abline(h = 4.5, lwd = 0.2)
abline(h = 2.5, lwd = 0.2)

dev.off()

################################################################################
# Road traffic noise
################################################################################
noise.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "lden_c_preg2", "lden_c_preg3", "lden_c_preg4", "lden_c_preg5")) %>%
  mutate(cat_name = case_when(
    variable == "lden_c_preg2" ~ "55-55.9 dB", 
    variable == "lden_c_preg3" ~ "60-64.9 dB", 
    variable == "lden_c_preg4" ~ "65-69.9 dB", 
    variable == "lden_c_preg5" ~ ">70 dB")) %>%
  arrange(variable, adjustment) %>%
  mutate(cat_name = ifelse(adjustment == "model_1", cat_name, ""))
  
n_cat_lden <- descriptives$categorical %>%
  dplyr::filter(
    variable == "lden_c_preg" & cohort == "combined" & !is.na(category) & 
      category != 1) %>%
  pull(value)

png(
  file = here("figures/jan22-pres", "single_noise.png"), 
  width = 30.58*0.66, 
  height = 12.98, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

forest(
  x = noise.pdata %>% pull(est), 
  ci.lb = noise.pdata %>% pull(lowci),
  ci.ub = noise.pdata %>% pull(uppci), 
  slab = noise.pdata %>% pull(cat_name), 
  xlab = "Odds ratio for postnatal depression per IQR change in exposure", 
  cex = 0.7, 
  cex.lab = 0.7,
  cex.axis = 0.7,
  ilab =  cbind(
    noise.pdata %>% pull(n_sub),
    noise.pdata %>% pull(n_stud)),
  ilab.xpos = c(subj_head, stud_head),
  header = c("Noise level", "OR [95% CI]"), 
  refline = 1, 
  xlim = c(-3.0, 2.6),
  alim = c(0.2, 1.6), 
  steps = 7, 
  digits = c(2, 2), 
  col = rep(c(col_1, col_2), 4), 
  psize = 1)

text(subj_head, 10, "N subjects", cex = 0.7, font = 2)
text(stud_head, 10, "N studies", cex = 0.7, font = 2)

abline(h = 6.5, lwd = 0.2)
abline(h = 4.5, lwd = 0.2)
abline(h = 2.5, lwd = 0.2)

dev.off()

################################################################################
# Natural spaces
################################################################################
green.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "ndvi300_preg_iqr_p", "greenyn300_preg1", "blueyn300_preg1")) %>%
  arrange(desc(variable), adjustment)

png(
  file = here("figures/jan22-pres", "single_green.png"), 
  width = 30.58*0.66, 
  height = 12.98, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,2))

subj_head <- -1.2 
stud_head <- -0.4

green.pdata %>%
  forestWrap(
    fit = single.fit$ipd[c("ndvi", "green", "blue")],
    other_labs = c(subj_head, stud_head),
    y_tit = "Natural space exposure",
    x_limits = c(-3.0, 2.6), 
    axis_limits = c(0.2, 1.4), 
    digits = 2, 
    title = "Odds ratio for postnatal depression per IQR change in exposure", 
    colour = rep(c(col_1, col_2), 3))

text(subj_head, 8, "N subjects", cex = 0.7, font = 2)
text(stud_head, 8, "N studies", cex = 0.7, font = 2)

abline(h = 2.5, lwd = 0.2)
abline(h = 4.5, lwd = 0.2)

dev.off()

################################################################################
# Built environment
################################################################################
built.pdata <- single.pdata %>%
  dplyr::filter(variable %in% c(
    "frichness300_preg_iqr_p", "walkability_mean_preg_iqr_p", 
    "popdens_preg_iqr_p")) %>%
  arrange(variable, adjustment)

png(
  file = here("figures/jan22-pres", "single_built.png"), 
  width = 30.58*0.66, 
  height = 12.98, 
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
    y_tit = "Built Environment",
    digits = 2, 
    title = "Odds ratio for postnatal depression per IQR change in exposure", 
    colour = rep(c(col_1, col_2), 3))

text(subj_head, 8, "N subjects", cex = 0.7, font = 2)
text(stud_head, 8, "N studies", cex = 0.7, font = 2)

abline(h = 2.5, lwd = 0.2)
abline(h = 4.5, lwd = 0.2)

dev.off()

################################################################################
# Explore confounding
################################################################################
cont_exp_short <- c("no2", "pm25", "pm10", "ndvi", "fac_rich", "pop_dens", "walk")

## ---- Maternal education -----------------------------------------------------
  edu.pdata <- edu.pdata %>% 
  dplyr::filter(variable %in% c("edu_m_02", "edu_m_03")) %>%
  arrange(name) %>%
  mutate(name = factor(
    name,
    levels = cont_exp_short,
    ordered = TRUE
  )) %>%
  arrange(name)

  png(
    file = here("figures/jan22-pres", "ed_exp.png"), 
    width = 30.58*0.66, 
    height = 12.98, 
    units = "cm",
    res = 300)
  
  par(mar=c(5,0,0,2))
  
  subj_head <- -1.2 
  stud_head <- -0.4
  
  forest(
    x = edu.pdata %>% pull(est), 
    ci.lb = edu.pdata %>% pull(lowci),
    ci.ub = edu.pdata %>% pull(uppci), 
    slab = edu.pdata %>% pull(full_name), 
    xlab = "Association between maternal education and exposure (ref = high eduction)",
    cex = 0.7, 
    cex.lab = 0.7,
    cex.axis = 0.7,
    header = c("Environmental exposure", "OR [95% CI]"), 
    refline = 0, 
    xlim = c(-0.4, 0.4),
    alim = c(-0.2, 0.2), 
    steps = 5, 
    digits = c(2, 2), 
    psize = 1, 
    col = rep(c(col_1, col_2), 7))

  abline(h = 2.5, lwd = 0.2)
  abline(h = 4.5, lwd = 0.2)
  
  dev.off()  

## ---- Area deprivation -------------------------------------------------------
area.pdata <- area.pdata %>% 
    dplyr::filter(variable %in% c("areases_tert2", "areases_tert3")) %>%
    mutate(name = factor(
      name,
      levels = cont_exp_short,
      ordered = TRUE
    )) %>%
    mutate(full_name = if_else(variable == "areases_tert2", full_name, "")) %>%
    arrange(name)
  
  png(
    file = here("figures/jan22-pres", "area_exp.png"), 
    width = 30.58*0.66, 
    height = 12.98, 
    units = "cm",
    res = 300)
  
  par(mar=c(5,0,0,2))
  
  subj_head <- -1.2 
  stud_head <- -0.4
  
  forest(
    x = area.pdata %>% pull(est), 
    ci.lb = area.pdata %>% pull(lowci),
    ci.ub = area.pdata %>% pull(uppci), 
    slab = area.pdata %>% pull(full_name), 
    xlab = "Association between area deprivation and exposure (ref = low deprivation)",
    cex = 0.7, 
    cex.lab = 0.7,
    cex.axis = 0.7,
    header = c("Environmental exposure", "OR [95% CI]"), 
    refline = 0, 
    xlim = c(-0.6, 0.6),
    alim = c(-0.4, 0.4), 
    steps = 5, 
    digits = c(2, 2), 
    psize = 1, 
    col = rep(c(col_1, col_2), 7))
  
  abline(h = 2.5, lwd = 0.2)
  abline(h = 4.5, lwd = 0.2)
  abline(h = 6.5, lwd = 0.2)
  abline(h = 8.5, lwd = 0.2)
  abline(h = 10.5, lwd = 0.2)
  abline(h = 12.5, lwd = 0.2)
  
  dev.off()  

  ################################################################################
  # Joint exposures
  ################################################################################






################################################################################
# 3. Descriptives for full sample   
################################################################################
descriptives_full <- dh.getStats(
  df = "env_pnd", 
  vars = all.vars
)






descriptives$continuous %>%
  dplyr::filter(variable == "lden_preg" & !is.na(mean)) %>%
  select(cohort, variable, perc_50, perc_5, perc_95)



################################################################################
# 3. Write descriptives  
################################################################################

# We do it like this because we can't make markdown files in the analysis 
# server so instead we do it locally.
save(exposures.desc, file = here("data", "exp_desc.RData"))
save(outcome.desc, file = here("data", "out_desc.RData"))

################################################################################
# 4. Within-time point correlations  
################################################################################

## ---- Create two subsets with only the required variables --------------------

## Pregnancy
dh.dropCols(
  df = "analysis_df", 
  vars = c(
    "no2_preg", "pm25_preg", "lden_preg", "ndvi300_preg", "green_dist_preg", 
    "blue_dist_preg", "bdens300_preg", "fdensity300_preg", "frichness300_preg", 
    "landuseshan300_preg", "walkability_mean_preg", "agrgr_preg", "natgr_preg", 
    "urbgr_preg"),
  comp_var = "child_id",
  type = "keep",
  new_df_name = "heat_preg")

## ---- Correlation matrices ---------------------------------------------------
exp_cor_preg <- ds.cor(
  x = "heat_preg", 
  type = "split"
)

save(exp_cor_preg, file = here("data", "exp_cor_preg.RData"))

################################################################################
# 5. Between time point correlations
################################################################################
cor_ref <- tibble(
  var_1 = c(
    "lden_preg", "ndvi300_preg", "green_dist_preg", 
    "blue_dist_preg", "bdens300_preg", "fdensity300_preg", "frichness300_preg", 
    "landuseshan300_preg", "walkability_mean_preg", "agrgr_preg", "natgr_preg", 
    "urbgr_preg"
  ), 
  var_2 = c(
    "lden_1", "ndvi300_1", "green_dist_1", 
    "blue_dist_1", "bdens300_1", "fdensity300_1", "frichness300_1", 
    "landuseshan300_1", "walkability_mean_1", "agrgr_1", "natgr_1", 
    "urbgr_1"
  )
)

btp_cor <- cor_ref %>%
  pmap(function(var_1, var_2){
    
    ds.cor(
      x = paste0("analysis_df$", var_1),
      y = paste0("analysis_df$", var_2),
      type = "split"
    )
  })

save(btp_cor, file = here("data", "btp_cor_preg.RData"))

################################################################################
# 6. Box plots using Demetris' function  
################################################################################

## ---- Polution ---------------------------------------------------------------
violin_pol.data <- dh.getAnonPlotData(
  df = "analysis_df", 
  vars = c("no2_preg", "pm25_preg"))

## ---- Natural spaces ---------------------------------------------------------
violin_nat.data <- dh.getAnonPlotData(
  df = "analysis_df", 
  vars = c("ndvi300_preg", "green_dist_preg", "blue_dist_preg"))

## ---- Built data -----------------------------------------
violin_built.data <- dh.getAnonPlotData(
  df = "analysis_df", 
  vars = c(
    "bdens300_preg", "bdens300_1", "fdensity300_preg", "fdensity300_preg",
    "fdensity300_1", "frichness300_preg", "frichness300_1", "landuseshan300_preg",
    "landuseshan300_1", "walkability_mean_preg", "walkability_mean_1", 
    "agrgr_preg", "agrgr_1", "urbgr_preg", "urbgr_1"))

## ---- LU natural green -------------------------------------------------------
violin_lu.data <- dh.getAnonPlotData(
  df = "analysis_df", 
  vars = c("natgr_preg", "natgr_1"),
  conns = conns[!names(conns) == "moba"])

## ---- Lden -------------------------------------------------------------------
violin_noise.data <- dh.getAnonPlotData(
  df = "analysis_df", 
  vars = "lden_preg", 
  conns = conns[!names(conns) %in% c("alspac", "dnbc", "inma_gip", "inma_val")])

## ---- Combine ----------------------------------------------------------------
violin_out <- c(
  violin_pol.data, violin_nat.data, violin_built.data, violin_noise.data, 
  violin_lu.data)

save(violin_out, file = here("data", "violin_out.RData"))


################################################################################
# 7. Exposure - outcome associations: model formulae  
################################################################################
cohorts <- names(conns)

ppd_miss <- c("genr", "inma", "dnbc")

## ---- Natural spaces ---------------------------------------------------------
nat.mod <- list(
  ndvi = list(
    outcome = "ppd",
    exposure = "ndvi300_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]), 
  green_dist = list(
    outcome = "ppd",
    exposure = "green_dist_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]), 
  blue_dist = list(
    outcome = "ppd",
    exposure = "blue_dist_preg", 
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]))

## ---- Polution ---------------------------------------------------------------
pol.mod <- list(
  no2 = list(
    outcome = "ppd",
    exposure = "no2_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]), 
  pm25 = list(
    outcome = "ppd",
    exposure = "pm25_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]))

## ---- Grey space -------------------------------------------------------------
grey.mod <- list(
  bdens = list(
    outcome = "ppd",
    exposure = "bdens300_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]), 
  fdens = list(
    outcome = "ppd",
    exposure = "fdensity300_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]),
  frich = list(
    outcome = "ppd",
    exposure = "frichness300_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]),
  landuse = list(
    outcome = "ppd",
    exposure = "landuseshan300_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]),
  walk = list(
    outcome = "ppd",
    exposure = "walkability_mean_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]),
  lu_agr = list(
    outcome = "ppd",
    exposure = "agrgr_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]),
  lu_forst = list(
    outcome = "ppd",
    exposure = "natgr_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% c(ppd_miss, "moba") == FALSE]),
  lu_urb_green = list(
    outcome = "ppd",
    exposure = "urbgr_preg",
    covariates = "",
    cohorts = cohorts[cohorts %in% ppd_miss == FALSE]))


dh.glmWrap <- function(x, type, dummy_suff = "_dummy", data = "analysis_df"){
  
  if(type == "ipd"){
    
    out <- ds.glm(
      formula = x$model,
      data = "analysis_df", 
      family = "binomial", 
      datasources = conns[x$cohorts])
    
  }
  
  
  else if(type == "slma"){
    
    out <- ds.glmSLMA(
      formula = x$model,
      dataName = "analysis_df", 
      family = "binomial",
      datasources = conns[x$cohorts])
  }
  
  return(out)
}


################################################################################
# 8. Run models  
################################################################################

## ---- Natural spaces ---------------------------------------------------------
nat.fit <- nat.mod %>%
  map(dh.makeGlmForm, type = "slma") %>% 
  map(dh.glmWrap, type = "slma")


## ---- Polution ---------------------------------------------------------------
pol.fit <- pol.mod %>%
  map(dh.makeGlmForm, type = "slma") %>% 
  map(dh.glmWrap, type = "slma")


## ---- Grey space -------------------------------------------------------------
grey.fit <- grey.mod %>%
  map(dh.makeGlmForm, type = "slma") %>% 
  map(dh.glmWrap, type = "slma")


################################################################################
# 9. Get coefficients for plots   
################################################################################

## ---- Natural spaces ---------------------------------------------------------
nat.out <- list(fit = nat.fit, model = nat.mod) %>%
  pmap(function(fit, model){
    
    dh.lmTab(
      model = fit,
      type = "slma", 
      coh_names = model$cohorts, 
      ci_format = "separate", 
      direction = "wide", 
      round_digits = 10) 
    
  }) %>% bind_rows(.id = "exposure")

## ---- Polution ---------------------------------------------------------------
pol.out <- list(fit = pol.fit, model = pol.mod) %>%
  pmap(function(fit, model){
    
    dh.lmTab(
      model = fit,
      type = "slma", 
      coh_names = model$cohorts, 
      ci_format = "separate", 
      direction = "wide", 
      round_digits = 10)  
    
  }) %>% bind_rows(.id = "exposure")

## ---- Grey spaces ------------------------------------------------------------
grey.out <- list(fit = grey.fit, model = grey.mod) %>%
  pmap(function(fit, model){
    
    dh.lmTab(
      model = fit,
      type = "slma", 
      coh_names = model$cohorts, 
      ci_format = "separate", 
      direction = "wide", 
      round_digits = 10) 
    
  }) %>% bind_rows(.id = "exposure")

## ---- Combine and output -----------------------------------------------------
single_reg.out <- bind_rows(nat.out, pol.out, grey.out)
save(single_reg.out, file = here("data", "single_reg.RData"))

save.image()

################################################################################
# 10. Create subsets for stratified odds ratios  
################################################################################

## ---- NDVI -------------------------------------------------------------------
dh.renameVars(
  df = "analysis_df", 
  names = tibble(oldvar = "ndvi300_preg", newvar = "ndvi_p")
)

ndvi_ninfea <- tibble(
  cohort = "ninfea",
  low_val = seq(0.1, 0.4, 0.1), 
  high_val = seq(0.2, 0.5, 0.1))

ndvi_moba <- tibble(
  cohort = "moba",
  low_val = seq(0.3, 0.6, 0.1), 
  high_val = seq(0.4, 0.7, 0.1))

ndvi_alspac <- tibble(
  cohort = "alspac",
  low_val = seq(0.2, 0.5, 0.1), 
  high_val = seq(0.3, 0.6, 0.1))

ndvi_genr <- tibble(
  cohort = "genr",
  low_val = seq(0.3, 0.5, 0.1), 
  high_val = seq(0.4, 0.6, 0.1))

ndvi_ref <- bind_rows(ndvi_ninfea, ndvi_moba, ndvi_alspac, ndvi_genr) %>%
  mutate(
    df = "analysis_df", 
    subset_var = "ndvi_p", 
    new_df_name = paste0("ndvi_p_", high_val))

ndvi_ref %>% 
  pmap(function(cohort, low_val, high_val, df, subset_var, new_df_name){
    
    dh.subsetBetween(
      df = df,
      subset_var = subset_var,
      low_val = low_val,
      high_val = high_val,
      new_df_name = new_df_name)
  })

ndvi_ref %>% 
  pmap(function(cohort, new_df_name, ...){
    
    ds.dim(
      x = new_df_name, 
      datasources = conns[cohort]
    )
  })

datashield.workspace_save(conns, "env_pnd_9a")
conns  <- datashield.login(logindata, restore = "env_pnd_9a")

################################################################################
# 11. Stratified odds ratios  
################################################################################
ndvi_strat <- ndvi_ref %>%
  pmap(function(new_df_name, cohort, ...){
    ds.glmSLMA(
      formula = "ppd ~ ndvi300_preg", 
      family = "binomial", 
      dataName  = new_df_name, 
      datasources = conns[cohort])
  })

ndvi_out <- list(models = ndvi_strat, cohort =  ndvi_ref %>% pull(cohort)) %>%
  pmap(function(models, cohort){
    dh.lmTab(
      model = models, 
      type = "slma", 
      coh_names = cohort,
      direction = "wide", 
      ci_format = "separate")
  }) %>% set_names(ndvi_ref %>% pull(new_df_name)) %>%
  bind_rows(.id = "range") %>% 
  mutate(across(est:uppci, ~exp(.x)))

save(ndvi_out, file = here("data", "ndvi_strat.RData"))








