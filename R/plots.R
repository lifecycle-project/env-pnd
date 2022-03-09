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
library(GGally)  

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

word_land_half <- 29.7 /2

source("~/env-pnd/R/var-reference.R")
################################################################################
# Figure S3: Missingness forest plots  
################################################################################
################################################################################
# Prepare data
################################################################################
miss_converged.fit <- single_miss.fit %>%
  dplyr::filter(converged == TRUE)

miss.mdata <- dh.metaSepModels(
  fit = miss_converged.fit$fit,
  cohort = miss_converged.fit$cohort,
  model_name = miss_converged.fit$model,
  exp = TRUE, 
  method = "REML"
)

miss.pdata <- miss.mdata %>%
  dplyr::rename(variable = model) %>%
  left_join(., exp_preg.ref, by = "variable") %>%
  mutate(
    variable = factor(
      variable, levels = exp_preg.vars, ordered = TRUE)) %>%
  arrange(variable)

################################################################################
# Plot missingness models
################################################################################
png(
  file = here("figures", "miss_forest.png"), 
  width = word_land_half, 
  height = 10, 
  units = "cm",
  res = 300)

par(mar=c(5,0,1,2))

forest(
  x = miss.pdata %>% pull(coef), 
  ci.lb = miss.pdata %>% pull(low_ci),
  ci.ub = miss.pdata %>% pull(upper_ci), 
  slab = miss.pdata %>% pull(full_name),
  xlab = "Associations between postnatal depression and probability of being complete case", 
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Exposure", "Odds ratio [95% CI]"), 
  refline = 1, 
  xlim = c(-3.5, 6.5),
  alim = c(0, 4), 
  steps = 5, 
  digits = c(3, 2),
  psize = 1)

dev.off()

################################################################################
# EXPOSURE DISTRIBUTIONS
################################################################################
################################################################################
# Prepare data
################################################################################
exp_cont.pdata <- descriptives$continuous %>%
  dplyr::filter(!cohort == "combined" & variable %in% exp_preg.vars) %>%
  left_join(., full.ref, by = "variable") %>%
  left_join(., names_neat, by = "cohort") %>%
  mutate(cohort = factor(cohort, levels = names(conns), ordered = TRUE))

exp_cat.pdata <- descriptives$categorical %>%
  dplyr::filter(!cohort == "combined" & variable %in% exp_preg.vars) %>%
  left_join(., full.ref, by = "variable") %>%
  left_join(., full_values.ref, by = c("variable", "category")) %>%
  left_join(., names_neat, by = "cohort") %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  dplyr::filter(!is.na(category))


################################################################################
# Wrapper plot functions
################################################################################

## ---- Continuous variables ---------------------------------------------------
contExpPlots <- function(var, title, col){
  
  exp_cont.pdata %>%
    dplyr::filter(variable == var) %>%
    ggplot(aes(x = cohort_neat, y = perc_50, fill = full_name)) +
    geom_boxplot(
      aes(
        ymin = perc_5, 
        lower = perc_25, 
        middle = perc_50, 
        upper = perc_75, 
        ymax = perc_95), 
      stat = "identity", 
      width = 0.2,
      alpha = 0.6,
      lwd = 0.3, 
      fatten = 1,
      fill = col) +
    ylab(title) +
    theme_std + 
    theme_word +
    theme_bar_stack +
    theme_tight +
    theme(axis.title.x = element_blank())
  
}

## ---- Categorical variables --------------------------------------------------
catExpPlots <- function(var, title, col){
  
  exp_cat.pdata %>%
    dplyr::filter(variable == var & category == 1) %>%
    ggplot(aes(x = cohort_neat, y = perc_valid)) +
    geom_bar(
      position = "stack", 
      stat = "identity", 
      fill = col, 
      alpha = 0.6) +
    ylab(title) +
    theme_std +  
    theme_word +
    theme_bar_stack +
    theme_tight
  
}

################################################################################
# Figure 2: Ambient air polution
################################################################################

## ---- NO2 --------------------------------------------------------------------
no2.plot <- contExpPlots(
  var = "no2_preg",
  title = "NO2 (ug / m3)",
  col = palette_std[1])

ggsave(
  plot = no2.plot,
  filename = here("figures", "no2.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cont.pdata %>% 
  dplyr::filter(variable == "no2_preg") %>%
  dplyr::select(cohort, mean) %>%
  arrange(mean)

## ---- PM2.5 ------------------------------------------------------------------
pm25.plot <- contExpPlots(
  var = "pm25_preg",
  title = "PM2.5 (ug / m3)",
  col = palette_std[2])

ggsave(
  plot = pm25.plot,
  filename = here("figures", "pm25.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cont.pdata %>% 
  dplyr::filter(variable == "pm25_preg") %>%
  dplyr::select(cohort, mean) %>%
  arrange(mean)

## ---- PM10 -------------------------------------------------------------------
pm10.plot <- contExpPlots(
  var = "pm25_preg",
  title = "PM10 (ug / m3)",
  col = palette_std[3])

ggsave(
  plot = pm10.plot,
  filename = here("figures", "pm10.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cont.pdata %>% 
  dplyr::filter(variable == "pm10_preg") %>%
  dplyr::select(cohort, mean) %>%
  arrange(mean)

################################################################################
# Figure 3: Road traffic noise
################################################################################
noise_labs <- full_values.ref %>%
  dplyr::filter(variable == "lden_preg_f") %>%
  pull(cat_label)

lden.pdata <- exp_cat.pdata %>% 
  dplyr::filter(variable == "lden_preg_f") %>%
  mutate(cat_label = factor(cat_label, levels = noise_labs, ordered = TRUE))

lden.plot <- lden.pdata %>%
  ggplot(aes(x = fct_rev(cohort_neat), y = perc_valid, fill = fct_rev(cat_label))) +
  geom_bar(position = "stack", stat = "identity", alpha = 0.5) +
  ylab("Percentage") +
  coord_flip() +
  theme_std +  
  theme_word + 
  scale_fill_manual(values = c(palette_std[5:1])) +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.title.y = element_blank()) +
  guides(fill = guide_legend(reverse=T))

## ---- Save this --------------------------------------------------------------
ggsave(
  plot = lden.plot,
  filename = here("figures", "lden.png"),
  h = 8, w = word_full, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cat.pdata %>% 
  dplyr::filter(variable == "lden_preg_f") %>%
  dplyr::select(cohort, category, perc_valid) %>%
  arrange(category, perc_valid) %>%
  print(n = Inf)

################################################################################
# Figure 4: Natural spaces
################################################################################

## ---- NDVI -------------------------------------------------------------------
ndvi.plot <- contExpPlots(
  var = "ndvi300_preg",
  title = "Range 0 - 1",
  col = palette_ext[4])

ggsave(
  plot = ndvi.plot,
  filename = here("figures", "ndvi.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cont.pdata %>% 
  dplyr::filter(variable == "ndvi300_preg") %>%
  dplyr::select(cohort, mean) %>%
  arrange(mean)


## ---- Major green space ------------------------------------------------------
green.plot <- catExpPlots(
  var = "greenyn300_preg",
  title = "Percentage",
  col = palette_ext[5])

ggsave(
  plot = green.plot,
  filename = here("figures", "green.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cat.pdata %>% 
  dplyr::filter(variable == "greenyn300_preg" & category == 1) %>%
  dplyr::select(cohort, category, perc_valid) %>%
  arrange(category, perc_valid) %>%
  print(n = Inf)


## ---- Major blue space -------------------------------------------------------
blue.plot <- catExpPlots(
  var = "blueyn300_preg",
  title = "Percentage",
  col = palette_ext[6])

ggsave(
  plot = blue.plot,
  filename = here("figures", "blue.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cat.pdata %>% 
  dplyr::filter(variable == "blueyn300_preg" & category == 1) %>%
  dplyr::select(cohort, category, perc_valid) %>%
  arrange(category, perc_valid) %>%
  print(n = Inf)


################################################################################
# Figure 5: Built environment
################################################################################

## ---- Facility richness ------------------------------------------------------
fac.plot <- contExpPlots(
  var = "frichness300_preg",
  title = "Range 0 - 1",
  col = palette_ext[8])

ggsave(
  plot = fac.plot,
  filename = here("figures", "fac.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cont.pdata %>% 
  dplyr::filter(variable == "frichness300_preg") %>%
  dplyr::select(cohort, mean) %>%
  arrange(mean)

## ---- Walkability ------------------------------------------------------------
walk.plot <- contExpPlots(
  var = "walkability_mean_preg",
  title = "Range 0 - 1",
  col = palette_ext[9])

ggsave(
  plot = walk.plot,
  filename = here("figures", "walk.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cont.pdata %>% 
  dplyr::filter(variable == "walkability_mean_preg") %>%
  dplyr::select(cohort, mean) %>%
  arrange(mean)

## ---- Population density -----------------------------------------------------
pop.plot <- contExpPlots(
  var = "popdens_preg",
  title = "Inhabitants / km2",
  col = palette_ext[10])

ggsave(
  plot = pop.plot,
  filename = here("figures", "pop.png"),
  h = 6, w = 5.8, 
  units="cm", 
  dpi=1200, type="cairo")

## Min and max values for text
exp_cont.pdata %>% 
  dplyr::filter(variable == "popdens_preg") %>%
  dplyr::select(cohort, mean) %>%
  arrange(mean)


################################################################################
# Figure S4: Correlations between pregnancy exposures
################################################################################

exp_names_short <- c(exp_preg.ref$full_name[1:5], "Green_space_300m", 
                     "Blue_space_300m", exp_preg.ref$full_name[8:10])

## ---- Prepare data -----------------------------------------------------------
cor_preg.plotdata <- exp_cor_preg %>%
  map(function(x){
    
    x[["Correlation Matrix"]] %>%
      as_tibble %>%
      set_names(exp_names_short)
  }) %>%
  set_names(names(conns))

## ---- Make the plots --------------------------------------------------------
cor.plots <- cor_preg.plotdata %>%
  map(
    ~ggcorr(
      data = NULL, 
      cor_matrix = ., 
      label = TRUE, 
      label_round = 2, 
      hjust = 1, 
      layout.exp = 1, 
      label_size = 3, 
      low = palette_ext[2], 
      mid = "white", 
      high = palette_std[5], 
      legend.size = 10) +
      theme_cor + 
      theme(legend.position = "none"))

## ---- Save the plots ---------------------------------------------------------
cor.plots %>%
  imap(
    ~ggsave(
      plot = .x,
      filename = here("figures", paste0("cor_", .y, ".png")),
      h = 8, w = word_land_half, 
      units="cm", 
      dpi=1200, type="cairo")
  )


cor.plots$ninfea

################################################################################
# SINGLE EXPOSURE MODELS  
################################################################################
################################################################################
# Prep function
################################################################################
prepMainPlot <- function(mdata, variable, ref, levels = ref$full_name){
  
  out <- mdata %>%
    dplyr::filter(variable %in% ref$variable) %>%
    left_join(., ref, by = "variable") %>%
    left_join(., names_neat, by = "cohort") %>%
    mutate(full_name = factor(
      full_name, 
      levels = levels,
      ordered = TRUE)) %>%
    arrange(full_name)
  
}

################################################################################
# Wrapper functions
################################################################################
forestWrap <- function(
  coefs = NULL, cex = 0.6, cex.lab = 0.8, cex.axis = 0.8, 
  ilab.xpos = c(-1.5, -0.5), header = c("Cohort", "OR [95% CI]"), refline = 1, 
  xlim = c(-3, 6), alim = c(0, 4), steps = 5, digits = c(2, 2), rows = NULL, 
  col = NULL, ylim = NULL, psize = 1, 
  ilab = cbind(
    coefs %>% pull(valid_n), 
    coefs %>% pull(weight_scaled) %>% round(2)), 
  col_1 = "cohort_neat", showweights = FALSE){
  
  forest(
    x = coefs %>% pull(est), 
    ci.lb = coefs %>% pull(lowci),
    ci.ub = coefs %>% pull(uppci), 
    slab = coefs %>% pull(!!sym(col_1)), 
    xlab = "", 
    cex = cex, 
    cex.lab = cex.lab,
    cex.axis = cex.axis,
    ilab = ilab,
    ilab.xpos = ilab.xpos,
    header = header, 
    refline = refline, 
    ylim = ylim,
    xlim = xlim,
    alim = alim, 
    steps = steps, 
    digits = c(digits, 2), 
    rows = rows, 
    col = col, 
    psize = psize, 
    showweights = showweights)
}

polyWrap <- function(meta, row, cex = 0.6){
  
  addpoly(
    meta, 
    row = row, 
    mlab = list(bquote(
      paste("RE Model (", I^2, " = ",
            .(round(meta$I2, 1)),
            ")"))),
    transf = transf.exp.int, 
    cex = cex)
  
}

################################################################################
# Prepare data
################################################################################
model_1.pdata <- prepMainPlot(
  mdata = model_1.mdata, 
  ref = exp_preg_coef.ref)

################################################################################
# Figure 6: Combined plot
################################################################################
fig_6.pdata <- model_1.pdata %>% 
  dplyr::filter(cohort == "combined") 

nsub.pos <- -1.5
nstud.pos <- -1
i2.pos <- -0.5

png(
  file = here("figures", "fig_6.png"), 
  width = word_full, 
  height = 10, 
  units = "cm",
  res = 1200)

par(mar=c(5,0,1,2))

forest(
  x = fig_6.pdata %>% pull(est), 
  ci.lb = fig_6.pdata %>% pull(lowci),
  ci.ub = fig_6.pdata %>% pull(uppci), 
  slab = fig_6.pdata %>% pull(full_name), 
  xlab = "", 
  cex = 0.6, 
  cex.lab = 0.6,
  cex.axis = 0.6,
  ilab =  cbind(
    fig_6.pdata %>% pull(n_studies), 
    fig_6.pdata %>% pull(valid_n),
    fig_6.pdata %>% pull(i2)),
  ilab.xpos = c(nsub.pos, nstud.pos, i2.pos),
  header = c("Exposure", "OR [95% CI]"), 
  refline = 1, 
  xlim = c(-3.5, 3.5),
  alim = c(0, 2),  
  steps = 5, 
  digits = c(2, 2), 
  psize = 1, 
  rows = rev(c(3, 4, 5, 8, 9, 10, 13, 14, 15, 16, 19, 20, 21)), 
  ylim = c(3, 26))

text(nsub.pos, 25, "N", cex = 0.7, font = 2)
text(nstud.pos, 25, "K", cex = 0.7, font = 2)
text(i2.pos, 25, "I2", cex = 0.7, font = 2)

text(-3.02, 22.3, "Air pollutants", cex = 0.7, font = 2)
text(-2.90, 17.3, "Road traffic noise", cex = 0.7, font = 2)
text(-2.98, 11.3, "Natural spaces", cex = 0.7, font = 2)
text(-2.90, 6.3, "Built environment", cex = 0.7, font = 2)

dev.off()


################################################################################
# FOREST PLOTS
################################################################################
################################################################################
# Figure S5: Ambient air pollution   
################################################################################
air.pdata <- model_1.pdata %>%
  dplyr::filter(variable %in% c(
    "no2_preg_iqr_c", "pm25_preg_iqr_c", "pm10_preg_iqr_c"))

air_coh.pdata <- air.pdata %>% dplyr::filter(cohort != "combined")
air_meta.pdata <- air.pdata %>% dplyr::filter(cohort == "combined")

png(
  file = here("figures", "pollution_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forestWrap(
  coefs = air_coh.pdata, 
  rows = rev(c(2:8, 12:22, 26:36)),
  ylim = c(0, 40))

text(-1.5, 39, "N subjects", cex = 0.6, font = 2)
text(-0.5, 39, "Study weight %", cex = 0.6, font = 2)

text(-2.74, 37.3, "NO2", cex = 0.6, font = 2)
text(-2.69, 23.3, "PM2.5", cex = 0.6, font = 2)
text(-2.70, 9.3, "PM10", cex = 0.6, font = 2)

polyWrap(meta = air_meta.pdata$metafor_obj[[1]], row = 24.8)
polyWrap(air_meta.pdata$metafor_obj[[2]], row = 10.8)
polyWrap(air_meta.pdata$metafor_obj[[3]], row = 0.8)

dev.off()

################################################################################
# Figure S6: road traffic noise   
################################################################################
noise.pdata <- model_1.pdata %>%
  dplyr::filter(variable %in% c(
    "lden_c_preg2", "lden_c_preg3", "lden_c_preg4", "lden_c_preg5"))

noise_coh.pdata <- noise.pdata %>% dplyr::filter(cohort != "combined")
noise_meta.pdata <- noise.pdata %>% dplyr::filter(cohort == "combined")

png(
  file = here("figures", "noise_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = noise_coh.pdata  %>% pull(est), 
  ci.lb = noise_coh.pdata %>% pull(lowci),
  ci.ub = noise_coh.pdata %>% pull(uppci), 
  slab = noise_coh.pdata %>% pull(cohort_neat), 
  xlab = "", 
  ilab = cbind(
    noise_coh.pdata %>% pull(valid_n), 
    noise_coh.pdata %>% pull(weight_scaled) %>% round(2)),
  ilab.xpos = c(-1.5, -0.5),
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Exposure", "OR [95% CI]"),  
  refline = 1, 
  xlim = c(-3, 6),
  ylim = c(0, 49),
  alim = c(0, 4), 
  steps = 5, 
  digits = c(2, 2), 
  rows = c(45:37, 33:25, 21:13, 8:2))

text(-1.5, 48, "N subjects", cex = 0.6, font = 2)
text(-0.5, 48, "Study weight %", cex = 0.6, font = 2)

text(-2.54, 46.3, "55-55.9 dB", cex = 0.6, font = 2)
text(-2.54, 34.3, "60-64.9 dB", cex = 0.6, font = 2)
text(-2.54, 22.3, "65-69.9 dB", cex = 0.6, font = 2)
text(-2.65, 9.3, ">70 dB", cex = 0.6, font = 2)

polyWrap(noise_meta.pdata$metafor_obj[[1]], row = 35.8)
polyWrap(noise_meta.pdata$metafor_obj[[2]], row = 23.8)
polyWrap(noise_meta.pdata$metafor_obj[[3]], row = 11.8)
polyWrap(noise_meta.pdata$metafor_obj[[4]], row = 0.8)

dev.off()

################################################################################
# Figure S7: Natural spaces  
################################################################################
natural.pdata <- model_1.pdata %>%
  dplyr::filter(variable %in% c(
    "ndvi300_preg_iqr_c", "greenyn300_preg1", "blueyn300_preg1"))

natural_coh.pdata <- natural.pdata %>% dplyr::filter(cohort != "combined")
natural_meta.pdata <- natural.pdata %>% dplyr::filter(cohort == "combined") 

png(
  file = here("figures", "natural_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forestWrap(
  coefs = natural_coh.pdata, 
  rows = rev(c(2:11, 16:26, 30:40)), 
  ylim = c(0, 44))

text(-1.5, 43, "N subjects", cex = 0.6, font = 2)
text(-0.5, 43, "Study weight %", cex = 0.6, font = 2)

text(-2.71, 41.3, "NDVI", cex = 0.6, font = 2)
text(-1.94, 27.3, "Major green space within 300m", cex = 0.6, font = 2)
text(-1.98, 12.3, "Major blue space within 300m", cex = 0.6, font = 2)

polyWrap(natural_meta.pdata$metafor_obj[[1]], row = 28.8)
polyWrap(natural_meta.pdata$metafor_obj[[2]], row = 14.8)
polyWrap(natural_meta.pdata$metafor_obj[[3]], row = 0.8)

dev.off()

################################################################################
# Figure S8: built environment   
################################################################################
built.pdata <- model_1.pdata %>%
  dplyr::filter(variable %in% c(
    "frichness300_preg_iqr_c", "walkability_mean_preg_iqr_c", 
    "popdens_preg_iqr_c"))

built_coh.pdata <- built.pdata %>% dplyr::filter(cohort != "combined")
built_meta.pdata <- built.pdata %>% dplyr::filter(cohort == "combined") 

png(
  file = here("figures", "built_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forestWrap(
  coefs = built_coh.pdata, 
  rows = c(36:27, 23:14, 10:2), 
  ylim = c(0, 40))

text(-1.5, 39, "N subjects", cex = 0.6, font = 2)
text(-0.5, 39, "Study weight %", cex = 0.6, font = 2)

text(-2.38, 37.3, "Facility richness", cex = 0.6, font = 2)
text(-2.55, 24.3, "Walkability", cex = 0.6, font = 2)
text(-2.32, 11.3, "Population density", cex = 0.6, font = 2)

polyWrap(built_meta.pdata$metafor_obj[[1]], row = 25.8)
polyWrap(built_meta.pdata$metafor_obj[[2]], row = 12.8)
polyWrap(built_meta.pdata$metafor_obj[[3]], row = 0.8)

dev.off()

################################################################################
# SENSITIVITY PLOTS
################################################################################
################################################################################
# Figure S9: Subset analyses
################################################################################

## ---- Prepare data -----------------------------------------------------------
par.pdata <- prepMainPlot(
  mdata = sens_1_par.mdata, 
  ref = exp_preg_coef.ref) %>%
  mutate(subset = "first_preg")

comorb.pdata <- prepMainPlot(
  mdata = sens_preg_comorb.mdata, 
  ref = exp_preg_coef.ref) %>%
  mutate(subset = "no_comorbidities") 

psych.pdata <- prepMainPlot(
  mdata = sens_preg_dep.mdata, 
  ref = exp_preg_coef.ref) %>%
  mutate(subset = "no_psych")

subset.pdata <- bind_rows(par.pdata, comorb.pdata, psych.pdata) %>%
  dplyr::filter(cohort == "combined") %>%
  arrange(full_name, subset) %>%
  mutate(full_name = as.character(full_name)) %>%
  mutate(full_name = ifelse(
    subset %in% c("no_comorbidities", "no_psych"), "", full_name))

## ---- Make plot --------------------------------------------------------------
png(
  file = here("figures", "subset_sens.png"), 
  width = word_land, 
  height = 30, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

sub_cols <- c("black", palette_std[c(2, 5)])

forestWrap(
  coefs = subset.pdata,
  col_1 = "full_name",
  header = c("Exposure", "OR [95% CI]"),
  ylim = c(0, 69), 
  col = rep(sub_cols, 13), 
  xlim = c(-3.5, 3.5),
  alim = c(0, 2),  
  steps = 5, 
  ilab.xpos = c(nsub.pos, nstud.pos, i2.pos), 
  ilab =  cbind(
    subset.pdata %>% pull(n_studies), 
    subset.pdata %>% pull(valid_n),
    subset.pdata %>% pull(i2)), 
  rows = rev(c(1:3, 6:8, 11:13, 16:18, 21:23, 26:28, 31:33, 36:38, 41:43, 46:48, 
               51:53, 56:58, 61:63)), 
  cex = 0.7)

text(nsub.pos, 68, "N", cex = 0.7, font = 2)
text(nstud.pos, 68, "K", cex = 0.7, font = 2)
text(i2.pos, 68, "I2", cex = 0.7, font = 2)

text(-3.15, 65, "Air pollutants", cex = 0.7, font = 2)
text(-3.06, 50, "Road traffic noise", cex = 0.7, font = 2)
text(-3.12, 35, "Natural spaces", cex = 0.7, font = 2)
text(-3.06, 15, "Built environment", cex = 0.7, font = 2)

dev.off()

################################################################################
# Figure S10: Continuous exposures by quartile
################################################################################

## ---- Prepare data -----------------------------------------------------------
quart.pdata <- prepMainPlot(
  mdata = quart.mdata, 
  ref = exp_quart.ref, 
  levels = unique(exp_quart.ref$full_name)) %>%
  dplyr::filter(cohort == "combined") %>%
  separate(
    col = exposure, 
    into = c("exposure", "quantile"), 
    sep = "_(?!.*_)") %>%
  mutate(full_name = as.character(full_name)) %>%
  mutate(full_name = ifelse(
    quantile %in% 2:4, "", full_name)) %>%
  print(n = Inf)


## ---- Make plot --------------------------------------------------------------
png(
  file = here("figures", "quart_sens.png"), 
  width = word_land, 
  height = 30, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

quart_cols <- c("black", palette_std[c(2, 5, 3)])

forestWrap(
  coefs = quart.pdata,
  col_1 = "full_name",
  header = c("Exposure", "OR [95% CI]"),
  ylim = c(0, 50), 
  col = rep(quart_cols, 7), 
  xlim = c(-3.5, 3.5),
  alim = c(0, 2),  
  steps = 5, 
  ilab.xpos = c(nsub.pos, nstud.pos, i2.pos), 
  ilab =  cbind(
    quart.pdata %>% pull(n_studies), 
    quart.pdata %>% pull(valid_n),
    quart.pdata %>% pull(i2)), 
  rows = rev(c(1:4, 8:11, 15:18, 22:25, 29:32, 36:39, 43:46)), 
  cex = 1)

text(nsub.pos, 49, "N", cex = 1, font = 2)
text(nstud.pos, 49, "K", cex = 1, font = 2)
text(i2.pos, 49, "I2", cex = 1, font = 2)

dev.off()

################################################################################
# Figure 7: Joint exposures
################################################################################
adjust_neat <- tibble(
  adjustment = c("population", "green", "population_green", "noise", 
                 "pollution", "noise_pollution"), 
  adjustment_neat = c("Pop. density", "NDVI", "Pop. density & NDVI", "Lden", 
                      "NO2", "Lden & NO2")
)

joint.ref <- tibble(
  variable = c(
    "no2_preg", "pm25_preg", "pm10_preg", "lden_preg_f2", "lden_preg_f3", 
    "lden_preg_f4", "lden_preg_f5", "ndvi300_preg", "greenyn300_preg1", 
    "blueyn300_preg1",  "frichness300_preg", "walkability_mean_preg", 
    "popdens_preg"), 
  full_name = c(
    "NO2", "PM2.5", "PM10", "55-55.9 dB", "60-64.9 dB", "65-69.9 dB", ">70 dB", 
    "NDVI", "Green space within 300m", "Blue space within 300m", 
    "Facility richness", "Walkability score", "Population density"))

## ---- Prepare continuous data ------------------------------------------------
joint_cont <- joint.mod %>%
  dplyr::filter(exposure %in% c(
    "no2_preg_iqr_c", "pm25_preg_iqr_c", "pm10_preg_iqr_c", 
    "ndvi300_preg_iqr_c", "frichness300_preg_iqr_c", 
    "walkability_mean_preg_iqr_c", "popdens_preg_iqr_c")) 

joint_cont_tmp <- joint_cont %>% 
  pmap(function(exposure, meta, ...){
    
    meta %>%
      dplyr::filter(variable == exposure) %>%
      dplyr::filter(cohort == "combined") 
  })

joint_cont.pdata <- joint_cont_tmp %>%
  set_names(joint_cont$adjustment) %>%
  bind_rows(.id = "adjustment") %>%
  left_join(., adjust_neat, by = "adjustment") %>%
  left_join(., joint.ref, by = "variable") %>%
  mutate(full_name = ifelse(
    adjustment %in% c(
      "green", "population_green", "pollution", "noise_pollution"), "", 
    full_name))

## ---- Prepare noise data -----------------------------------------------------
joint_noise <- joint.mod %>% dplyr::filter(exposure %in% "lden_c_preg")
           
joint_noise <- joint_noise %>% 
  pmap(function(exposure, meta, ...){
    
    meta %>%
      dplyr::filter(variable %in% c(
      "lden_preg_f2", "lden_preg_f3", "lden_preg_f4", "lden_preg_f5")) %>%
      dplyr::filter(cohort == "combined") 
  }) %>% set_names(joint_noise$adjustment) %>%
  bind_rows(.id = "adjustment") %>%
  left_join(., adjust_neat, by = "adjustment") %>%
  left_join(., joint.ref, by = "variable") %>%
  mutate(adjustment_neat = ifelse(
    variable %in% c(
      "lden_preg_f3", "lden_preg_f4", "lden_preg_f5"), "", 
    adjustment_neat))

## ---- Prepare binary natural spaces data -------------------------------------
joint_nat_bin <- joint.mod %>% dplyr::filter(exposure %in% c(
  "greenyn300_preg", "blueyn300_preg"))

joint_nat_bin <- joint_nat_bin %>% 
  pmap(function(exposure, meta, ...){
    
    meta %>%
      dplyr::filter(variable %in% c(
        "greenyn300_preg1", "blueyn300_preg1")) %>%
      dplyr::filter(cohort == "combined") 
  }) %>% set_names(joint_nat_bin$adjustment) %>%
  bind_rows(.id = "adjustment") %>%
  left_join(., adjust_neat, by = "adjustment") %>%
  left_join(., joint.ref, by = "variable") %>%
  mutate(full_name = ifelse(
    adjustment %in% c(
      "pollution", "noise_pollution"), "", full_name))
  

## ---- Combine in correct order -----------------------------------------------
joint.pdata <- bind_rows(
  joint_cont.pdata %>% 
    dplyr::filter(exposure %in% c("no2_preg", "pm25_preg", "pm10_preg")), 
  joint_noise, 
  joint_cont.pdata %>% dplyr::filter(exposure == "ndvi300_preg"),
  joint_nat_bin, 
  joint_cont.pdata %>% 
    dplyr::filter(exposure %in% c(
      "frichness300_preg", "walkability_mean_preg", "popdens_preg")), 
)

## ---- Make plot --------------------------------------------------------------
png(
  file = here("figures", "joint.png"), 
  width = word_full, 
  height = 20, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forestWrap(
  coefs = joint.pdata,
  col_1 = "full_name",
  header = c("Exposure", "OR [95% CI]"),
  ylim = c(0, 66), 
  xlim = c(-6.2, 5.7),
  alim = c(0, 4),  
  steps = 5, 
  ilab.xpos = c(-3.5, -2.5, -1.5, -0.5), 
  ilab =  cbind(
    joint.pdata %>% pull(adjustment_neat), 
    joint.pdata %>% pull(n_studies), 
    joint.pdata %>% pull(valid_n),
    joint.pdata %>% pull(i2)), 
  rows = rev(c(1:3, 6:8, 11:13, 16:18, 21:23, 26:28, 31:34, 37:40, 43:46, 
               49:51, 54:56, 59:61)), 
  cex = 0.6, 
  cex.lab = 0.6,
  cex.axis = 0.6)

text(-3.5, 65, "Adjustment", cex = 0.6, font = 2)
text(-2.5, 65, "N", cex = 0.6, font = 2)
text(-1.5, 65, "K", cex = 0.6, font = 2)
text(-0.5, 65, "I2", cex = 0.6, font = 2)

text(-5.47, 62.3, "Air pollutants", cex = 0.6, font = 2)
text(-5.32, 47.3, "Road traffic noise", cex = 0.6, font = 2)
text(-5.43, 29.3, "Natural spaces", cex = 0.6, font = 2)
text(-5.32, 14.3, "Built environment", cex = 0.6, font = 2)

dev.off()



################################################################################
# ADDITIONAL SENSITIVITY: NOT INCLUDED IN MANUSCRIPT
################################################################################
################################################################################
# Using cohort-specific IQR for pollution
################################################################################

## ---- Prepare data -----------------------------------------------------------
pol_sep.pdata <- prepMainPlot(
  mdata = pol_sep.mdata, 
  ref = exp_preg_coef_sep.ref)

## ---- Make plot --------------------------------------------------------------
forestWrap(
  coefs = pol_sep.pdata, 
  rows = rev(c(1:8, 11:22, 25:36)),
  ylim = c(0, 40), 
  col = c(
    rep("black", 11), "#800000",
    rep("black", 11), "#800000", 
    rep("black", 7), "#800000"))

text(-1.5, 39, "N subjects", cex = 0.6, font = 2)

text(-2.74, 37.3, "NO2", cex = 0.6, font = 2)
text(-2.69, 23.3, "PM2.5", cex = 0.6, font = 2)
text(-2.70, 9.3, "PM10", cex = 0.6, font = 2)


################################################################################
# Using raw scores for pollution
################################################################################

## ---- Prepare data -----------------------------------------------------------
pol_raw.ref <- exp_preg.ref %>% 
  dplyr::filter(variable %in% c("no2_preg", "pm25_preg", "pm10_preg"))

pol_raw.pdata <- prepMainPlot(
  mdata = pol_raw.mdata, 
  ref = pol_raw.ref)

## ---- Make plot --------------------------------------------------------------
forestWrap(
  coefs = pol_raw.pdata, 
  rows = rev(c(1:8, 11:22, 25:36)),
  ylim = c(0, 40), 
  col = c(
    rep("black", 11), "#800000",
    rep("black", 11), "#800000", 
    rep("black", 7), "#800000"))




text(-1.5, 39, "N subjects", cex = 0.6, font = 2)

text(-2.74, 37.3, "NO2", cex = 0.6, font = 2)
text(-2.69, 23.3, "PM2.5", cex = 0.6, font = 2)
text(-2.70, 9.3, "PM10", cex = 0.6, font = 2)



