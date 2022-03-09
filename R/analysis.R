################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Conduct analyses 
## Date: 3rd August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(here)
library(dsTim)
library(dsHelper)
library(tidyverse)

#remove.packages("dsTim")

conns <- datashield.login(logindata, restore = "env_pnd_25")
source("~/env-pnd/R/var-reference.R")


################################################################################
# 1. Descriptive statistics: analysis sample
################################################################################
exp.desc <- dh.getStats(
  df = "analysis_df", 
  vars = c(exp_preg.vars, exp_year_1.vars)
)

cov.desc <- dh.getStats(
  df = "analysis_df", 
  vars = cov.vars
)

out.desc <- dh.getStats(
  df = "analysis_df", 
  vars = "ppd"
)

descriptives <- list(exp.desc, cov.desc, out.desc) %>%
  pmap(bind_rows)

save.image("~/env-pnd/4-feb-22.RData")

descriptives$categorical %>% print(n = Inf)

################################################################################
# 2. Descriptive statistics: excluded participants
################################################################################
exp_exc.desc <- dh.getStats(
  df = "excluded_df", 
  vars = c(exp_preg.vars)
)

cov_exc.desc <- dh.getStats(
  df = "excluded_df", 
  vars = cov.vars
)

out_exc.desc <- dh.getStats(
  df = "excluded_df", 
  vars = "ppd"
)

exc.desc <- list(exp_exc.desc, cov_exc.desc, out_exc.desc) %>%
  pmap(bind_rows)

save.image("~/env-pnd/4-feb-22.RData")


################################################################################
# 3. MISSING DATA
################################################################################
################################################################################
# 3a. Define models  
################################################################################
conns <- datashield.login(logindata, restore = "env_pnd_24")

avail_exp_miss <- dh.anyData(
  df = "analysis_df", 
  vars = exposure.ref$variable)

avail_cov_miss <- dh.anyData(
  df = "analysis_df", 
  vars = "sex")

single_miss.mod <- dt.buildModels(
  avail_exp = avail_exp_miss, 
  avail_cov = avail_cov_miss) %>%
  mutate(name = paste0(variable, "_miss"))

################################################################################
# 3b. Define complete cases  
################################################################################
single_miss.mod %>%
  pmap(function(variable, name, cohorts, covariates, outcome, ...){
    
    dt.defineCompleteCase(
      df = "baseline_df", 
      vars = c(variable, unlist(covariates), outcome), 
      newobj = name,
      conns = conns[cohorts]
    )
  })

datashield.workspace_save(conns, "env_pnd_miss_1")
conns <- datashield.login(logindata, restore = "env_pnd_miss_1")

################################################################################
# 3c. Prepare data  
################################################################################

## ---- Tibble of variables to join --------------------------------------------
tojoin <- single_miss.mod %>%
  group_by(cohort) %>%
  group_split 

## ---- Join into one dataframe ------------------------------------------------
tojoin %>%
  map(function(x){
    
    ds.dataFrame(
      x = x$name,
      newobj = "missing",
      datasources = conns[x$cohort[[1]]]
    )
  })

datashield.workspace_save(conns, "env_pnd_miss_2")
conns <- datashield.login(logindata, restore = "env_pnd_miss_2")

## ---- Fill missing columns ---------------------------------------------------
ds.dataFrameFill("missing", "missing")

datashield.workspace_save(conns, "env_pnd_miss_3")
conns <- datashield.login(logindata, restore = "env_pnd_miss_3")

## ---- Fix levels -------------------------------------------------------------
ds.colnames("missing")[[1]] %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("missing$", .x), 
      newobj.name = paste0(.x, "_f"))
  )

datashield.workspace_save(conns, "env_pnd_miss_4")
conns <- datashield.login(logindata, restore = "env_pnd_miss_4")

## ---- Join back into one dataframe -------------------------------------------
ds.dataFrame(
  x = c("baseline_df", paste0(ds.colnames("missing")[[1]], "_f")),
  newobj = "missing_f")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_miss_5")
conns <- datashield.login(logindata, restore = "env_pnd_miss_5")

################################################################################
# 3d. Extract descriptives  
################################################################################
miss_vars <- ds.colnames("missing_f")[[1]]

miss.descriptives <- dh.getStats(
  df = "missing_f", 
  vars = miss_vars
)

miss.descriptives$categorical$variable

save.image("~/env-pnd/4-feb-22.RData")

################################################################################
# 3e. Regression models to see whether outcome is associated with missingness  
################################################################################
ds.colnames("missing_f")

missing.mod <- single_miss.mod %>%
  mutate(formula = paste0(
    paste0(variable, "_miss"), 
    "~ppd+sex"
  ))

single_miss.fit <- dh.multGLM(
  df = "missing_f", 
  model_names = missing.mod$variable,
  formulae = missing.mod$formula,
  cohorts = missing.mod$cohort)

save.image("~/env-pnd/4-feb-22.RData")

## Could also have a look at more detailed missingness patterns, maybe see
## who requests it.


################################################################################
# 4. Correlations between pregnancy exposures
################################################################################
## ---- Create subsets with required variables ---------------------------------
dh.dropCols(
  df = "analysis_df", 
  vars = exp_preg.vars,
  type = "keep",
  new_obj = "heat_preg")

## ---- Correlation matrices ---------------------------------------------------
exp_cor_preg <- ds.cor(
  x = "heat_preg", 
  type = "split",
  naAction = "pairwise.complete"
)

save.image("~/env-pnd/4-feb-22.RData")

################################################################################
# MAIN RESULTS
################################################################################
################################################################################
# 5. Summarise availability of data
################################################################################
conns <- datashield.login(logindata, restore = "env_pnd_30")

ds.colnames("analysis_df")

## ---- Define covariate models ------------------------------------------------
cov.def <- list(
  model_1 = c(
    "edu_m_0", "areases_tert", "parity_bin", "agebirth_m_y", 
    "birth_month", "birth_year"), 
  model_2 = "eusilc_income_quintiles", 
  model_3 = "ethn3_m",
  model_4 = "cohab"
)

ds.colnames("analysis_df", datasources = conns[c("eden_nan", "eden_poit")])
ds.summary("analysis_df$areases_tert", datasources = conns[eden_coh])

## ---- Check availability -----------------------------------------------------
avail_exp <- dh.anyData(
  df = "analysis_df", 
  vars = exp_preg_analysis.vars)

avail_cov_1 <- dh.anyData(
  df = "analysis_df", 
  vars = cov.def$model_1)

avail_cov_2 <- dh.anyData(
  df = "analysis_df", 
  vars = cov.def$model_2)

avail_cov_3 <- dh.anyData(
  df = "analysis_df", 
  vars = cov.def$model_3)

avail_cov_4 <- dh.anyData(
  df = "analysis_df", 
  vars = cov.def$model_4)

avail_cov <- list(
  model_1 = avail_cov_1,
  model_2 = bind_rows(avail_cov_1, avail_cov_2), 
  model_3 = bind_rows(avail_cov_1, avail_cov_3), 
  model_4 = bind_rows(avail_cov_1, avail_cov_4)
)

save.image()

################################################################################
# SINGLE EXPOSURE MODELS
################################################################################

## ---- Build models -----------------------------------------------------------
single.mod <- list(
  model_1 = dh.buildModels(
    exp_tib = avail_exp, 
    cov_tib = avail_cov$model_1, 
    outcome = "ppd"), 
  model_2 = dh.buildModels(
    exp_tib = avail_exp, 
    cov_tib = avail_cov$model_2,
    outcome = "ppd"), 
  model_3 = dh.buildModels(
    exp_tib = avail_exp, 
    cov_tib = avail_cov$model_3, 
    outcome = "ppd"), 
  model_4 = dh.buildModels(
    exp_tib = avail_exp,
    cov_tib = avail_cov$model_4, 
    outcome = "ppd")
)

################################################################################
# 6. Main model
################################################################################
conns <- datashield.login(logindata, restore = "env_pnd_30")

## ---- Fit models= ------------------------------------------------------------
model_1.fit <- dh.multGLM(
  df = "analysis_df", 
  ref = single.mod$model_1)

## ---- Check convergence ------------------------------------------------------
model_1.fit %>% dplyr::filter(converged != TRUE)

## ---- Restrict to models that converged --------------------------------------
model_1_conv.fit <- model_1.fit %>% dplyr::filter(converged == TRUE)

## ---- Meta-analyse coefficients ----------------------------------------------
model_1.mdata <- dh.metaSepModels(
  ref = model_1_conv.fit,
  exp = TRUE, 
  method = "REML", 
  output = "both")

################################################################################
# 7. Different buffers for NDVI
################################################################################
green_extra <- dh.getStats(
  df = "nonrep", 
  vars = c("ndvi100_preg", "ndvi300_preg", "ndvi500_preg")

################################################################################
# 8. Adjusted for income
################################################################################

## ---- Fit models -------------------------------------------------------------
#model_2.fit <- dh.multGLM(
#  df = "analysis_df", 
#  ref = single.mod$model_2)

## ---- See what didn't converged ----------------------------------------------
#model_2.fit %>% dplyr::filter(converged != TRUE)

## ---- Restrict to models that converged --------------------------------------
#model_2_conv.fit <- model_2.fit %>% dplyr::filter(converged == TRUE)

## ---- Meta-analyse coefficients ----------------------------------------------
#model_2.mdata <- dh.metaSepModels(
#  ref = model_2_conv.fit,
#  exp = TRUE, 
#  method = "REML", 
#  coh_out = TRUE)

#save.image("~/env-pnd/4-feb-22.RData")


################################################################################
# 9. Adjusted for ethnicity
################################################################################

## ---- Restrict to cohorts with variability -----------------------------------
model_3_rest <- single.mod$model_3 %>% 
  dplyr::filter(cohort %in% c("alspac", "bib", "genr"))

## ---- Fit models -------------------------------------------------------------
model_3.fit <- dh.multGLM(
  df = "analysis_df", 
  ref = model_3_rest)

## ---- See what converged -----------------------------------------------------
model_3.fit %>% dplyr::filter(converged != TRUE)

## ---- Restrict to models that converged --------------------------------------
model_3_conv.fit <- model_3.fit %>% dplyr::filter(converged == TRUE)

## ---- Meta-analyse coefficients ----------------------------------------------
model_3.mdata <- dh.metaSepModels(
  ref = model_3_conv.fit,
  exp = TRUE, 
  method = "REML", 
  output = "both")

################################################################################
# 10. Adjusted for cohabitation
################################################################################

## ---- Fit models -------------------------------------------------------------
model_4_rest.mod <- single.mod$model_4 %>% 
  dplyr::filter(cohort %in% c("alspac", "bib", "dnbc", "genr", "moba"))

model_4.fit <- dh.multGLM(
  df = "analysis_df", 
  ref = model_4_rest.mod)

## ---- See what converged -----------------------------------------------------
model_4.fit %>% dplyr::filter(converged != TRUE)

## ---- Restrict to models that converged --------------------------------------
model_4_conv.fit <- model_4.fit %>% dplyr::filter(converged == TRUE)

## ---- Get coefficients -------------------------------------------------------
model_4.mdata <- dh.metaSepModels(
  ref = model_4_conv.fit,
  exp = TRUE, 
  method = "REML", 
  output = "both")

save.image()

conns <- datashield.login(logindata, restore = "env_pnd_30")

################################################################################
# 11. Restricting to first-time mothers
################################################################################

## ---- Define covariates ------------------------------------------------------

## Here we excluded parity
sens_par.cov <- c(
  "edu_m_0", "areases_tert", "sex", "agebirth_m_y", "birth_month", "birth_year")

## ---- Get availability -------------------------------------------------------
avail_sens_par <- dh.anyData(
  df = "parity_df", 
  vars = sens_par.cov)

## ---- Build models -----------------------------------------------------------
sens_1_par.mod = dh.buildModels(
  exp_tib = avail_exp, 
  cov_tib = avail_sens_par, 
  outcome = "ppd")

## ---- Fit models -------------------------------------------------------------
sens_1_par.fit <- dh.multGLM(
  df = "parity_df", 
  ref = sens_1_par.mod)

## ---- See what converged -----------------------------------------------------
sens_1_par.fit %>% dplyr::filter(converged != TRUE)

## ---- Restrict to models that converged --------------------------------------
sens_1_par_conv.fit <- sens_1_par.fit %>% dplyr::filter(converged == TRUE)

## ---- Meta-analyse coefficients ----------------------------------------------
sens_1_par.mdata <- dh.metaSepModels(
  ref = sens_1_par_conv.fit,
  exp = TRUE, 
  method = "REML", 
  output = "both")

save.image()

################################################################################
# 12. Restricting to pregnancies free from comorbidities
################################################################################
sens_preg_comorb.fit <- dh.multGLM(
  df = "no_comorbid_df", 
  ref = single.mod$model_1)

## ---- Check convergence ------------------------------------------------------
sens_preg_comorb.fit %>% dplyr::filter(converged != TRUE)

## ---- Restrict to models that converged --------------------------------------
sens_preg_comorb_conv.fit <- sens_preg_comorb.fit %>% 
  dplyr::filter(converged == TRUE)

## ---- Meta-analyse coefficients ----------------------------------------------
sens_preg_comorb.mdata <- dh.metaSepModels(
  ref = sens_preg_comorb_conv.fit,
  exp = TRUE, 
  method = "REML", 
  output = "both")

save.image()

## Something didn't converge in the meta analysis. Maybe add in some error
## handling to the meta-analysis function.

################################################################################
# 13. Restricting to women with no previous history of depression
################################################################################
sens_preg_dep.fit <- dh.multGLM(
  df = "no_dep_psych_df", 
  ref = single.mod$model_1)

## ---- Check convergence ------------------------------------------------------
sens_preg_dep.fit %>% dplyr::filter(converged != TRUE)

## ---- Restrict to models that converged --------------------------------------
sens_preg_dep_conv.fit <- sens_preg_dep.fit %>% 
  dplyr::filter(converged == TRUE)

## ---- Meta-analyse coefficients ----------------------------------------------
sens_preg_dep.mdata <- dh.metaSepModels(
  ref = sens_preg_dep_conv.fit,
  exp = TRUE, 
  method = "REML", 
  output = "both")

save.image()

################################################################################
# 14. Non-linear models
################################################################################

## ---- Check coverage of exposures --------------------------------------------
avail_exp_quart <- dh.anyData(
  df = "analysis_df", 
  vars = exp_quart.ref$variable)

## ---- Build models -----------------------------------------------------------
quart.mod <- dh.buildModels(
  exp_tib = avail_exp_quart,
  cov_tib = avail_cov$model_1,
  outcome = "ppd")

## ---- Fit models -------------------------------------------------------------
quart.fit <- dh.multGLM(
  df = "analysis_df", 
  ref = quart.mod)

## ---- See what converged -----------------------------------------------------
quart.fit %>% dplyr::filter(converged == FALSE) %>% print(n = Inf)

quart_conv.fit <- quart.fit %>% dplyr::filter(converged == TRUE) 

## ---- Meta-analyse coefficients ----------------------------------------------
quart.mdata <- dh.metaSepModels(
  ref = quart_conv.fit,
  exp = TRUE, 
  method = "REML", 
  output = "both")

conns <- datashield.login(logindata, restore = "env_pnd_30")

save.image()

################################################################################
# 14. 100m buffers
################################################################################





################################################################################
# 15. Joint pollution models
################################################################################

## ---- Get exposure details ---------------------------------------------------
joint_exp <- list(
  no2 = dh.anyData("analysis_df", "no2_preg"),
  pm25 = dh.anyData("analysis_df", "pm25_preg"),
  pm10 = dh.anyData("analysis_df", "pm10_preg"),
  noise = dh.anyData("analysis_df", "lden_preg_f"),
  ndvi = dh.anyData("analysis_df", "ndvi300_preg"),
  green = dh.anyData("analysis_df", "greenyn300_preg"),
  blue = dh.anyData("analysis_df", "blueyn300_preg"),
  facility = dh.anyData("analysis_df", "frichness300_preg"),
  walkability = dh.anyData("analysis_df", "walkability_mean_preg"), 
  population = dh.anyData("analysis_df", "popdens_preg"))
  
## ---- Get covariate details --------------------------------------------------
joint_cov <- list(
  pop = dh.anyData("analysis_df", c(cov.def$model_1, "popdens_preg_iqr_c")),
  green = dh.anyData("analysis_df", c(cov.def$model_1, "ndvi300_preg_iqr_c")),
  pop_green = dh.anyData(
    "analysis_df", c(cov.def$model_1, "popdens_preg_iqr_c", "ndvi300_preg_iqr_c")),
  noise = dh.anyData("analysis_df", c(cov.def$model_1, "lden_c_preg")),
  polution = dh.anyData("analysis_df", c(cov.def$model_1, "no2_preg_iqr_c")),
  noise_polution = dh.anyData(
    "analysis_df", c(cov.def$model_1, "lden_c_preg", "no2_preg_iqr_c"))
)

save.image()

## ---- Make reference tibbles -------------------------------------------------
air.ref <- tibble(
  exposure = c(
    rep(c("no2_preg_iqr_c", "pm25_preg_iqr_c", "pm10_preg_iqr_c"), each = 3)),
  adjustment = rep(c("population", "green", "population_green"), 3), 
  exp_ref = rep(c(
    list(joint_exp$no2),
    list(joint_exp$pm25),
    list(joint_exp$pm10)), each = 3),
  cov_ref = rep(c(
    list(joint_cov$pop), 
    list(joint_cov$green),
    list(joint_cov$pop_green)), 3))
    
noise.ref <- tibble(
  exposure = rep("lden_c_preg", 3), 
  adjustment = c("population", "green", "population_green"),
  exp_ref = rep(list(joint_exp$noise), 3),
  cov_ref = c(
    list(joint_cov$pop),
    list(joint_cov$green), 
    list(joint_cov$pop_green)))

nat.ref <- tibble(
  exposure = rep(c("ndvi300_preg_iqr_c", "greenyn300_preg", "blueyn300_preg"), 
                 each = 3),
  adjustment = rep(c("noise", "pollution", "noise_pollution"), 3), 
  exp_ref = rep(c(
    list(joint_exp$ndvi),
    list(joint_exp$green),
    list(joint_exp$blue)), each = 3),
  cov_ref = rep(c(
      list(joint_cov$noise), 
      list(joint_cov$polution), 
      list(joint_cov$noise_polution)), 3))

built.ref <- tibble(
  exposure = rep(c("frichness300_preg_iqr_c", "walkability_mean_preg_iqr_c", 
                   "popdens_preg_iqr_c"), each = 3), 
  adjustment = rep(c("noise", "pollution", "noise_pollution"), 3),
  exp_ref = rep(c(
    list(joint_exp$facility),
    list(joint_exp$walkability),
    list(joint_exp$population)), each = 3),
  cov_ref = rep(c(
    list(joint_cov$noise), 
    list(joint_cov$polution), 
    list(joint_cov$noise_polution)), 3))

joint.ref <- bind_rows(air.ref, noise.ref, nat.ref, built.ref)
  
## ---- Build models -----------------------------------------------------------
joint_tmp.mod <- joint.ref %>%
  pmap(function(exp_ref, cov_ref, ...){
    
    dh.buildModels(
      exp_tib = exp_ref, 
      cov_tib = cov_ref, 
      outcome = "ppd")
  
    })

joint.mod <- joint.ref %>%
  mutate(model = joint_tmp.mod)
  
## ---- Fit models -------------------------------------------------------------
joint_tmp.fit <- joint.mod %>%
  pmap(function(model, ...){
    
    dh.multGLM(
      df = "analysis_df", 
      ref = model)
    
  })

## ---- Check didn't converged -------------------------------------------------
joint_no_convergence.fit <- joint_tmp.fit %>%
  map(function(x){
    
    x %>% dplyr::filter(converged == FALSE)
    
  })

## ---- Restrict to those that did converge ------------------------------------
joint_converged.fit <- joint_tmp.fit  %>%
  map(function(x){
    
    x %>% dplyr::filter(converged == TRUE)
    
  })

joint.mod <- joint.mod %>%
  mutate(fit = joint_converged.fit)

## ---- Meta-analyse models ----------------------------------------------------
joint.meta <- joint.mod$fit %>%
  map(function(x){
    
    dh.metaSepModels(
      ref = x,
      exp = TRUE, 
      method = "REML", 
      output = "both")
    
  })

joint.mod <- joint.mod %>%
  mutate(meta = joint.meta)

save.image()

joint.mod$meta





################################################################################
# Sensitivity analyses using different transformations of exposure
################################################################################

## Probably won't include in manuscript, but for my piece of mind

################################################################################
# Using cohort-specific IQR
################################################################################
pol_iqr_sep.vars <- c("no2_preg_iqr_s", "pm25_preg_iqr_s", "pm10_preg_iqr_s")

## ---- Get data availability --------------------------------------------------
avail_exp_cohort <- dh.anyData(
  df = "analysis_df", 
  vars = pol_iqr_sep.vars
)

## ---- Build models -----------------------------------------------------------
pol_sep.mod <- dt.buildModels(
  avail_exp = avail_exp_cohort, 
  avail_cov = avail_cov$model_1)

## ---- Fit models -------------------------------------------------------------
pol_sep.fit <- dh.multGLM(
  df = "analysis_df", 
  ref = pol_sep.mod)

## ---- Meta-analyse coefficients ----------------------------------------------
pol_sep.mdata <- dh.metaSepModels(
  ref = pol_sep.fit,
  exp = TRUE, 
  method = "REML", 
  coh_out = TRUE)


################################################################################
# Using raw scores
################################################################################
pol_raw.vars <- c("no2_preg", "pm25_preg", "pm10_preg")

## ---- Get data availability --------------------------------------------------
avail_exp_cohort <- dh.anyData(
  df = "analysis_df", 
  vars = pol_raw.vars
)

## ---- Build models -----------------------------------------------------------
pol_raw.mod <- dt.buildModels(
  avail_exp = avail_exp_cohort, 
  avail_cov = avail_cov$model_1)

## ---- Fit models -------------------------------------------------------------
pol_raw.fit <- dh.multGLM(
  df = "analysis_df", 
  ref = pol_raw.mod)

## ---- Meta-analyse coefficients ----------------------------------------------
pol_raw.mdata <- dh.metaSepModels(
  ref = pol_raw.fit,
  exp = TRUE, 
  method = "REML", 
  coh_out = TRUE)












## ---- Joint exposures: air pollution ------------------------------------------
air_exp <- avail_exp %>%
  dplyr::filter(
    variable %in% c("no2_preg_iqr_c", "pm25_preg_iqr_c", "pm10_preg_iqr_c"))

joint_air.mod <- list(
  adj_pop = dt.buildModels(air_exp, joint_cov$pop),
  adj_green = dt.buildModels(air_exp, joint_cov$green),
  adj_pop_green = dt.buildModels(air_exp, joint_cov$pop_green))

## ---- Joint exposures: traffic noise -----------------------------------------
noise_exp <- avail_exp %>% dplyr::filter(variable == "lden_c_preg")

joint_noise.mod <- list(
  adj_pop = dt.buildModels(noise_exp, joint_cov$pop),
  adj_green = dt.buildModels(noise_exp, joint_cov$green),
  adj_pop_green = dt.buildModels(noise_exp, joint_cov$pop_green))

## ---- Joint exposures: natural spaces ----------------------------------------
nat_exp <- avail_exp %>% dplyr::filter(
  variable %in% c("ndvi300_preg_iqr_c", "greenyn300_preg", "blueyn300_preg")
)

joint_natural.mod <- list(
  adj_noise = dt.buildModels(nat_exp, joint_cov$noise),
  adj_polution = dt.buildModels(nat_exp, joint_cov$polution),
  adj_noise_polution = dt.buildModels(nat_exp, joint_cov$noise_polution))

## ---- Joint exposures: urban environment -------------------------------------
urban_exp <- avail_exp %>% dplyr::filter(
  variable %in% c("frichness300_preg_iqr_c", "walkability_mean_preg_iqr_c", 
                  "popdens_preg_iqr_c"))

joint_urb.mod <- list(
  adj_noise = dt.buildModels(urban_exp, joint_cov$noise),
  adj_polution = dt.buildModels(urban_exp, joint_cov$polution),
  adj_noise_polution = dt.buildModels(urban_exp, joint_cov$noise_polution))

## ---- Joint exposures: traffic and air pollution ------------------------------
joint_air_traffic <- dt.buildModels(air_exp, joint_cov$noise)

save.image("~/env-pnd/4-feb-22.RData")


################################################################################
# MAIN MODELS
################################################################################
conns <- datashield.login(logindata, restore = "env_pnd_29")

################################################################################
# 5. Single exposure, unadjusted
################################################################################
single_m1.fit = list(
  ipd = single_m1.mod %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = single_m1.mod %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

save.image()
################################################################################
# 6. Single exposure, adjusted
################################################################################
single_m2_fixed.mod <- single_m2.mod %>%
  dt.changeForm(
    elements = "pop_dens", 
    vars = "inma_gip", 
    category = "cohorts", 
    type = "remove") 

single_m2.fit = list(
  ipd = single_m2_fixed.mod %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = single_m2_fixed.mod %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 



save.image()



################################################################################
# 7. Multiple exposures
################################################################################

## ---- Air polution -----------------------------------------------------------
joint_air_pd.fit = list(
  ipd = joint_air.mod$adj_pd %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_air.mod$adj_pd %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

joint_air_gs.fit = list(
  ipd = joint_air.mod$adj_gs %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_air.mod$adj_gs %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

joint_air_pd_gs.fit = list(
  ipd = joint_air.mod$adj_pd_gs %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_air.mod$adj_pd_gs %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

## ---- Road traffic noise -----------------------------------------------------
joint_noise_pd.fit = list(
  ipd = joint_noise.mod$adj_pd %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_noise.mod$adj_pd %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

joint_noise_gs.fit = list(
  ipd = joint_noise.mod$adj_gs %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_noise.mod$adj_gs %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

joint_noise_pd_gs.fit = list(
  ipd = joint_noise.mod$adj_pd_gs %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_noise.mod$adj_pd_gs %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial"))

## ---- Natural spaces ---------------------------------------------------------
joint_nat_tn.fit = list(
  ipd = joint_nat.mod$adj_tn %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_nat.mod$adj_tn %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

joint_nat_pl.fit = list(
  ipd = joint_nat.mod$adj_pl %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_nat.mod$adj_pl %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

joint_nat_tn_pl.fit = list(
  ipd = joint_nat.mod$adj_tn_pl %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_nat.mod$adj_tn_pl %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

## ---- Urban environment ------------------------------------------------------
joint_urb_tn.fit = list(
  ipd = joint_urb.mod$adj_tn %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_urb.mod$adj_tn %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

joint_urb_pl.fit = list(
  ipd = joint_urb.mod$adj_pl %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_urb.mod$adj_pl %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

joint_urb_tn_pl.fit = list(
  ipd = joint_urb.mod$adj_tn_pl %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_urb.mod$adj_tn_pl %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

## ---- Joint pollution & traffic noise -----------------------------------------
joint_air_traf.fit = list(
  ipd = joint_air_traffic %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = joint_air_traffic %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

save.image()

################################################################################
# 8. Diagnostics  
################################################################################
################################################################################
# Correlations between SEP variables and exposures
################################################################################
diagModelBuilder <- function(cov_avail, exposure){
  avail_exp %>%
    mutate(variable = factor(
      variable, 
      levels = exp_ref$exposure, 
      ordered = TRUE)) %>%
    group_by(variable) %>%
    group_split %>%
    map(~bind_rows(., cov_avail)) %>%
    map(function(x){
      x %>% 
        select(-variable) %>% 
        dplyr::select(where(function(x) all(x) == TRUE)) %>%
        colnames}) %>%
    set_names(exp_ref$exposure) %>% 
    imap(function(.x, .y){
      out <- list(
        exposure = exposure, 
        outcome = .y,
        covariates = NULL,
        cohorts = .x)}) %>%
    set_names(exp_ref$name)
}

cont_exp <- c("ndvi", "no2", "pm25", "pm10", "fac_rich", "walk", "pop_dens")


## ---- Maternal education -----------------------------------------------------
edu_avail <- dh.anyData(df = "analysis_df", vars = "edu_m_0")
edu.mod <- diagModelBuilder(edu_avail, "edu_m_0")

edu.fit <- edu.mod[cont_exp] %>%
  map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
  map(dt.glmWrap, type = "ipd", family = "gaussian") 

## ---- Area deprivation -------------------------------------------------------
area_avail <- dh.anyData(df = "analysis_df", vars = "areases_tert")
area.mod <- diagModelBuilder(area_avail, "areases_tert")

area.fit <- area.mod[cont_exp] %>%
  dt.changeForm(
    elements = "pop_dens", 
    vars = "inma_gip", 
    category = "cohorts", 
    type = "remove") %>%
  map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
  map(dt.glmWrap, type = "ipd", family = "gaussian") 

save.image()




## Strange results for air polution, need to work out what is going on

test1 <- ds.glm(
  formula = "ppd~no2_preg_iqr_c", 
  data = "analysis_df", 
  family = "binomial"
)

test1 <- ds.glm(
  formula = "ppd~no2_preg", 
  data = "analysis_df", 
  family = "binomial"
)


test2 <- ds.glm(
  formula = "ppd~no2_preg_iqr_p+sex", 
  data = "analysis_df", 
  family = "binomial"
)

test3 <- ds.glm(
  formula = "ppd~no2_preg_iqr_p+sex+areases_tert_preg", 
  data = "analysis_df", 
  family = "binomial"
)


## ---- Create two subsets for with and without ppd ----------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$ppd", 
  V2.name = "1",
  Boolean.operator = "==", 
  newobj = "ppd_sub"
)

ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$ppd", 
  V2.name = "0",
  Boolean.operator = "==", 
  newobj = "noppd_sub"
)

ds.table("ppd_sub$no2_preg")
ds.table("ppd_sub$no2_preg")

ds.mean("ppd_sub$no2_preg", type = "split")
ds.mean("noppd_sub$no2_preg", type = "split")

ds.dim("ppd_sub")
ds.dim("noppd_sub")

################################################################################
# What's going on with BiB?
################################################################################
pol_diags <- dh.getStats(
  df = "analysis_df", 
  vars = c("no2_preg_iqr_c", "pm25_preg_iqr_c", "pm10_preg_iqr_c"))

pol_diags

ds.cor("analysis_df$no2_preg_iqr_c", "analysis_df$ppd", 
       datasources = conns["bib"])

single.mod$model_1 %>%
  dplyr::filter(variable %in% c(
    "no2_preg_iqr_c", "pm25_preg_iqr_c", "pm10_preg_iqr_c") & cohort == "bib") %>%
  pull(formula)

## ---- Full model -------------------------------------------------------------
bib_no2_full <- ds.glmSLMA(
  formula = "ppd~no2_preg_iqr_c+edu_m_0+areases_tert+parity_bin+sex+agebirth_m_y+birth_month+birth_year",
  dataName = "analysis_df", 
  family = "binomial", 
  datasources = conns["bib"]
)

ds.colnames("analysis_df")
exp(-1.84001567)

## ---- Unadjusted model -------------------------------------------------------
bib_no2_unadj <- ds.glmSLMA(
  formula = "ppd~no2_preg_iqr_c",
  dataName = "analysis_df", 
  family = "binomial", 
  datasources = conns["bib"]
)

exp(-1.769548)

## ---- Using raw pollution ----------------------------------------------------
bib_no2_unadj <- ds.glmSLMA(
  formula = "ppd~no2_preg",
  dataName = "analysis_df", 
  family = "binomial", 
  datasources = conns["bib"]
)

exp(-0.2356824)

## Hmm, that looks more sensible. Let's try fitting using the cohort-specific
## scores.

sep_vars <- c("no2_preg_iqr_s", "pm25_preg_iqr_s", "pm10_preg_iqr_s")



pol_sep_conv.fit <- pol_sep.fit %>% dplyr::filter(converged == TRUE)

pol_sep.mdata <- dh.metaSepModels(
  ref = pol_sep_conv.fit,
  exp = TRUE, 
  method = "REML", 
  coh_out = TRUE)

pol_sep.pdata <- pol_sep.mdata %>%
  dplyr::filter(variable %in% sep_vars) %>%
  left_join(., exp_preg_coef.ref, by = "variable") %>%
  left_join(., names_neat, by = "cohort") %>%
  mutate(full_name = factor(
    full_name, 
    levels = exp_preg_coef.ref$full_name,
    ordered = TRUE)) %>%
  arrange(full_name)

pol_sep.pdata %>% dplyr::filter(cohort == "bib")

exp(0.357)
exp(0.0178)
exp(0.577)

pol_sep.pdata <- pol_sep.pdata %>% 
  dplyr::filter(cohort == "combined") 

fig_6.pdata <- model_1.pdata %>% 
  dplyr::filter(cohort == "combined") %>%
  dplyr::filter(model%in% c("no2_preg_iqr_c", "pm25_preg_iqr_c", "pm10_preg_iqr_c"))





