################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Produce descriptive statistics   
## Date: 28th May 21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(dsHelper)


conns <- datashield.login(logindata, restore = "env_pnd_14")

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
# 2. Descriptives for analysis dataset  
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

desc_green$continuous %>% filter(cohort == "combined") %>% print(n = Inf)


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
    
    






