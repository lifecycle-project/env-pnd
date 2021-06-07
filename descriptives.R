################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Produce descriptive statistics   
## Date: 28th May 21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(DSI)
library(DSOpal)
library(dsBaseClient)
library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
#library(remotes)
#install_github("lifecycle-project/ds-helper", ref = "maintenance")
library(dsHelper)
library(forcats)
library(here)

ls("package:dsBaseClient")

conns <- datashield.login(logindata, restore = "env_pnd_9")
################################################################################
# 1. Define variable groups  
################################################################################
sep.vars <- c("areases_tert_preg", "areases_tert_1", "areases_quint_preg", 
              "areases_quint_1" )

pol.vars <- c("no2_preg", #"no2_1", 
              "pm25_preg", #"pm25_1", 
              "lden_preg", "lden_1")

nat.vars <- c("ndvi300_preg", "ndvi300_1", "green_dist_preg", "green_dist_1",
              "blue_dist_preg" , "blue_dist_1")

lu.vars <- c("bdens300_preg", "bdens300_1", "urbgr_preg", "urbgr_1",            
             "natgr_preg", "natgr_1", "agrgr_1", "walkability_mean_preg", 
             "walkability_mean_1", "landuseshan300_preg", 
             "landuseshan300_1", "frichness300_preg", "frichness300_1", 
             "fdensity300_preg", "fdensity300_1", "agrgr_preg")

################################################################################
# 2. Extract stats  
################################################################################
exposures.desc <- dh.getStats(
  df = "analysis_df", 
  vars = c(sep.vars, pol.vars, nat.vars, lu.vars)
)

outcome.desc <- dh.getStats(
  df = "analysis_df", 
  vars = "ppd"
)

################################################################################
# 3. Write descriptives  
################################################################################

# We do it like this because we can't make markdown files in the analysis 
# server so instead we do it locally.

save(exposures.desc, file = here("data", "exp_desc.RData"))
save(outcome.desc, file = here("data", "out_desc.RData"))

################################################################################
# 4. Heat map analysis  
################################################################################

## ---- Create two subsets with only the required variables --------------------

## Pregnancy
dh.dropCols(
  df = "analysis_df", 
  vars = c(
    "no2_preg", "pm25_preg", "lden_preg", "ndvi300_preg", "green_dist_preg", 
    "blue_dist_preg", "bdens100_preg", "bdens300_preg", "fdensity300_preg", 
    "frichness300_preg", "landuseshan300_preg", "walkability_mean_preg", 
    "agrgr_preg", "natgr_preg", "urbgr_preg"),
  comp_var = "child_id",
  type = "keep",
  new_df_name = "heat_preg")

# Birth to 12 months
dh.dropCols(
  df = "analysis_df", 
  vars = c(
    "areases_tert_1", "areases_quint_1", "fam_splitup_1", "no2_1", 
    "pm25_1", "lden_1", "ndvi300_1", "green_dist_1", "blue_dist_1", 
    "age_years_1", "cohab_1", "bdens100_1", "bdens300_1", "urbgr_1",
    "natgr_1", "agrgr_1", "walkability_mean_1", "landuseshan300_1",
    "frichness300_1", "fdensity300_1"),
  comp_var = "child_id",
  type = "keep",
  new_df_name = "heat_0_12")

## ---- Correlation matrices ---------------------------------------------------
exp_cor_preg <- ds.cor(
  x = "heat_preg", 
  type = "split"
)

exp_cor_0_12 <- ds.cor(
  x = "heat_0_12", 
  type = "split"
)
  
save(exp_cor_preg, file = here("data", "exp_cor_preg.RData"))
save(exp_cor_0_12, file = here("data", "exp_cor_0_12.RData"))

################################################################################
# 5. Box plots using Demetris' function  
################################################################################

## ---- Variables which exist for all cohorts ----------------------------------
violin_all.data <- dh.getAnonPlotData(
  df = "analysis_df", 
  vars = c(
    "no2_preg", "pm25_preg", "ndvi300_preg", "ndvi300_1", "green_dist_preg", 
    "green_dist_1", "blue_dist_preg", "blue_dist_1"))

## ---- Variables not present for DNBC -----------------------------------------
violin_built.data <- dh.getAnonPlotData(
  df = "analysis_df", 
  vars = c(
  "bdens100_preg", "bdens100_1", "bdens300_preg", "bdens300_1", 
  "fdensity300_preg", "fdensity300_1", "frichness300_preg", "frichness300_1", 
  "landuseshan300_preg", "landuseshan300_1", "walkability_mean_preg", 
  "walkability_mean_1", "agrgr_preg", "agrgr_1", "natgr_preg", "natgr_1", 
  "urbgr_preg", "urbgr_1"), 
  conns = conns[c("alspac", "genr", "moba", "ninfea")])

## ---- Lden -------------------------------------------------------------------
violin_noise.data <- dh.getAnonPlotData(
  df = "analysis_df", 
  vars = "lden_preg", 
  conns = conns[c("inma", "genr", "moba", "ninfea")])

violin_out <- c(violin_all.data, violin_built.data, violin_noise.data)

save(violin_out, file = here("data", "violin_out.RData"))

################################################################################
# 6. Create subsets for stratified odds ratios  
################################################################################

## ---- NDVI -------------------------------------------------------------------
dh.renameVars(
  df = "analysis_df", 
  names = tibble(oldvar = "ndvi300_preg", newvar = "ndvi_p")
)

ndvi_ref <- tibble(
  df = rep("analysis_df", 4),
  subset_var = rep("ndvi_p", 4), 
  low_val = seq(0, 0.75, 0.25),
  high_val = seq(0.25, 1, 0.25), 
  new_df_name = paste0("ndvi_", high_val)
)

ndvi_ref %>% 
  pmap(function(df, subset_var, low_val, high_val, new_df_name){
    
    dh.subsetBetween(
      df = df,
      subset_var = subset_var,
      low_val = low_val,
      high_val = high_val,
      new_df_name = new_df_name)
  })

################################################################################
# 7. Stratified odds ratios  
################################################################################
ppd_coh <- c("alspac", "genr", "moba", "ninfea")

ndvi_strat <- c("ndvi_0.4", "ndvi_0.6", "ndvi_0.8") %>% 
  map(
    ~ds.glmSLMA(
      formula = "ppd~ndvi_p", 
      family = "binomial", 
      dataName  = ., 
      datasources = conns[ppd_coh])
  )

 %>%
  map(
      
    dh.lmTab(
      model = ndvi_strat[[3]], 
      type = "slma", 
      coh_names = ppd_coh,
      direction = "wide", 
      ci_format = "separate")
    
))




ds.colnames("analysis_df")




