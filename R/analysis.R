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

conns <- datashield.login(logindata, restore = "env_pnd_24")
source("~/env-pnd/R/var-reference.R")


################################################################################
# 3. Descriptive statistics   
################################################################################

## ---- Analysis sample --------------------------------------------------------
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

## ---- Excluded participants --------------------------------------------------
exc.desc <- dh.getStats(
  df = "baseline_df", 
  
)




################################################################################
# 4. MISSING DATA
################################################################################
################################################################################
# 4a. Define models  
################################################################################
conns <- datashield.login(logindata, restore = "env_pnd_23")

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
# 4b. Define complete cases  
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
# 4c. Prepare data  
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
  x = paste0(ds.colnames("missing")[[1]], "_f"),
  newobj = "missing_f")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_miss_5")
conns <- datashield.login(logindata, restore = "env_pnd_miss_5")

################################################################################
# 4d. Extract descriptives  
################################################################################
miss_vars <- ds.colnames("missing_f")[[1]]

miss.descriptives <- dh.getStats(
  df = "missing_f", 
  vars = miss_vars
)

save.image("~/env-pnd/4-feb-22.RData")

################################################################################
# 4e. Regression models to see whether outcome is associated with missingness  
################################################################################
single_miss.fit <- single_miss.mod %>%
  pmap(function(formula, cohort, ...){
    
    ds.glm(
      formula = formula, 
      data = "baseline_df", 
      family = "binomial", 
      datasources = conns[cohort])
  })

single_miss.mod <- single_miss.mod %>%
  mutate(model = single_miss.fit)

save.image("~/env-pnd/4-feb-22.RData")


################################################################################
# 3. Summarise availability of data
################################################################################

## ---- Define exposures -------------------------------------------------------
exp.vars <- c(
  "no2_preg_iqr_c", "pm25_preg_iqr_c", "pm10_preg_iqr_c", "lden_c_preg", 
  "ndvi300_preg_iqr_c", "greenyn300_preg", "blueyn300_preg", 
  "frichness300_preg_iqr_c", "walkability_mean_preg_iqr_c", 
  "popdens_preg_iqr_c")

## ---- Check availability -----------------------------------------------------
avail_exp <- dh.anyData(
  df = "analysis_df", 
  vars = exp.vars)

avail_cov <- dh.anyData(
  df = "analysis_df", 
  vars = cov.vars)

joint_cov <- list(
  pop = dh.anyData("analysis_df", c(cov.vars, "popdens_preg_iqr_c")),
  green = dh.anyData("analysis_df", c(cov.vars, "ndvi300_preg_iqr_c")),
  pop_green = dh.anyData(
    "analysis_df", c(cov.vars, "popdens_preg_iqr_c", "ndvi300_preg_iqr_c")),
  noise = dh.anyData("analysis_df", c(cov.vars, "lden_c_preg")),
  polution = dh.anyData("analysis_df", c(cov.vars, "no2_preg_iqr_c")),
  noise_polution = dh.anyData(
    "analysis_df", c(cov.vars, "lden_c_preg", "no2_preg_iqr_c"))
)

save.image()


################################################################################
# 3. Build models
################################################################################

## ---- Single exposure --------------------------------------------------------
single.mod <- dt.buildModels(
  avail_exp = avail_exp,
  avail_cov = avail_cov)

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
conns <- datashield.login(logindata, restore = "env_pnd_16")

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



