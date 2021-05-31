################################################################################
## Project: Urban environment and postnatal depression    
## Script purpose: Put analysis script together 
## Date: 25th May 21
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

################################################################################
# 1. Define variables and tables
################################################################################

## ---- Non-repeated -----------------------------------------------------------
nonrep.vars <- list(
  variables = c(
  "areases_tert_preg", "areases_quint_preg", "parity_m", "sex", 
  "birth_month", "eusilc_income_quintiles", "agebirth_m_y", "ethn1_m", 
  "ethn2_m", "ethn3_m", "coh_country", "preg_smk", "preg_alc", "preg_cig", 
  "preg_alc_unit", "breastfed_any", "breastfed_ever", "no2_preg", "pm25_preg", 
  "lden_preg", "ndvi300_preg", "green_dist_preg", "blue_dist_preg", "cohort_id", 
  "ppd", "preg_dia", "preg_ht", "ga_bj", "ga_us", "prepreg_dep", "child_id", 
  "child_no", "preg_no", "mother_id", "outcome", "con_anomalies", "birth_month", 
  "bdens100_preg", "bdens300_preg", "fdensity300_preg", "frichness300_preg", 
  "landuseshan300_preg", "walkability_mean_preg", "agrgr_preg", "natgr_preg", 
  "urbgr_preg"), 
  tables = c(
    "alspac/2_1_core_1_2/non_rep",
    #"lc_dnbc_core_2_1.2_1_core_non_rep_tcadman_2020-lc19", 
    "lc_eden_core_2_1.Project22_non_rep", 
    "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_210118_1", 
    "lc_genr_core_2_2.2_1_core_non_rep_TC _ECCNLC202053", 
    "lc_moba_core_2_1.2_1_core_2021_1_non_rep_environment_depression", 
    "lc_ninfea_core_2_1.p12_tcadman"))

## ---- Trimester repeated -----------------------------------------------------
trirep.vars <- list(
  variables = c(
"child_id", "bdens100_t1", "bdens100_t2", "bdens100_t3", "bdens300_t1", 
"bdens300_t2", "bdens300_t3", "fdensity300_t1", "fdensity300_t2", "fdensity300_t3", 
"frichness300_t1", "frichness300_t2", "frichness300_t3", "landuseshan300_t1", 
"landuseshan300_t2", "landuseshan300_t3", "walkability_mean_t1", 
"walkability_mean_t2", "walkability_mean_t3", "agrgr_t1", "agrgr_t2", 
"agrgr_t3", "natgr_t1", "natgr_t2", "natgr_t3", "urbgr_t1", "urbgr_t2", 
"urbgr_t3", "no2_t1", "no2_t2", "no2_t3", "pm25_t1", "pm25_t2", "pm25_t3", 
"lden_t1", "lden_t2", "lden_t3", "ndvi300_t1", "ndvi300_t2", "ndvi300_t3", 
"green_dist_t1", "green_dist_t2", "green_dist_t3", "blue_dist_t1", 
"blue_dist_t2", "blue_dist_t3"), 
  tables = c(
    "alspac/2_1_core_1_2/trimester",
    "2_1_core_trimester_rep_tcadman_2021-lc08", 
    "lc_eden_core_2_1.Project22_trimester_rep", 
    #"lc_isglobal_core_2_1.2_1_core_1_1_non_rep_210118_1", 
    "lc_genr_core_2_2.2_1_core_trimester_rep_TC_ECCNLC202053", 
    #"lc_moba_core_2_1.2_1_core_2021_1_non_rep_environment_depression", 
    #"lc_ninfea_core_2_1.p12_tcadman"
    )
)

## ---- Yearly-repeated --------------------------------------------------------
yearrep.vars <- list(
  variables = c(
    "child_id", "edu_m_", "areases_tert_", "areases_quint_", "fam_splitup", 
    "no2_", "pm25_", "lden_", "ndvi300_", "green_dist_", "blue_dist_", 
    "age_years", "cohab_", "bdens100_", "bdens300_", "urbgr_", "natgr_", 
    "agrgr_", "walkability_mean_", "landuseshan300_", "frichness300_", 
    "fdensity300_"), 
  tables = c(
    "alspac/2_1_core_1_3/yearly_rep", 
    #"lc_dnbc_core_2_1.2_1_core_yearly_rep_tcadman_2020-lc19", 
    "lc_eden_core_2_1.Project22_yearly_rep",
    "lc_isglobal_core_2_1.2_1_core_1_1_yearly_rep_210118_1", 
    "lc_genr_core_2_2.2_1_core_yearly_rep_TC_ECCNLC202053", 
    "lc_moba_core_2_1.2_1_core_2021_1_yearly_rep_environment_depression",
    "lc_ninfea_core_2_1.p12_tcadman_yearly_rep"))

################################################################################
# 2. Assign variables  
################################################################################

## ---- Non-repeated -----------------------------------------------------------
datashield.assign(
  symbol = "nonrep", 
  value = nonrep.vars$tables, 
  variables = nonrep.vars$variables, 
  conns = conns)

## ---- Trimester repeated -----------------------------------------------------

## ---- Yearly repeated --------------------------------------------------------
datashield.assign(
  symbol = "yearrep", 
  value = yearrep.vars$tables, 
  variables = yearrep.vars$variables, 
  conns = conns)
    
## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_1")
conns <- datashield.login(logindata, restore = "env_pnd_1")

################################################################################
# 3. Check availability  
################################################################################
available <- list(
  nonrep = dh.classDiscrepancy(
    df = "nonrep",
    vars = nonrep.vars$variables), 
  yearrep = dh.classDiscrepancy(
    df = "yearrep",
    vars = yearrep.vars$variables
  ))

available$nonrep %>% print(n = Inf)
available$yearrep %>% print(n = Inf)

################################################################################
# 4. Fill missing variables  
################################################################################
ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("yearrep", "yearrep")

## ---- Check ------------------------------------------------------------------
filled <- list(
  nonrep = dh.classDiscrepancy(
    df = "nonrep", 
    vars = nonrep.vars$variables), 
  yearrep = dh.classDiscrepancy(
    df = "yearrep", 
    vars = yearrep.vars$variables))

filled$nonrep %>% filter(discrepancy == "yes")
filled$yearrep %>% filter(discrepancy == "yes")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_2")
conns  <- datashield.login(logindata, restore = "env_pnd_2")

################################################################################
# 5. Fix factor variables   
################################################################################

## ---- Create variables with correct levels -----------------------------------

## Non-repeated
filled$nonrep %>% 
  filter(alspac == "factor") %>% 
  pull(variable) %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("nonrep$", .), 
      newobj.name = .))

## Yearly-repeated
filled$yearrep %>% 
  filter(alspac == "factor") %>% 
  pull(variable) %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("yearrep$", .), 
      newobj.name = .))

## ---- Remove original vars from dataframes -----------------------------------

## Non-repeated
dh.dropCols(
  df = "nonrep", 
  vars = filled$nonrep %>% filter(alspac == "factor") %>% pull(variable), 
  new_df_name = "nonrep", 
  comp_var = "child_id", 
  type = "remove", 
  conns = conns
)

## Yearly-repeated
dh.dropCols(
  df = "yearrep", 
  vars = filled$yearrep %>% filter(alspac == "factor") %>% pull(variable), 
  new_df_name = "yearrep", 
  comp_var = "child_id", 
  type = "remove", 
  conns = conns
)

## ---- Join correct variables back --------------------------------------------

## Non-repeated
ds.dataFrame(
  x = c(
    "nonrep", filled$nonrep %>% filter(alspac == "factor") %>% pull(variable)),
  newobj = "nonrep")

## Yearly-repeated
ds.dataFrame(
  x = c(
    "yearrep", filled$yearrep %>% filter(alspac == "factor") %>% pull(variable)),
  newobj = "yearrep")


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_3")
conns  <- datashield.login(logindata, restore = "env_pnd_3")


################################################################################
# 6. Create baseline variables from non repeated tables 
################################################################################

## ---- Parity -----------------------------------------------------------------

# We need to recode parity as a binary variable as there are issues later with 
# disclosive information when we run the models if we leave it ordinal.
ds.recodeValues(
  var.name = "nonrep$parity_m",
  values2replace.vector = c(0, 1, 2, 3, 4),
  new.values.vector = c(0, 1, 1, 1, 1),
  newobj = "parity_bin")

## ---- Combine these new variables with non-repeated dataframe ----------------
ds.dataFrame(
  x = c("nonrep", "parity_bin"),
  newobj = "nonrep")

dh.tidyEnv(
  obj = "parity_bin", 
  type = "remove", 
  conns = conns)


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_4")
conns <- datashield.login(logindata, restore = "env_pnd_4")


################################################################################
# 7. Create exposures for first year of birth
################################################################################

## ---- Subset to keep observations where child's age == 0 ---------------------
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "0",
  Boolean.operator = "==",
  newobj = "baseline_vars")

## ---- Convert to wide format -------------------------------------------------

# For the actual analysis we will want our dataset to be in wide format 
ds.reShape(
  data.name = "baseline_vars",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = c("edu_m_", "areases_tert_", "areases_quint_", "fam_splitup", 
              "no2_", "pm25_", "lden_", "ndvi300_", "green_dist_", "blue_dist_", 
              "age_years", "cohab_", "bdens100_", "bdens300_", "urbgr_", "natgr_", 
              "agrgr_", "walkability_mean_", "landuseshan300_", "frichness300_", 
              "fdensity300_"), 
  direction = "wide", 
  newobj = "baseline_wide")


## ---- Rename baseline_vars more sensible names -------------------------------
old_new <- tribble(
  ~oldvar, ~newvar,
  "edu_m_.0", "edu_m_1",
  "areases_tert_.0", "areases_tert_1",
  "areases_quint_.0", "areases_quint_1",
  "fam_splitup.0", "fam_splitup_1",
  "no2_.0", "no2_1",
  "pm25_.0", "pm25_1",
  "lden_.0", "lden_1",
  "ndvi300_.0", "ndvi300_1",
  "green_dist_.0", "green_dist_1",
  "blue_dist_.0", "blue_dist_1",
  "age_years.0", "age_years_1",
  "cohab_.0", "cohab_1",
  "bdens100_.0", "bdens100_1",
  "bdens300_.0", "bdens300_1",
  "urbgr_.0", "urbgr_1",
  "natgr_.0", "natgr_1",
  "agrgr_.0", "agrgr_1",
  "walkability_mean_.0", "walkability_mean_1",
  "landuseshan300_.0", "landuseshan300_1",
  "frichness300_.0", "frichness300_1",
  "fdensity300_.0", "fdensity300_1")    
  
dh.renameVars(
  df = "baseline_wide", 
  names = old_new, 
  conns = conns)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_5")
conns <- datashield.login(logindata, restore = "env_pnd_5")


################################################################################
# 8. Create cohort dummy  
################################################################################

## ---- Get cohort codes -------------------------------------------------------
coh_codes <- dh.getStats(
  df = "nonrep",
  vars = "cohort_id", 
  conns = conns
)

codes.tab <- coh_codes$categorical %>% filter(value != 0 & cohort != "combined") 

## ---- Make dummy variable ----------------------------------------------------
coh_dummy <- tibble(
  cohort = paste0(codes.tab$cohort, "_dummy"), 
  value = as.character(codes.tab$category))

coh_dummy %>%
  pmap(function(cohort, value){
    ds.Boole(
      V1 = "nonrep$cohort_id", 
      V2 = value,
      Boolean.operator = "==",
      numeric.output = TRUE, 
      na.assign = "NA", 
      newobj = cohort)
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_6")
conns <- datashield.login(logindata, restore = "env_pnd_6")


################################################################################
# 9. Merge datasets  
################################################################################

## ---- Non-repeated and first-year variables ----------------------------------
ds.merge(
  x.name = "nonrep",
  y.name = "baseline_wide",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "env_pnd"
)

## ---- Add the cohort dummy variables -----------------------------------------
ds.dataFrame(
  x = c('env_pnd', coh_dummy$cohort), 
  newobj = 'env_pnd'
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_7")
conns <- datashield.login(logindata, restore = "env_pnd_7")

################################################################################
# 10. Define valid cases: subjects with outcome and >= 1 exposure
################################################################################

# So when it comes to write up the analysis, we need to be able to specify an
# analysis dataset as a subset of all data, e.g. "contained all participants 
# with at least one exposure and outcome"


## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c(
  "areases_tert_1", "areases_quint_1", "fam_splitup_1", "no2_1", 
  "pm25_1", "lden_1", "ndvi300_1", "green_dist_1", "blue_dist_1", 
  "age_years_1", "cohab_1", "bdens100_1", "bdens300_1", "urbgr_1",
  "natgr_1", "agrgr_1", "walkability_mean_1", "landuseshan300_1",
  "frichness300_1", "fdensity300_1", "no2_preg", "pm25_preg", 
  "lden_preg", "ndvi300_preg", "green_dist_preg", "blue_dist_preg", 
  "bdens100_preg", "bdens300_preg", "fdensity300_preg", "frichness300_preg", 
  "landuseshan300_preg", "walkability_mean_preg", "agrgr_preg", "natgr_preg", 
  "urbgr_preg") 
  
out.vars <- "ppd"

cov.vars <- c(
  "areases_tert_preg", "areases_quint_preg", "parity_m", "sex", 
"birth_month", "eusilc_income_quintiles", "agebirth_m_y", "ethn1_m", 
"ethn2_m", "ethn3_m", "coh_country", "preg_smk", "preg_alc", "preg_cig", 
"preg_alc_unit", "breastfed_any", "breastfed_ever",  "cohort_id", 
"preg_dia", "preg_ht", "ga_bj", "prepreg_dep", "child_id", "child_no", 
"preg_no", "mother_id", "outcome", "con_anomalies", "birth_month", 
coh_dummy$cohort)


## ---- Now we create vars indicating whether any non-missing values are present
dh.subjHasData(
  df = "env_pnd", 
  vars = exp.vars, 
  new_label = "exposure", 
  conns = conns)

#dh.subjHasData(
#  df = "env_pnd", 
#  vars = out.vars, 
#  new_label = "outcome", 
#  conns = conns)

## ---- Next create another variable indicating whether a valid case -----------
#ds.make(
#  toAssign = "any_exposure+any_outcome", 
#  newobj = "n_exp_out")

#ds.Boole(
#  V1 = "n_exp_out", 
#  V2 = "2", 
#  Boolean.operator = "==", 
#  na.assign = 0, 
#  newobj = "valid_case")

#ds.summary("valid_case")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_8")
conns <- datashield.login(logindata, restore = "env_pnd_8")


################################################################################
# 11. Create analysis dataset  
################################################################################

## ---- Make a vector of all the variables we want to keep ---------------------
keep_vars <- c(exp.vars, out.vars, cov.vars)

## ---- Drop variables we don't need -------------------------------------------
var_index <- dh.findVarsIndex(
  df = "env_pnd", 
  vars = keep_vars, 
  conns = conns)

## ---- Subset based on valid cases and required variables ---------------------
var_index %>%
  imap(
    ~ds.dataFrameSubset(
      df.name = "env_pnd", 
      V1.name = "any_exposure", 
      V2.name = "1", 
      Boolean.operator = "==", 
      keep.cols = .x,
      keep.NAs = FALSE, 
      newobj = "analysis_df", 
      datasources = conns[.y]))

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_9")



