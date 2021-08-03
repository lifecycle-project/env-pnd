################################################################################
## Project: Urban environment and postnatal depression    
## Script purpose: Put analysis script together 
## Date: 25th May 21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(remotes)
install_github("lifecycle-project/dsHelper", ref = "features")
library(dsHelper)

################################################################################
# 1. Assign tables
################################################################################

## ---- Variable details -------------------------------------------------------
nonrep.vars <- c(
  "areases_tert_preg", "areases_quint_preg", "parity_m", "sex", 
  "birth_month", "eusilc_income_quintiles", "agebirth_m_y", "ethn1_m", 
  "ethn2_m", "ethn3_m", "coh_country", "preg_smk", "preg_alc", "preg_cig", 
  "preg_alc_unit", "breastfed_any", "breastfed_ever", "no2_preg", "pm25_preg", 
  "lden_preg", "ndvi300_preg", "green_dist_preg", "blue_dist_preg", "cohort_id", 
  "ppd", "preg_dia", "preg_ht", "ga_bj", "ga_us", "prepreg_dep", "child_id", 
  "child_no", "preg_no", "mother_id", "outcome", "con_anomalies", "birth_month", 
  "bdens100_preg", "bdens300_preg", "fdensity300_preg", "frichness300_preg", 
  "landuseshan300_preg", "walkability_mean_preg", "agrgr_preg", "natgr_preg", 
  "urbgr_preg", "urb_area_id", "lden_c_preg")

yearrep.vars <- c(
  "child_id", "edu_m_", "areases_tert_", "areases_quint_", "fam_splitup", 
  "no2_", "pm25_", "lden_", "ndvi300_", "green_dist_", "blue_dist_", 
  "age_years", "cohab_", "bdens100_", "bdens300_", "urbgr_", "natgr_", 
  "agrgr_", "walkability_mean_", "landuseshan300_", "frichness300_", 
  "fdensity300_", "lden_c_")

## ---- Table details ----------------------------------------------------------
cohorts_tables <- bind_rows(
  tibble(
    cohort = "alspac",
    table = c(
      "alspac/2_1_core_1_4/non_rep",
      "alspac/2_1_core_1_4/yearly_rep")),
  tibble(
    cohort = "bib",
    table = c(
      "sp455/2_2_core_1_3/non_rep",
      "sp455/2_2_core_1_3/yearly_rep")),
  tibble(
    cohort = "dnbc",
    table = c(
      "lc_dnbc_core_2_2.2_2_core_non_rep_tcadman_2021-lc08",
      "lc_dnbc_core_2_2.2_2_core_yearly_rep_tcadman_2021-lc08")),
  tibble(
    cohort = "eden_nan",
    table = c(
      "lc_eden_core_2_1.Project22_non_rep", 
      "lc_eden_core_2_1.Project22_yearly_rep")),
  tibble(
    cohort = "eden_poit",
    table = c(
      "lc_eden_core_2_1.Project22_non_rep", 
      "lc_eden_core_2_1.Project22_yearly_rep")),
  tibble(
    cohort = "inma_gip",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_210118_1", 
      "lc_isglobal_core_2_1.2_1_core_1_1_yearly_rep_210118_1")),
  tibble(
    cohort = "inma_sab",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_210118_1", 
      "lc_isglobal_core_2_1.2_1_core_1_1_yearly_rep_210118_1")),
  tibble(
    cohort = "inma_val",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_210118_1", 
      "lc_isglobal_core_2_1.2_1_core_1_1_yearly_rep_210118_1")),
  tibble(
    cohort = "genr",
    table = c(
      "lc_genr_core_2_2.2_1_core_non_rep_TC _ECCNLC202053", 
      "lc_genr_core_2_2.2_1_core_yearly_rep_TC_ECCNLC202053")),
  tibble(
    cohort = "moba",
    table = c(
      "lc_moba_core_2_1.2_1_core_2021_2_non_rep_environment_depression", 
      "lc_moba_core_2_1.2_1_core_2021_1_yearly_rep_environment_depression")),
  tibble(
    cohort = "ninfea",
    table = c(
      "lc_ninfea_core_2_1.p12_tcadman", 
      "lc_ninfea_core_2_1.p12_tcadman_yearly_rep")),
  tibble(
    cohort = "rhea",
    table = c(
      "lc_rhea_core_2_1.tcadman_nr", 
      "lc_rhea_core_2_1.tcadman_y"))) %>%
  mutate(type = rep(c("nonrep", "yearrep"), nrow(.)/2))

   
## ---- Assign tables ----------------------------------------------------------
cohorts_tables %>%
  dplyr::filter(cohort != "rhea") %>%
  pwalk(function(cohort, table, type){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })


## ---- Check this has worked --------------------------------------------------
ds.colnames("nonrep")
ds.colnames("yearrep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_1")
conns <- datashield.login(logindata, restore = "env_pnd_1")


################################################################################
# 2. Check availability  
################################################################################
available <- list(
  nonrep = dh.classDiscrepancy(
    df = "nonrep",
    vars = nonrep.vars), 
  yearrep = dh.classDiscrepancy(
    df = "yearrep",
    vars = yearrep.vars
  ))

available$nonrep %>% print(n = Inf)
available$yearrep %>% print(n = Inf)


################################################################################
# 3. Fill missing variables  
################################################################################
ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("yearrep", "yearrep")

## ---- Check ------------------------------------------------------------------
filled <- list(
  nonrep = dh.classDiscrepancy(
    df = "nonrep", 
    vars = nonrep.vars), 
  yearrep = dh.classDiscrepancy(
    df = "yearrep", 
    vars = yearrep.vars))

filled$nonrep %>% dplyr::filter(discrepancy == "yes")
filled$yearrep %>% dplyr::filter(discrepancy == "yes")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_2")
conns  <- datashield.login(logindata, restore = "env_pnd_2")


################################################################################
# 4. Remove lden for cohorts it shouldn't exist for  
################################################################################
wrong_noise <- c("alspac", "bib", "inma_gip", "inma_sab", "inma_val")

dh.dropCols(
  df = "yearrep", 
  vars = "lden_", 
  type = "remove", 
  conns = conns[wrong_noise]
)

length_ref <- tibble(
  cohort = wrong_noise,
  length = ds.dim(
    x = "yearrep",
    type = "split", 
    datasources = conns[wrong_noise]) %>%
    map(~.[[1]]) %>%
    unlist %>%
    as.integer)

length_ref %>%
  pmap(function(cohort, length){
    ds.rep(
      x1 = NA, 
      times = length,
      source.times = "c",
      newobj = "lden_", 
      datasources = conns[cohort])
    })

ds.dataFrame(
  x = c("yearrep", "lden_"),
  newobj = "yearrep", 
  datasources = conns[wrong_noise]
)


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_3")
conns  <- datashield.login(logindata, restore = "env_pnd_3")

################################################################################
# 5. Fix factor variables   
################################################################################

## ---- Create variables with correct levels -----------------------------------

## Non-repeated
filled$nonrep %>% 
  dplyr::filter(dnbc == "factor") %>% 
  pull(variable) %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("nonrep$", .), 
      newobj.name = .))

## Yearly-repeated
filled$yearrep %>% 
  dplyr::filter(dnbc == "factor") %>% 
  pull(variable) %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("yearrep$", .), 
      newobj.name = .))

## ---- Remove original vars from dataframes -----------------------------------

## Non-repeated
dh.dropCols(
  df = "nonrep", 
  vars = filled$nonrep %>% dplyr::filter(dnbc == "factor") %>% pull(variable), 
  type = "remove")

## Yearly-repeated
dh.dropCols(
  df = "yearrep", 
  vars = filled$yearrep %>% dplyr::filter(dnbc == "factor") %>% pull(variable), 
  type = "remove")


## ---- Join correct variables back --------------------------------------------

## Non-repeated
ds.dataFrame(
  x = c(
    "nonrep", filled$nonrep %>% dplyr::filter(dnbc == "factor") %>% pull(variable)),
  newobj = "nonrep")

## Yearly-repeated
ds.dataFrame(
  x = c(
    "yearrep", filled$yearrep %>% dplyr::filter(dnbc == "factor") %>% pull(variable)),
  newobj = "yearrep")


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_4")
conns  <- datashield.login(logindata, restore = "env_pnd_4")


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
datashield.workspace_save(conns, "env_pnd_5")
conns <- datashield.login(logindata, restore = "env_pnd_5")


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
              "fdensity300_", "lden_c_"), 
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
  "fdensity300_.0", "fdensity300_1",
  "lden_c_.0", "lden_c_1")    
  
dh.renameVars(
  df = "baseline_wide", 
  names = old_new, 
  conns = conns)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_6")
conns <- datashield.login(logindata, restore = "env_pnd_6")

################################################################################
# 8. Merge datasets  
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

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_7")
conns <- datashield.login(logindata, restore = "env_pnd_7")

################################################################################
# 9. Create sub-cohorts for inma and eden  
################################################################################
sub_coh <- c("inma_gip", "inma_sab", "inma_val", "eden_nan", "eden_poit")


tibble(
  cohort = sub_coh,
  value = c("1102", "1103", "1104", "1801", "1802")) %>%
  pmap(function(cohort, value){
    
    ds.dataFrameSubset(
      df.name = "env_pnd", 
      V1.name = "env_pnd$urb_area_id",
      V2.name = value,
      Boolean.operator = "==",
      newobj = "env_pnd", 
      datasources = conns[cohort])
    
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_8")
conns <- datashield.login(logindata, restore = "env_pnd_8")

################################################################################
# 10. Create cohort dummy variables  
################################################################################

## ---- Get cohort codes -------------------------------------------------------
coh_codes <- dh.getStats(
  df = "env_pnd",
  vars = "cohort_id", 
  conns = conns
)

coh_codes.tab <- coh_codes$categorical %>% 
  dplyr::filter(value != 0 & !cohort %in% c(sub_coh, "combined")) %>%
  mutate(ref_var = "cohort_id") %>%
  select(category, cohort, ref_var)

## ---- Get urban ID codes -----------------------------------------------------
urb_codes <- dh.getStats(
  df = "env_pnd",
  vars = "urb_area_id", 
  conns = conns
)

urb_codes.tab <- urb_codes$categorical %>% 
  dplyr::filter(value != 0 & cohort %in% sub_coh) %>%
  mutate(ref_var = "urb_area_id") %>%
  select(category, cohort, ref_var)

ref_codes <- bind_rows(coh_codes.tab, urb_codes.tab) %>%
  mutate(
    dummy = paste0(cohort, "_d"), 
    value = as.character(category)) %>%
  select(dummy, value, ref_var)

## ---- Make dummy variable ----------------------------------------------------
ref_codes %>%
  pmap(function(dummy, value, ref_var){
    ds.Boole(
      V1 = paste0("env_pnd$", ref_var), 
      V2 = value,
      Boolean.operator = "==",
      numeric.output = TRUE, 
      na.assign = 0, 
      newobj = dummy)
  })

## ---- Add the cohort dummy variables -----------------------------------------
ds.dataFrame(
  x = c('env_pnd', ref_codes$dummy), 
  newobj = 'env_pnd'
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_9")
conns <- datashield.login(logindata, restore = "env_pnd_9")

################################################################################
# 11. Define valid cases: subjects with outcome and >= 1 exposure
################################################################################

# So when it comes to write up the analysis, we need to be able to specify an
# analysis dataset as a subset of all data, e.g. "contained all participants 
# with at least one exposure and outcome"


## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c(
  "areases_tert_1", "areases_quint_1", "fam_splitup_1", "no2_1", 
  "pm25_1", "lden_1", "ndvi300_1", "green_dist_1", "blue_dist_1", 
  "bdens300_1", "urbgr_1", "natgr_1", "agrgr_1", "walkability_mean_1",
  "landuseshan300_1", "frichness300_1", "fdensity300_1", "no2_preg",
  "pm25_preg", "lden_preg", "ndvi300_preg", "green_dist_preg", "blue_dist_preg", 
  "bdens300_preg", "fdensity300_preg", "frichness300_preg", 
  "landuseshan300_preg", "walkability_mean_preg", "agrgr_preg", "natgr_preg", 
  "urbgr_preg", "lden_c_preg", "lden_c_1") 
  
out.vars <- "ppd"

cov.vars <- c(
  "areases_tert_preg", "areases_quint_preg", "parity_m", "sex", 
"birth_month", "eusilc_income_quintiles", "agebirth_m_y", "ethn1_m", 
"ethn2_m", "ethn3_m", "coh_country", "preg_smk", "preg_alc", "preg_cig", 
"preg_alc_unit", "breastfed_any", "breastfed_ever",  "cohort_id", 
"preg_dia", "preg_ht", "ga_bj", "prepreg_dep", "child_id", "child_no", 
"preg_no", "mother_id", "outcome", "con_anomalies", "birth_month", 
"age_years_1", "cohab_1", ref_codes$dummy, "urb_area_id")


## ---- Now we create vars indicating whether any non-missing values are present
dh.defineCases(
  df = "env_pnd", 
  vars = exp.vars, 
  type = "any"
)

ds.table("dc_any_data")

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
datashield.workspace_save(conns, "env_pnd_10")
conns <- datashield.login(logindata, restore = "env_pnd_10")


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
      V1.name = "dc_any_data", 
      V2.name = "1", 
      Boolean.operator = "==", 
      keep.cols = .x,
      keep.NAs = FALSE, 
      newobj = "analysis_df", 
      datasources = conns[.y]))

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_9")
conns <- datashield.login(logindata, restore = "env_pnd_9")


################################################################################
# 12. Create IQR versions of variables  
################################################################################
dh.makeIQR(
  df = "env_pnd", 
  vars = "no2_preg", 
  type = "pooled"
)

dh.makeIQR(
  df = "env_pnd", 
  vars = "no2_preg", 
  type = "separate"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_10")
conns <- datashield.login(logindata, restore = "env_pnd_10")


test_pool <- dh.getStats(df = "env_pnd", vars = "no2_preg_iqr_c")
test_sep <- dh.getStats(df = "env_pnd", vars = "no2_preg_iqr_p")


