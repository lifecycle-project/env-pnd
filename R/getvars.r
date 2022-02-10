################################################################################
## Project: Urban environment and postnatal depression    
## Script purpose: Put analysis script together 
## Date: 25th May 21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(remotes)
install_github("timcadman/datashield-tim", lib = "~/R/userlib")
install_github("lifecycle-project/ds-helper")
library(dsHelper)

################################################################################
# 1. Assign tables
################################################################################

## ---- Variable details -------------------------------------------------------
nonrep.vars <- c(
  "areases_tert_preg", "areases_quint_preg", "parity_m", "sex", 
  "birth_month", "birth_year", "eusilc_income_quintiles", "agebirth_m_y", 
  "ethn1_m", "ethn2_m", "ethn3_m", "coh_country", "preg_smk", "preg_alc", 
  "preg_cig", "preg_alc_unit", "breastfed_any", "breastfed_ever", "no2_preg", 
  "pm25_preg", "lden_preg", "ndvi300_preg", "green_dist_preg", "blue_dist_preg", 
  "cohort_id", "ppd", "preg_dia", "preg_ht", "ga_bj", "ga_us", "prepreg_dep", 
  "prepreg_anx", "prepreg_psych", "preg_psych", "child_id", "child_no", 
  "preg_no", "mother_id", "outcome", "con_anomalies", "bdens100_preg", 
  "bdens300_preg", "fdensity300_preg", "frichness300_preg", 
  "landuseshan300_preg", "walkability_mean_preg", "agrgr_preg", "natgr_preg", 
  "urbgr_preg", "urb_area_id", "lden_c_preg", "greenyn300_preg", 
  "blueyn300_preg", "popdens_preg", "pm10_preg")

yearrep.vars <- c(
  "child_id", "edu_m_", "areases_tert_", "areases_quint_", "fam_splitup", 
  "no2_", "pm25_", "lden_", "ndvi300_", "green_dist_", "blue_dist_", 
  "age_years", "cohab_", "bdens100_", "bdens300_", "urbgr_", "natgr_", 
  "agrgr_", "walkability_mean_", "landuseshan300_", "frichness300_", 
  "fdensity300_", "lden_c_", "greenyn300_", "blueyn300_", "popdens_", "pm10_")

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
      "sp455/2_2_core_1_4/non_rep",
      "sp455/2_2_core_1_4/yearly_rep")),
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
    cohort = "genr",
    table = c(
      "lc_genr_core_2_2.2_1_core_non_rep_TC _ECCNLC202053", 
      "lc_genr_core_2_2.2_1_core_yearly_rep_TC_ECCNLC202053")),
  tibble(
    cohort = "moba",
    table = c(
      "lc_moba_core_2_1.2_1_core_2021_7_non_rep_urban_environment_postnatal_depression", 
      "lc_moba_core_2_1.2_1_core_2021_7_yearly_rep_urban_environment_postnatal_depression")),
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

save.image()

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
wrong_noise <- c("alspac", "bib", "inma_gip", "inma_sab")

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
# 6. Recode parity as binary
################################################################################
ds.asNumeric("nonrep$parity", "parity")

ds.Boole(
  V1 = "parity",
  V2 = 1,
  Boolean.operator = ">",
  newobj = "parity_bin"
)

ds.asFactor("parity_bin", "parity_bin")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_5")
conns <- datashield.login(logindata, restore = "env_pnd_5")

################################################################################
# 7. Recode birth month
################################################################################
ds.asNumeric("nonrep$birth_month", "birth_month")

season_ref <- tibble(
  old_val = seq(1, 12),
  new_val = c(
    rep("spring", 3),
    rep("summer", 3),
    rep("autumn", 3), 
    rep("winter", 3))
)

season_ref %>% 
  pmap(function(old_val, new_val){
    
    ds.recodeValues(
      var.name = "birth_month",
      values2replace.vector = old_val,
      new.values.vector = new_val,
      newobj = "birth_month")
  })

ds.asFactor("birth_month", "birth_month_f")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_6")
conns <- datashield.login(logindata, restore = "env_pnd_6")

################################################################################
# 8. Recode birth year
################################################################################
ds.asNumeric("nonrep$birth_year", "birth_year_c")

year_ref <- tibble(
  old_val = c(
    1991, 1992, 1993, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
    2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017),
  new_val = c(
    "91_95", "91_95", "91_95", "96_00", "96_00", "96_00", "01_05", "01_05", 
    "01_05", "01_05", "01_05", "05_11", "05_11", "05_11", "05_11", "05_11", 
    "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", "05_11")
)

year_ref %>% 
  pmap(function(old_val, new_val){
    
    ds.recodeValues(
      var.name = "birth_year_c",
      values2replace.vector = old_val,
      new.values.vector = new_val,
      newobj = "birth_year_c", 
      datasources = conns[names(conns) != "rhea"])
    
  })

ds.asFactor("birth_year_c", "birth_year_f")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_7")
conns <- datashield.login(logindata, restore = "env_pnd_7")

################################################################################
# 9. Recode maternal age at birth
################################################################################
ds.asNumeric("nonrep$agebirth_m_y", "mat_age")

mat_age_ref <- tibble(
  old_val = c(
    seq(16, 20, 1),
    seq(21, 25, 1), 
    seq(26, 30, 1), 
    seq(31, 35, 1), 
    seq(36, 40, 1), 
    c(seq(41, 50, 1), 55)),
  new_val = c(
    rep("16_20", 5), 
    rep("21_25", 5), 
    rep("26_30", 5), 
    rep("31_35", 5),
    rep("36_40", 5),
    rep("41_50", 11)
  )
)

mat_age_ref %>% 
  pmap(function(old_val, new_val){
    
    ds.recodeValues(
      var.name = "mat_age",
      values2replace.vector = old_val,
      new.values.vector = new_val,
      newobj = "mat_age")
    
  })

ds.asFactor("mat_age", "mat_age_f")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_8")
conns <- datashield.login(logindata, restore = "env_pnd_8")

################################################################################
# 10. Create preterm birth variable
################################################################################
ds.assign(
  toAssign = "nonrep$ga_bj", 
  newobj = "ga_all",
  datasources = conns[!conns == "moba"]
) 

ds.assign(
  toAssign = "nonrep$ga_us", 
  newobj = "ga_all",
  datasources = conns["moba"]
)

ds.Boole(
  V1 = "ga_all",
  V2 = 37*7,
  Boolean.operator = ">",
  newobj = "ga_bin"
)

ds.asFactor("ga_bin", "preterm")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_9")
conns <- datashield.login(logindata, restore = "env_pnd_9")

################################################################################
# 11. Join this back into nonrep dataframe
################################################################################
ds.dataFrame(
  x = c("nonrep", "parity_bin", "birth_month_f", "birth_year_f", "mat_age_f", 
        "preterm"), 
  newobj = "nonrep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_10")
conns <- datashield.login(logindata, restore = "env_pnd_10")

################################################################################
# 12. Collapse top two levels of lden  
################################################################################
ds.recodeLevels(
  x = "nonrep$lden_c_preg", 
  newCategories = c(1, 2, 3, 4, 5, 5), 
  newobj = "lden_preg_f"
)

ds.dataFrame(
  x = c("nonrep", "lden_preg_f"), 
  newobj = "nonrep"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_11")
conns <- datashield.login(logindata, restore = "env_pnd_11")

################################################################################
# 13. Create exposures for 0 - 12 months
################################################################################

## ---- Subset to keep observations where child's age == 1 ---------------------
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "exp_0_12_tmp")

## ---- Convert to wide format -------------------------------------------------
ds.reShape(
  data.name = "exp_0_12_tmp",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = c("areases_tert_", "no2_", "pm25_", "lden_", "ndvi300_", 
              "green_dist_", "blue_dist_", "age_years", "bdens100_", 
              "bdens300_", "urbgr_", "natgr_", "agrgr_", "walkability_mean_", 
              "landuseshan300_", "frichness300_", "fdensity300_", "lden_c_", 
              "greenyn300_", "blueyn300_", "popdens_", "pm10_", "cohab_"), 
  direction = "wide", 
  drop = "edu_m_",
  newobj = "exp_0_12")

## ---- Rename -----------------------------------------------------------------
exp_rename <- tribble(
  ~oldvar, ~newvar,
  "areases_tert_.1", "areases_tert_1",
  "no2_.1", "no2_1",
  "pm25_.1", "pm25_1",
  "lden_.1", "lden_1",
  "ndvi300_.1", "ndvi300_1",
  "green_dist_.1", "green_dist_1",
  "blue_dist_.1", "blue_dist_1",
  "age_years.1", "age_years_1",
  "bdens100_.1", "bdens100_1",
  "bdens300_.1", "bdens300_1",
  "urbgr_.1", "urbgr_1",
  "natgr_.1", "natgr_1",
  "agrgr_.1", "agrgr_1",
  "walkability_mean_.1", "walkability_mean_1",
  "landuseshan300_.1", "landuseshan300_1",
  "frichness300_.1", "frichness300_1",
  "fdensity300_.1", "fdensity300_1",
  "lden_c_.1", "lden_c_1", 
  "greenyn300_.1", "greenyn300_1",
  "blueyn300_.1", "blueyn300_1",
  "popdens_.1", "popdens_1",
  "pm10_.1", "pm10_1", 
  "cohab_.1", "cohab")    

dh.renameVars(
  df = "exp_0_12", 
  current_names = exp_rename$oldvar,
  new_names = exp_rename$newvar,
  conns = conns)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_12")
conns <- datashield.login(logindata, restore = "env_pnd_12")

################################################################################
# 14. Create exposure for maternal education at birth  
################################################################################

## ---- Subset to keep observations where child's age == 0 ---------------------
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "0",
  Boolean.operator = "==",
  newobj = "mat_ed_tmp")

## ---- Convert to wide format -------------------------------------------------
ds.reShape(
  data.name = "mat_ed_tmp",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = "edu_m_", 
  direction = "wide", 
  newobj = "mat_ed")

dh.dropCols(
  df = "mat_ed", 
  vars = c("child_id", "edu_m_.0"),
  type = "keep"
)

## ---- Rename -----------------------------------------------------------------
dh.renameVars(
  df = "mat_ed", 
  current_names = "edu_m_.0",
  new_names = "edu_m_0",
  conns = conns)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_13")
conns <- datashield.login(logindata, restore = "env_pnd_13")

################################################################################
# 15. Merge datasets  
################################################################################

## ---- Non-repeated and first-year variables ----------------------------------
ds.merge(
  x.name = "nonrep",
  y.name = "exp_0_12",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "env_pnd"
)

## ---- Merge in education variable --------------------------------------------
ds.merge(
  x.name = "env_pnd",
  y.name = "mat_ed",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "env_pnd"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_14")
conns <- datashield.login(logindata, restore = "env_pnd_14")

################################################################################
# 16. Create combined area deprivation variable  
################################################################################

# MoBa only has it in the first year of birth.
ds.assign(
  toAssign = "env_pnd$areases_tert_preg", 
  newobj = "areases_tert",
  datasources = conns[names(conns) != "moba"]
) 

ds.assign(
  toAssign = "env_pnd$areases_tert_1", 
  newobj = "areases_tert",
  datasources = conns["moba"]
)

## ---- Join back in -----------------------------------------------------------
ds.dataFrame(
  x = c("env_pnd", "areases_tert"), 
  newobj = "env_pnd")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_15")
conns <- datashield.login(logindata, restore = "env_pnd_15")

################################################################################
# 17. Create sub-cohorts for inma and eden  
################################################################################
sub_coh <- c("inma_gip", "inma_sab", "eden_nan", "eden_poit")

tibble(
  cohort = sub_coh,
  value = c("1102", "1103", "1801", "1802")) %>%
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
datashield.workspace_save(conns, "env_pnd_16")
conns <- datashield.login(logindata, restore = "env_pnd_16")

################################################################################
# 18. Create cohort dummy variables  
################################################################################

## ---- Get cohort codes -------------------------------------------------------
coh_codes <- dh.getStats(
  df = "env_pnd",
  vars = "cohort_id", 
  conns = conns)

coh_codes.tab <- coh_codes$categorical %>% 
  dplyr::filter(value != 0 & !cohort %in% c(sub_coh, "combined")) %>%
  mutate(ref_var = "cohort_id") %>%
  dplyr::select(category, cohort, ref_var)

## ---- Get urban ID codes -----------------------------------------------------
urb_codes <- dh.getStats(
  df = "env_pnd",
  vars = "urb_area_id", 
  conns = conns)

urb_codes.tab <- urb_codes$categorical %>% 
  dplyr::filter(value != 0 & cohort %in% sub_coh) %>%
  mutate(ref_var = "urb_area_id") %>%
  dplyr::select(category, cohort, ref_var)

ref_codes <- bind_rows(coh_codes.tab, urb_codes.tab) %>%
  mutate(
    dummy = paste0(cohort, "_d"), 
    value = as.character(category)) %>%
  dplyr::select(dummy, value, ref_var)

save.image("~/env-pnd/4-feb-22.RData")
## ---- Make dummy variable ----------------------------------------------------
ref_codes %>%
  pmap(function(variable, dummy, value, ref_var){
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
datashield.workspace_save(conns, "env_pnd_17")
conns <- datashield.login(logindata, restore = "env_pnd_17")

################################################################################
# 19. Set reference levels of binary variables to 0  
################################################################################
refvars <- c("greenyn300_preg", "blueyn300_preg")

refvars %>%
  map(
    ~ds.changeRefGroup(
      x = paste0("env_pnd$", .),
      ref = "0",
      newobj = .,
      reorderByRef = FALSE)
  )

dh.dropCols(
  df = "env_pnd", 
  vars = refvars,
  type = "remove"
)

ds.dataFrame(
  x = c("env_pnd", refvars), 
  newobj = "env_pnd"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_18")
conns <- datashield.login(logindata, restore = "env_pnd_18")

################################################################################
# 20. Define baseline dataset for which urban exposures were estimated
################################################################################

## ---- Identify non-missing cases ---------------------------------------------
dh.defineCases(
  df = "env_pnd", 
  vars = "urb_area_id", 
  type = "any", 
  new_obj = "baseline_valid"
)

## ---- Set Rhea to 1 ----------------------------------------------------------
ds.assign(
  toAssign = "baseline_valid+1", 
  newobj = "baseline_valid", 
  datasources = conns["rhea"]
)

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "env_pnd", 
  V1.name = "baseline_valid", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "baseline_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_19")
conns <- datashield.login(logindata, restore = "env_pnd_19")

################################################################################
# 21. Define valid cases: subjects with outcome and >= 1 exposure
################################################################################

## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c(
  "ndvi300_preg", "greenyn300_preg", "blueyn300_preg", "no2_preg", "pm25_preg", 
  "pm10_preg",  "lden_preg_f",  "frichness300_preg", "walkability_mean_preg", 
  "popdens_preg")

out.vars <- "ppd"

## ---- Now we create vars indicating whether any non-missing values are present
dh.defineCases(
  df = "baseline_df", 
  vars = exp.vars, 
  type = "any", 
  new_obj = "any_exp"
)

dh.defineCases(
  df = "baseline_df", 
  vars = out.vars, 
  type = "any", 
  new_obj = "any_out"
)

ds.make(
  toAssign = "any_exp+any_out", 
  newobj = "n_complete")

ds.Boole(
  V1 = "n_complete", 
  V2 = "2", 
  Boolean.operator = "==", 
  na.assign = 0, 
  newobj = "some_vars")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_20")
conns <- datashield.login(logindata, restore = "env_pnd_20")

################################################################################
# 22. Identify singletons & live births
################################################################################

## ---- Set MoBa outcome as 1 --------------------------------------------------
ds.rep(
  x1 = 1,
  times = ds.dim("baseline_df", datasources = conns["moba"])[[1]][[1]] ,
  source.x1 = "clientside",
  source.times = "c",
  newobj = "outcome",
  datasources = conns["moba"])

ds.make(
  toAssign = "baseline_df$outcome",
  newobj = "outcome",
  datasources = conns[!names(conns) == c("moba")])

ds.asFactor("outcome", "outcome")

dh.dropCols(
  df = "baseline_df",
  vars = "outcome",
  type = "remove",
  conns = conns["moba"])

ds.dataFrame(
  x = c("baseline_df", "outcome"),
  newobj = "baseline_df",
  datasources = conns["moba"])

## ---- Set all GEN-R child_no as 1 --------------------------------------------
ds.rep(
  x1 = 1,
  times = ds.dim("baseline_df", datasources = conns["genr"])[[1]][[1]],
  source.x1 = "clientside",
  source.times = "c",
  newobj = "child_no",
  datasources = conns[c("genr", "moba")])

ds.asInteger("child_no", "child_no", datasources = conns["genr"])

dh.dropCols(
  df = "baseline_df",
  vars = "child_no",
  type = "remove",
  conns = conns["genr"])

ds.dataFrame(
  x = c("baseline_df", "child_no"),
  newobj = "baseline_df",
  datasources = conns["genr"])

## ---- Define cases meeting exclusion criteria ---------------------------------
ds.Boole(
  V1 = "baseline_df$outcome",
  V2 = 1,
  Boolean.operator = "==",
  newobj = "outcome_valid")

ds.Boole(
  V1 = "baseline_df$child_no",
  V2 = 1,
  Boolean.operator = "==",
  newobj = "child_no_valid")

ds.make(
  toAssign = "outcome_valid*child_no_valid",
  newobj = "exclusions_valid")

## ---- Define final valid cases -----------------------------------------------
ds.make(
  toAssign = "exclusions_valid*some_vars",
  newobj = "valid")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_21")
conns <- datashield.login(logindata, restore = "env_pnd_21")

################################################################################
# 23. Create analysis dataset  
################################################################################

## ---- Drop variables we don't need -------------------------------------------
var_index <- dh.findVarsIndex(
  df = "baseline_df", 
  vars = c(exp.vars, out.vars, cov.vars, other.vars), 
  conns = conns)

## ---- Subset based on valid cases and required variables ---------------------
var_index %>%
  imap(
    ~ds.dataFrameSubset(
      df.name = "baseline_df", 
      V1.name = "valid", 
      V2.name = "1", 
      Boolean.operator = "==", 
      keep.cols = .x,
      keep.NAs = FALSE, 
      newobj = "analysis_df", 
      datasources = conns[.y]))

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_22")
conns <- datashield.login(logindata, restore = "env_pnd_22")

################################################################################
# 24. Create IQR versions of variables  
################################################################################
iqr.vars <- bind_rows(exp_preg.ref, exp_year_1.ref) %>%
  dplyr::filter(type == "cont") %>%
  pull(variable)

dh.makeIQR(
  df = "analysis_df", 
  vars = iqr.vars, 
  type = "split")

dh.makeIQR(
  df = "analysis_df", 
  vars = iqr.vars, 
  type = "combine")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_23")
conns <- datashield.login(logindata, restore = "env_pnd_23")

################################################################################
# 25. Create excluded participants dataset
################################################################################
ds.dataFrameSubset(
  df.name = "baseline_df", 
  V1.name = "valid", 
  V2.name = "0", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "excluded_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_24")
conns <- datashield.login(logindata, restore = "env_pnd_24")




################################################################################
# 18. Create quartiles of continuous variables
################################################################################

## ---- NDVI -------------------------------------------------------------------

## Combined
dh.quartileSplit(
  df = "analysis_df",
  var = "ndvi300_preg_iqr_p",
  type = "combine",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## Split
dh.quartileSplit(
  df = "analysis_df",
  var = "ndvi300_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_s_")

## ---- NO2 --------------------------------------------------------------------

## Combined
dh.quartileSplit(
  df = "analysis_df",
  var = "no2_preg_iqr_p",
  type = "combine",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## Split
dh.quartileSplit(
  df = "analysis_df",
  var = "no2_preg_iqr_c",
  type = "combine",
  band_action = "ge_l",
  var_suffix = "_q_s_")
## ---- PM2.5 ------------------------------------------------------------------

## Combined

## Split

## ---- PM10 -------------------------------------------------------------------

## Combined

## Split

## ---- Facility richness  -----------------------------------------------------

## Combined

## Split

## ---- Walkability ------------------------------------------------------------

## Combined

## Split

## ---- Population density -----------------------------------------------------

## Combined

## Split








