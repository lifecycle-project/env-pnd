################################################################################
## Project: Urban environment and postnatal depression    
## Script purpose: Put analysis script together 
## Date: 25th May 21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Assign tables
################################################################################

## ---- Variable details -------------------------------------------------------
nonrep.vars <- c(
  "areases_tert_preg", "areases_quint_preg", "parity_m", "sex", 
  "birth_month", "birth_year", "eusilc_income_quintiles", "agebirth_m_y", 
  "ethn1_m", "ethn2_m", "ethn3_m", "coh_country", "preg_smk", "preg_alc", 
  "preg_cig", "preg_alc_unit", "breastfed_any", "breastfed_ever", "no2_preg", 
  "pm25_preg", "lden_preg", "ndvi100_preg", "ndvi300_preg", "ndvi500_preg", 
  "green_dist_preg", "blue_dist_preg", "cohort_id", "ppd", "preg_dia", 
  "preg_ht", "ga_bj", "ga_us", "prepreg_dep", "prepreg_anx", "prepreg_psych", 
  "preg_psych", "child_id", "child_no", "preg_no", "mother_id", "outcome", 
  "con_anomalies", "bdens100_preg", "bdens300_preg", "fdensity300_preg", 
  "frichness300_preg", "landuseshan300_preg", "walkability_mean_preg", 
  "agrgr_preg", "natgr_preg", "urbgr_preg", "urb_area_id", "lden_c_preg", 
  "greenyn300_preg", "blueyn300_preg", "popdens_preg", "pm10_preg")

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
      "project22-eden/2_1_core_1_0/non_rep", 
      "project22-eden/2_1_core_1_0/yearly_rep")),
  tibble(
    cohort = "eden_poit",
    table = c(
      "project22-eden/2_1_core_1_0/non_rep", 
      "project22-eden/2_1_core_1_0/yearly_rep")),
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
  dplyr::filter(cohort %in% c("eden_nan", "eden_poit")) %>%
  pwalk(function(cohort, table, type){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })

## ---- Check this has worked --------------------------------------------------
ds.colnames("nonrep", datasources = conns["eden_nan"])
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_2")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_3")
conns  <- datashield.login(logindata, restore = "env_pnd_3")

################################################################################
# 5. Fix factor variables   
################################################################################

## ---- Create variables with correct levels -----------------------------------

## Non-repeated
filled$nonrep %>% 
  dplyr::filter(eden_nan == "factor") %>% 
  pull(variable) %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("nonrep$", .), 
      newobj.name = .))

## Yearly-repeated
filled$yearrep %>% 
  dplyr::filter(eden_nan == "factor") %>% 
  pull(variable) %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("yearrep$", .), 
      newobj.name = .))

## ---- Remove original vars from dataframes -----------------------------------

## Non-repeated
dh.dropCols(
  df = "nonrep", 
  vars = filled$nonrep %>% dplyr::filter(eden_nan == "factor") %>% pull(variable), 
  type = "remove")

## Yearly-repeated
dh.dropCols(
  df = "yearrep", 
  vars = filled$yearrep %>% dplyr::filter(eden_nan == "factor") %>% pull(variable), 
  type = "remove")


## ---- Join correct variables back --------------------------------------------

## Non-repeated
ds.dataFrame(
  x = c(
    "nonrep", filled$nonrep %>% dplyr::filter(eden_nan == "factor") %>% pull(variable)),
  newobj = "nonrep")

## Yearly-repeated
ds.dataFrame(
  x = c(
    "yearrep", filled$yearrep %>% dplyr::filter(eden_nan == "factor") %>% pull(variable)),
  newobj = "yearrep")


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_4")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_5")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_6")
conns <- datashield.login(logindata, restore = "env_pnd_6")

################################################################################
# 8. Recode birth year
################################################################################
ds.asNumeric("nonrep$birth_year", "birth_year_c")

ds.table("birth_year_c")

year_ref <- tibble(
  old_val = c(
    1990, 1991, 1992, 1993, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
    2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017),
  new_val = c(
    "90_95", "90_95", "90_95", "90_95", "96_00", "96_00", "96_00", "96_00", 
    "96_00", "01_05", "01_05", "01_05", "01_05", "01_05", "05_11", "05_11", 
    "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", "05_11", 
    "05_11", "05_11")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_7")
conns <- datashield.login(logindata, restore = "env_pnd_7")

################################################################################
# 9. Recode maternal age at birth
################################################################################
ds.asNumeric("nonrep$agebirth_m_y", "mat_age")

mat_age_ref <- tibble(
  old_val = c(
    seq(15, 20, 1),
    seq(21, 25, 1), 
    seq(26, 30, 1), 
    seq(31, 35, 1), 
    seq(36, 40, 1), 
    c(seq(41, 50, 1), 55)),
  new_val = c(
    rep("15_20", 6), 
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_8")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_9")
conns <- datashield.login(logindata, restore = "env_pnd_9")

################################################################################
# 11. Join this back into nonrep dataframe
################################################################################
ds.dataFrame(
  x = c("nonrep", "parity_bin", "birth_month_f", "birth_year_f", "mat_age_f", 
        "preterm"), 
  newobj = "nonrep")


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_10")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_11")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_12")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_13")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_14")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_15")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_16")
conns <- datashield.login(logindata, restore = "env_pnd_16")

################################################################################
# 18. Create cohort dummy variables  
################################################################################
ds.dataFrameFill("env_pnd", "env_pnd")
ds.asFactor("env_pnd$cohort_id", "cohort_id")
dh.dropCols(df = "env_pnd", vars = "cohort_id", type = "remove")
ds.dataFrame(x = c("env_pnd", "cohort_id"), newobj = "env_pnd")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_16a")

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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_17")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_18")
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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_19")
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

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "baseline_df", 
  V1.name = "some_vars", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "some_exp_out_df")

ds.dim("some_exp_out_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_20")
conns <- datashield.login(logindata, restore = "env_pnd_20")

################################################################################
# 22. Restrict to live births
################################################################################

## ---- Set MoBa outcome as 1 --------------------------------------------------
ds.rep(
  x1 = 1,
  times = ds.dim("some_exp_out_df", datasources = conns["moba"])[[1]][[1]] ,
  source.x1 = "clientside",
  source.times = "c",
  newobj = "outcome",
  datasources = conns["moba"])

ds.make(
  toAssign = "some_exp_out_df$outcome",
  newobj = "outcome",
  datasources = conns[!names(conns) == c("moba")])

ds.asFactor("outcome", "outcome")

dh.dropCols(
  df = "some_exp_out_df",
  vars = "outcome",
  type = "remove",
  conns = conns["moba"])

ds.dataFrame(
  x = c("some_exp_out_df", "outcome"),
  newobj = "some_exp_out_df",
  datasources = conns["moba"])

## ---- Define cases meeting exclusion criteria --------------------------------
ds.Boole(
  V1 = "some_exp_out_df$outcome",
  V2 = 1,
  Boolean.operator = "==",
  na.assign = "0",
  newobj = "outcome_valid")

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "some_exp_out_df", 
  V1.name = "outcome_valid", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "live_births_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_21")
conns <- datashield.login(logindata, restore = "env_pnd_21")

################################################################################
# 23. Restrict to first borns
################################################################################

## ---- Set all GEN-R child_no as 1 --------------------------------------------
ds.rep(
  x1 = 1,
  times = ds.dim("live_births_df", datasources = conns["genr"])[[1]][[1]],
  source.x1 = "clientside",
  source.times = "c",
  newobj = "child_no",
  datasources = conns[c("genr", "moba")])

ds.asInteger("child_no", "child_no", datasources = conns["genr"])

dh.dropCols(
  df = "live_births_df",
  vars = "child_no",
  type = "remove",
  conns = conns["genr"])

ds.dataFrame(
  x = c("live_births_df", "child_no"),
  newobj = "live_births_df",
  datasources = conns["genr"])

ds.Boole(
  V1 = "live_births_df$child_no",
  V2 = 1,
  Boolean.operator = "==",
  na.assign = "0",
  newobj = "child_no_valid")

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "live_births_df", 
  V1.name = "child_no_valid", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "analysis_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_22")
conns <- datashield.login(logindata, restore = "env_pnd_22")

################################################################################
# 24. Create excluded participants dataset
################################################################################

## ---- First create vector indicating membership of analysis_df ---------------
dims <- ds.dim("analysis_df")

length.ref <- tibble(
  cohort = names(conns), 
  length = dims %>% map(~.x[[1]]) %>% unlist() %>% head(-1) %>% as.character
)

length.ref %>%
  pmap(function(cohort, length){
    ds.rep(
      x1 = 1,
      times = length,
      source.x1 = "clientside",
      source.times = "c",
      newobj = "case_def", 
      datasources = conns[cohort])
  })

ds.dataFrame(
  x = c("analysis_df$child_id", "case_def"),
  newobj = "analysis_df_tmp"
)

## ---- Now merge this vector with baseline_df ---------------------------------
ds.merge(
  x.name = "baseline_df",
  y.name = "analysis_df_tmp",
  by.x = "child_id",
  by.y = "child_id",
  all.x = TRUE,
  all.y = TRUE, 
  newobj = "exc_tmp")

## ---- Transform case_def to binary vector ------------------------------------
ds.Boole(
  V1 = "exc_tmp$case_def", 
  V2 = 1, 
  Boolean.operator = "==", 
  na.assign = "0", 
  newobj = "almost_there"
)

## ---- Create excluded subset based on this vector ----------------------------
ds.dataFrameSubset(
  df.name = "baseline_df", 
  V1.name = "almost_there", 
  V2.name = "0", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "excluded_df")

ds.dim("excluded_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_24")
conns <- datashield.login(logindata, restore = "env_pnd_24")

################################################################################
# 24. Create IQR versions of variables  
################################################################################
ds.dataFrameFill("analysis_df", "analysis_df")

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
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_25")
conns <- datashield.login(logindata, restore = "env_pnd_25")

################################################################################
# 25. Create quartiles of continuous variables
################################################################################

## ---- NO2 --------------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "no2_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- PM2.5 ------------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "pm25_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- PM10 -------------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "pm10_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- NDVI -------------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "ndvi300_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- Facility richness ------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "frichness300_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- Walkability ------------------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "walkability_mean_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- Population density -----------------------------------------------------
dh.quartileSplit(
  df = "analysis_df",
  var = "popdens_preg_iqr_c",
  type = "split",
  band_action = "ge_l",
  var_suffix = "_q_c_")

## ---- Fill dataframes --------------------------------------------------------
ds.dataFrameFill("analysis_df", "analysis_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_26")
conns <- datashield.login(logindata, restore = "env_pnd_26")    

################################################################################
# SENSITIVITY SUBSETS
################################################################################
################################################################################
# 26. First time mothers
################################################################################

## ---- Identify available data ------------------------------------------------
parity_avail <- dh.anyData(
  df = "analysis_df", 
  vars = "parity_m")

## ---- Subset -----------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$parity_bin", 
  V2.name = "0", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "parity_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_27")
conns <- datashield.login(logindata, restore = "env_pnd_27")  

################################################################################
# 27. Pregnancies free from comorbidities
################################################################################

## ---- Identify cases with any comorbidity ------------------------------------

## Pregnancy diabetes
ds.Boole(
  V1 = "analysis_df$preg_dia", 
  V2 = 1,
  Boolean.operator = "==", 
  na.assign = "0", 
  newobj = "preg_dia_y")

## Pregnancy hypertension
ds.Boole(
  V1 = "analysis_df$preg_ht", 
  V2 = 1,
  Boolean.operator = "==", 
  na.assign = "0", 
  newobj = "preg_ht_y")

## Preterm birth - note 1 = term.
ds.Boole(
  V1 = "analysis_df$preterm", 
  V2 = 0,
  Boolean.operator = "==", 
  na.assign = "0", 
  newobj = "preterm_y")

## ---- Create composite variable indicating how many comorbidities ------------
ds.assign(
  toAssign = "preg_dia_y+preg_ht_y+preterm_y", 
  newobj = "n_comorb"
)

## ---- Create variable indicating mothers with at least one -------------------
ds.Boole(
  V1 = "n_comorb", 
  V2 = 0,
  Boolean.operator = ">", 
  na.assign = "0", 
  newobj = "some_comorb")

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "some_comorb", 
  V2.name = "0", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "no_comorbid_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_28")
conns <- datashield.login(logindata, restore = "env_pnd_28")  

################################################################################
# 28. No history of depression
################################################################################

## ---- Identify available data ------------------------------------------------
dep_avail <- dh.anyData(
  df = "analysis_df", 
  vars = c("prepreg_psych", "prepreg_dep"))

## ---- Create variable indicating yes for either ------------------------------

## Depression
ds.Boole(
  V1 = "analysis_df$prepreg_dep", 
  V2 = 1,
  Boolean.operator = "==", 
  na.assign = "0", 
  newobj = "dep_y")

## Psychiatric problems
ds.Boole(
  V1 = "analysis_df$prepreg_psych", 
  V2 = 1,
  Boolean.operator = "==", 
  na.assign = "0", 
  newobj = "psych_y")

## ---- Create variable indicating mothers with at least one -------------------
ds.assign(
  toAssign = "dep_y+psych_y", 
  newobj = "dep_psych_n"
)

## ---- Create variable indicating mothers with at least one -------------------
ds.Boole(
  V1 = "dep_psych_n", 
  V2 = 0,
  Boolean.operator = ">", 
  na.assign = "0", 
  newobj = "dep_psych_any")

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "dep_psych_any", 
  V2.name = "0", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "no_dep_psych_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns[c("eden_nan", "eden_poit")], "env_pnd_29")
conns <- datashield.login(logindata, restore = "env_pnd_29")  

################################################################################
# 30. Fix areases in eden
################################################################################
eden_coh <- c("eden_nan", "eden_poit")

## ---- Assign variables again -------------------------------------------------
cohorts_tables %>%
  dplyr::filter(cohort %in% eden_coh & type == "nonrep") %>%
  pwalk(function(cohort, table, type){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = "area_ses", 
      value = table, 
      variables = c("child_id", "areases_tert_preg"))
  })

## ---- Rename new var ---------------------------------------------------------
ds.assign(
  toAssign = "area_ses$areases_tert_preg", 
  newobj = "areases_tert",
  datasources = conns[eden_coh]) 

## ---- Remove old empty vars --------------------------------------------------
dh.dropCols(
  df = "area_ses", 
  vars = "areases_tert_preg", 
  conns = conns[eden_coh], 
  type = "remove"
)

dh.dropCols(
  df = "analysis_df", 
  vars = "areases_tert", 
  conns = conns[eden_coh], 
  type = "remove"
)

## ---- Join back in -----------------------------------------------------------
ds.dataFrame(
  x = c("area_ses", "areases_tert"), 
  newobj = "area_ses",
  datasources = conns[eden_coh])

## ---- Merge with main data ---------------------------------------------------
ds.merge(
  x.name = "analysis_df", 
  y.name = "area_ses", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE,
  newobj = "analysis_df", 
  datasources = conns[eden_coh])

## ---- Check ------------------------------------------------------------------
ds.colnames("analysis_df", datasources = conns[eden_coh])
ds.summary("analysis_df$areases_tert", datasources = conns[eden_coh])

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_30")



################################################################################
# 30. Additional recodes
################################################################################

## ---- Binary variable for income ---------------------------------------------

## ---- Binary variable for ethnicity ------------------------------------------
ds.recodeValues(
  var.name = "analysis_df$ethn3_m",
  values2replace.vector = seq(1, 3, 1),
  new.values.vector = c(0, 1, 1),
  newobj = "eth_bin", 
  datasources = conns[!names(conns) %in% c("dnbc", "moba", "ninfea")])

ds.dataFrameFill("analysis_df", "analysis_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_30")
conns <- datashield.login(logindata, restore = "env_pnd_30")  

ds.table("eth_bin", 
         datasources = conns[!conns %in% c("dnbc", "moba", "rhea")])

ds.summary("analysis_df$ethn3_m")




        
        
        
        
        