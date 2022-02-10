################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Reference tables for variables
## Date: 3rd August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Create vectors of variables
################################################################################

## ---- Exposures --------------------------------------------------------------
exp_preg.vars <- c(
  "no2_preg", "pm25_preg", "pm10_preg", "lden_preg_f", "ndvi300_preg", 
  "greenyn300_preg", "blueyn300_preg",  "frichness300_preg", 
  "walkability_mean_preg", "popdens_preg")

exp_year_1.vars <- c(
  "no2_1", "pm25_1", "pm10_1", "lden_c_1", "ndvi300_1",  "greenyn300_1", 
  "blueyn300_1", "popdens_1", "frichness300_1", "walkability_mean_1"
)

exp.names <- c(
  "NO2", "PM2.5", "PM10", "Lden", "NDVI", "Green space > 5000m2 within 300m", 
  "Blue space > 5000m2 within 300m", "Facility richness", "Walkability score", 
  "Population density")

## ---- Covariates -------------------------------------------------------------
cov.vars <- c(
  "edu_m_0", "eusilc_income_quintiles", "areases_tert", "ethn3_m", 
  "parity_bin", "sex", "prepreg_psych", "prepreg_dep", "preg_dia", "preg_ht", 
  "birth_month_f", "birth_year_f", "mat_age_f", "preterm", "cohab")

cov.names <- c(
  "Maternal education", "Income", "Area deprivation", "Maternal ethnicity",
  "Parity", "Child sex", "Maternal pre-pregnancy psychiatric conditions",
  "Maternal pre-pregnancy depression",  "Gestational diabetes", 
  "Pregnancy hypertension", "Birth season", "Birth year", 
  "Maternal age at birth", "Pre-term birth", "Maternal cohabitation at birth")


## ---- Outcome --------------------------------------------------
out.vars <- "ppd"

out.names <- "Postpartum depression"


## ---- Meta variables ---------------------------------------------------------
meta.vars <- c(ref_codes$dummy, "child_no", "outcome", "child_id")

meta.names <- c(
  "alspac_dummy", "bib_dummy", "dnbc_dummy", "genr_dummy", "moba_dummy", 
  "ninfea_dummy", "rhea_dummy", "eden_nancy_dummy", "eden_poitiers dummy", 
  "inma_gip_dummy", "inma_sabadell dummy", "Child number", "Birth outcome", 
  "Child identifier")

################################################################################
# 2. Create reference tables
################################################################################

## ---- Exposures --------------------------------------------------------------
exp_preg.ref <- tibble(
  variable = exp_preg.vars,
  full_name = exp.names, 
  type = c(rep("cont", 3), "cat", "cont", rep("cat", 2), rep("cont", 3)), 
  var_type = "exposure"
)

exp_year_1.ref <- tibble(
  variable = exp_year_1.vars,
  full_name = exp.names, 
  type = c(rep("cont", 3), "cat", "cont", rep("cat", 2), rep("cont", 3)), 
  var_type = "exposure"
)

## ---- Covariates -------------------------------------------------------------
cov.ref <- tibble(
  variable = cov.vars,
  full_name = cov.names,
  type = rep("cat", 15),
  var_type = "covariate"
)

## ---- Outcome ----------------------------------------------------------------
out.ref <- tibble(
  variable = out.vars,
  full_name = out.names, 
  type = "cat",
  var_type = "outcome"
)

## ---- Meta variables ---------------------------------------------------------
meta.ref <- tibble(
  variable = meta.vars, 
  full_name = meta.names,
  type = c(rep("cat", 13), "cont"), 
  var_type = "meta"
)

## ---- Combined reference table -----------------------------------------------
full.ref <- bind_rows(
  exp_preg.ref, exp_year_1.ref, cov.ref, out.ref, meta.ref) 

full.ref %>% print(n = Inf)


