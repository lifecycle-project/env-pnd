################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Produces tables for manuscript 
## Date: 3rd August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(here)
library(readr)

conns <- datashield.login(logindata, restore = "env_pnd_24")
source("~/env-pnd/R/var-reference.R")
################################################################################
# METHODS  
################################################################################
################################################################################
# Inclusion criteria and participating cohorts / Figure 1
################################################################################
################################################################################
# Participating cohorts  
################################################################################
names(conns) %>% sort()

################################################################################
# Left column
################################################################################

## ---- Baseline sample --------------------------------------------------------
baseline_n <- ds.dim("baseline_df", type = "c")[[1]][[1]]

## ---- Any exposure + outcome -------------------------------------------------
some_exp_out_n <- ds.dim("some_exp_out_df", type = "c")[[1]][[1]]

## ---- Analysis dataset -------------------------------------------------------
analysis_n <- ds.dim("analysis_df", type = "c")[[1]][[1]]

################################################################################
# Right column
################################################################################

## ---- Excluded: no exposures or outcomes -------------------------------------
baseline_n - some_exp_out_n

## ---- Excluded: not live born or first born ----------------------------------
some_exp_out_n - analysis_n

################################################################################
# Total excluded
################################################################################
excluded_n <- (baseline_n - some_exp_out_n) + (some_exp_out_n - analysis_n)

save.image("~/env-pnd/4-feb-22.RData")

################################################################################
# Table S1: Cohort Ns
################################################################################
cohort_ns %>% 
  dplyr::filter(cohort != "combined") %>%
  dplyr::select(cohort, cohort_n)


################################################################################
# Table S2: correlations between exposures at two time points
################################################################################
cor_ref <- tibble(
  var_1 = c(
    "no2_preg", "pm25_preg", "pm10_preg", 
    "lden_c_preg", 
    "ndvi300_preg", "greenyn300_preg", "blueyn300_preg", 
    "frichness300_preg", "walkability_mean_preg", "popdens_preg"), 
  var_2 = c(
    "no2_1", "pm25_1", "pm10_1",
    "lden_c_1", 
    "ndvi300_1", "greenyn300_1", "blueyn300_1", 
    "frichness300_1", "walkability_mean_1", "popdens_1")
)

timepoint_cor_coh <- cor_ref %>%
  pmap(function(var_1, var_2){
    
    ds.cor(
      x = paste0("analysis_df$", var_1),
      y = paste0("analysis_df$", var_2),
      type = "split"
    )
  })

timepoint_cor_comb <- cor_ref %>%
  pmap(function(var_1, var_2){
    
    ds.cor(
      x = paste0("analysis_df$", var_1),
      y = paste0("analysis_df$", var_2),
      type = "combine", 
      naAction = "casewise.complete"
    )
  })

cor_names <- c(
  "no2", "pm25", "pm10",
  "lden_c", 
  "greenyn300", "blueyn300", "ndvi300",  
  "frichness300", "walkability", "popdens")

btp_coh <- timepoint_cor_coh %>% 
  map(function(x){
    x %>%
      map(~.[["Correlation Matrix"]][[2]]) %>%
      setNames(names(conns)) %>%
      bind_rows()}) %>%
  setNames(cor_names) %>%
  bind_rows(.id = "variable")

comb_cor <- timepoint_cor_comb %>%
  map(~.[["Correlation Matrix"]][[2]]) %>%
  setNames(cor_names) %>%
  unlist()

btp <- btp_coh %>%
  mutate(pooled = comb_cor) %>%
  select(variable, pooled, everything()) %>%
  mutate(across(pooled:rhea, round, 2))

write_csv(btp, path = here("tables", "btp_cor.csv"))



################################################################################
# Table S5: Ns for complete cases   
################################################################################
cc.tab <- miss.descriptives$categorical %>%
  mutate(variable = str_remove(variable, "_miss_f")) %>%
  dplyr::filter(cohort == "combined" & category == 1) %>%
  mutate(n_perc = paste0(value, " (", perc_valid, ")")) %>%
  left_join(., exp_preg.ref, by = "variable") %>%
  mutate(
    full_name = factor(
      full_name, 
      levels = exp_preg.ref$full_name, 
      ordered = TRUE)) %>%
  arrange(full_name) %>%
  dplyr::select(full_name, n_perc) 

write_csv(cc.tab, here("tables", "complete_cases.csv")) 

################################################################################
# Table S6: Sample characteristics analysis sample vs excluded
################################################################################

## ---- Simple function to make table ------------------------------------------
makeSampleComp <- function(stats){
  
  cat <- stats$categorical %>%
    dplyr::filter(variable %in% s5_cat.vars & cohort == "combined" &
                    category != "missing") %>%
    mutate(value = paste0(value, " (", perc_valid, ")")) %>%
    select(variable, category, value, cohort_n)
  
  cont <- stats$continuous %>%
    dplyr::filter(variable %in% s5_cont.vars & cohort == "combined") %>%
    mutate(value = paste0(perc_50, " (", perc_5, ",", perc_95, ")")) %>%
    select(variable, value, cohort_n) %>%
    mutate(category = NA)
  
  out <- bind_rows(cat, cont)
  
}

## ---- Specify variables ------------------------------------------------------
s5_cat.vars <- bind_rows(exp_preg.ref, cov.ref, out.ref) %>% 
  dplyr::filter(type == "cat") %>%
  pull(variable)

s5_cont.vars <- bind_rows(exp_preg.ref, cov.ref, out.ref) %>% 
  dplyr::filter(type == "cont") %>%
  pull(variable)


## ---- Make table -------------------------------------------------------------
a_sample.desc <- makeSampleComp(descriptives) %>% 
  mutate(sample = "analysis")

e_sample.desc <- makeSampleComp(exc.desc) %>% 
  mutate(sample = "excluded")

sample_comp <- bind_rows(a_sample.desc, e_sample.desc) %>%
  left_join(., full.ref, by = "variable") %>%
  mutate(variable = factor(
    variable, 
    levels = c(exp_preg.vars, cov.vars, out.vars), 
    ordered = TRUE)) %>%
  arrange(sample, variable) %>%
  left_join(., full_values.ref, by = c("variable", "category")) %>%
  mutate(category = factor(
    category, 
    levels = dt.setLevels(full_values.ref),
    ordered = TRUE)) %>%
  arrange(sample, variable, category) %>%
  pivot_wider(
    names_from = sample,
    values_from = value)
dplyr::select(full_name, cat_label, value, sample)

sample_comp %>% print(n = Inf)

analysis_n
excluded_n

write_csv(sample_comp, here("tables", "sample-comparison.csv"))

################################################################################
# RESULTS  
################################################################################
makeTable <- function(stats = descriptives, cat_vars){
  
  out <- stats$categorical %>%
    dplyr::filter(variable %in% cat_vars) %>%
    mutate(
      n_perc = paste0(value, " (", perc_total, ")")) %>%
    select(variable, cohort, category, n_perc)
  
  return(out)
  
}

################################################################################
# Table 1: Sample characteristics
################################################################################
tab1.vars <- c(cov.vars, out.vars)

table_1 <- makeTable(
  stats = descriptives,
  cat_vars = tab1.vars) %>%
  dplyr::filter(cohort == "combined") %>%
  left_join(., full.ref, by = "variable") %>%
  left_join(., full_values.ref, by = c("variable", "category")) %>%
  mutate(category = factor(
    category, 
    levels = dt.setLevels(
      full_values.ref %>% dplyr::filter(variable %in% tab1.vars)),
    ordered = TRUE)) %>%
  arrange(full_name, category, n_perc) %>%
  dplyr::select(full_name, cat_label, n_perc)

write_csv(table_1, path = here("tables", "table_1.csv"))

################################################################################
# Table S7: Sample characteristics by cohort  
################################################################################
table5 <- makeTable(
  stats = descriptives,
  cat_vars = tab1.vars) %>%
  dplyr::filter(cohort != "combined") %>%
  left_join(., full.ref, by = "variable") %>%
  left_join(., full_values.ref, by = c("variable", "category")) %>%
  pivot_wider(
    names_from = cohort,
    values_from = n_perc) %>%
  mutate(category = factor(
    category, 
    levels = dt.setLevels(
      full_values.ref %>% dplyr::filter(variable %in% tab1.vars)),
    ordered = TRUE)) %>%
  arrange(full_name, category) %>%
  dplyr::select(full_name, cat_label, alspac:rhea)

write_csv(table5, path = here("tables", "table5.csv"))

## ---- Cohort Ns for header ---------------------------------------------------
cohort_ns <- descriptives$categorical %>%
  dplyr::filter(variable == "sex" & category == 1 & cohort != "combined") %>%
  dplyr::select(cohort, cohort_n)

################################################################################
# ADDITIONAL ADJUSTMENTS
################################################################################
adjustTab <- function(mdata, var, cohorts){
  
  adj_tmp.tab <- prepMainPlot(
    mdata = mdata, 
    ref = exp_preg_coef.ref) %>%
    mutate(adjust = var) %>%
    dplyr::filter(cohort != "combined")
  
  main.tab <- model_1.pdata %>%
    mutate(adjust = "main") %>%
    dplyr::filter(cohort %in% cohorts)
  
  adj.tab <- bind_rows(adj_tmp.tab, main.tab) %>%
    arrange(full_name, adjust)
  
  adj.out <- adj.tab %>% 
    mutate(across(est:uppci, ~round(., 2))) %>%
    mutate(est_out = paste0(est, " (", lowci, ", ", uppci, ")")) %>%
    dplyr::select(full_name, cohort_neat, est_out, adjust) %>%
    pivot_wider(
      names_from = "adjust", 
      values_from = "est_out") %>%
    dplyr::select(full_name, cohort_neat, main, var)
  
  return(adj.out)
}

################################################################################
# Table S8: additional adjustment for ethnicity
################################################################################
eth.tab <- adjustTab(
  mdata = model_3.mdata,
  var = "ethnicity", 
  cohorts = c("alspac", "bib", "genr")
)

write_csv(eth.tab, file = here("tables", "eth_adj.csv"))

################################################################################
# Table S9: additional adjustment for cohabitation
################################################################################
cohab.tab <- adjustTab(
  mdata = model_4.mdata,
  var = "cohab", 
  cohorts = c("alspac", "bib", "dnbc", "genr", "moba")
)

write_csv(cohab.tab, file = here("tables", "cohab_adj.csv"))



################################################################################
# Number of cohorts with data on each exposure  
################################################################################
cohortNExp <- function(var, obj = descriptives$continuous){
  
  obj %>% 
    dplyr::filter(variable == var & !is.na(valid_n) & valid_n != 0 & 
                    cohort != "combined") %>%
    pull(cohort) %>%
    unique() %>%
    length()
  
} 

## ---- NDVI -------------------------------------------------------------------
cohortNExp("ndvi300_preg")

## ---- Air polution -----------------------------------------------------------
cohortNExp("no2_preg")
cohortNExp("pm25_preg")
cohortNExp("pm10_preg")

## ---- Road traffic noise -----------------------------------------------------
cohortNExp("lden_preg")
cohortNExp("lden_c_preg", obj = descriptives$categorical)

## ---- Built environment ------------------------------------------------------
cohortNExp("popdens_preg")
cohortNExp("frichness300_preg")
cohortNExp("walkability_mean_preg")

descriptives$categorical %>% dplyr::filter(variable == "lden_c_preg") %>% print(n = Inf)




################################################################################
# Table 3: Exposures  
################################################################################
tab3_cat.vars <- c(
  "greenyn300_preg", "blueyn300_preg", "lden_c_preg")

tab3_cont.vars <- c(
  "ndvi300_preg", "no2_preg", "pm25_preg", "pm10_preg", "frichness300_preg", 
  "walkability_mean_preg", "popdens_preg")

table3 <- makeTable(
  cat_vars = tab3_cat.vars, 
  cont_vars = tab3_cont.vars
) %>%
  dplyr::filter(cohort == "combined") %>%
  dplyr::select(-cohort) %>%
  arrange(
    factor(
      variable, levels = c(
        "ndvi300_preg", "greenyn300_preg", "blueyn300_preg", "no2_preg", 
        "pm25_preg", "pm10_preg", "lden_c_preg", "frichness300_preg", 
        "walkability_mean_preg", "popdens_preg"), 
      ordered = TRUE)
  )

write_csv(table3, path = here("tables", "table3.csv"))




################################################################################
# Table S4b: continuous descriptives by cohort  
################################################################################
table4b <- makeTable(
  stats = descriptives,
  cat_vars = tab2_cat.vars, 
  cont_vars = tab2_cont.vars
) %>%
  dplyr::filter(cohort != "combined") %>%
  dplyr::filter(!is.na(med_range)) %>%
  pivot_wider(
    names_from = cohort,
    values_from = c(med_range, missing)) %>%
  select(-category, -n_perc) %>%
  select(
    med_range_alspac, missing_alspac, med_range_bib, missing_bib, 
    med_range_dnbc, missing_dnbc, med_range_eden_nan, missing_eden_nan, 
    med_range_eden_poit, missing_eden_poit, med_range_genr, missing_genr, 
    med_range_inma_gip, missing_inma_gip, med_range_inma_sab, missing_inma_sab,
    med_range_moba, missing_moba, med_range_ninfea, missing_ninfea, 
    med_range_rhea, missing_rhea)                            

write_csv(table4b, path = here("tables", "table4b.csv"))

################################################################################
# Table S5a: Exposure categorical descriptives by cohort   
################################################################################
table5a <- makeTable(
  stats = descriptives,
  cat_vars = tab3_cat.vars, 
  cont_vars = tab3_cont.vars
) %>%
  dplyr::filter(cohort != "combined") %>%
  dplyr::filter(is.na(med_range)) %>%
  pivot_wider(
    names_from = cohort,
    values_from = n_perc) %>%
  select(-med_range, -missing) %>%
  arrange(
    factor(
      variable, levels = c(
        "greenyn300_preg", "blueyn300_preg", "lden_c_preg"), 
      ordered = TRUE)
  )

write_csv(table5a, path = here("tables", "table5a.csv"))

################################################################################
# Table S5b: Exposure continuous descriptives by cohort   
################################################################################
table5b <- makeTable(
  stats = descriptives,
  cat_vars = tab3_cat.vars, 
  cont_vars = tab3_cont.vars
) %>%
  dplyr::filter(cohort != "combined") %>%
  dplyr::filter(!is.na(med_range)) %>%
  pivot_wider(
    names_from = cohort,
    values_from = c(med_range, missing)) %>%
  select(-category, -n_perc) %>%
  select(
    med_range_alspac, missing_alspac, med_range_bib, missing_bib, 
    med_range_dnbc, missing_dnbc, med_range_eden_nan, missing_eden_nan, 
    med_range_eden_poit, missing_eden_poit, med_range_genr, missing_genr, 
    med_range_inma_gip, missing_inma_gip, med_range_inma_sab, missing_inma_sab,
    med_range_moba, missing_moba, med_range_ninfea, missing_ninfea, 
    med_range_rhea, missing_rhea)                            

write_csv(table5b, path = here("tables", "table5b.csv"))



