################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Produces tables for manuscript 
## Date: 3rd August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(here)
library(readr)

conns <- datashield.login(logindata, restore = "env_pnd_14")

exp_ordered <- c("ndvi300_preg", "greenyn300_preg", "blueyn300_preg", "no2_preg", 
"pm25_preg", "pm10_preg", "lden_c_preg", "frichness300_preg", 
"walkability_mean_preg", "popdens_preg")
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
# Maximum sample size
################################################################################
original_n <- dh.getStats(
  df = "env_pnd", 
  vars = "sex",
  conns = conns
)

## ---- Original dataset -------------------------------------------------------
max_n <- original_n$categorical %>%
  dplyr::filter(variable == "sex" & category == 1) %>%
  select(cohort, cohort_n)

## ---- Analysis dataset -------------------------------------------------------
cohort_ns <- descriptives$categorical %>%
  dplyr::filter(variable == "sex" & category == 1) %>%
  select(cohort, cohort_n) 

## ---- Participants with no eligible data -------------------------------------
max_n %>% filter(cohort == "combined") %>% pull(cohort_n) - 
  cohort_ns %>% filter(cohort == "combined") %>% pull(cohort_n)

################################################################################
# Table S2: correlations between exposures at two time points
################################################################################
cor_ref <- tibble(
  var_1 = c(
    "greenyn300_preg", "blueyn300_preg", "ndvi300_preg", 
    "no2_preg", "pm25_preg", "pm10_preg",
    "lden_c_preg", 
    "frichness300_preg", "walkability_mean_preg", "popdens_preg"), 
  var_2 = c(
    "greenyn300_1", "blueyn300_1", "ndvi300_1", 
    "no2_1", "pm25_1", "pm10_1",
    "lden_c_1", 
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
  "greenyn300", "blueyn300", "ndvi300",  
  "no2", "pm25", "pm10",
  "lden_c", 
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

## ---- Road traffic noise -----------------------------------------------------
cohortNExp("lden_preg")
cohortNExp("lden_c_preg", obj = descriptives$categorical)

## ---- Built environment ------------------------------------------------------
cohortNExp("popdens_preg")
cohortNExp("frichness300_preg")
cohortNExp("walkability_mean_preg")

descriptives$categorical %>% dplyr::filter(variable == "lden_c_preg") %>% print(n = Inf)

################################################################################
# RESULTS  
################################################################################
makeTable <- function(stats = descriptives, cat_vars, cont_vars){
  
  cat <- stats$categorical %>%
    dplyr::filter(variable %in% cat_vars) %>%
    mutate(
      n_perc = paste0(value, " (", perc_total, ")"), 
      med_range = NA,
      missing = NA) %>%
    select(variable, cohort, category, n_perc, med_range, missing)
  
  cont <- descriptives$continuous %>%
    dplyr::filter(variable %in% cont_vars) %>%
    mutate(
      med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")"),
      missing = paste0(missing_n, " (", missing_perc, ")"), 
      category = NA, 
      n_perc = NA) %>%
    select(variable, cohort, category, n_perc, med_range, missing) 
  
  out <- bind_rows(cat, cont)
  
  return(out)
  
}

################################################################################
# Table 2: Covariates and outcome
################################################################################
tab2_cat.vars <- c(
  "ppd", "edu_m_0", "areases_tert_preg", "ethn3_m", "parity_bin", "sex",  
  "birth_month", "prepreg_psych")

tab2_cont.vars <- c("agebirth_m_y", "birth_year")

table2 <- makeTable(
  stats = descriptives,
  cat_vars = tab2_cat.vars, 
  cont_vars = tab2_cont.vars
  ) %>%
  dplyr::filter(cohort == "combined") %>%
  dplyr::select(-cohort) %>%
  arrange(
    factor(
      variable, levels = c(
        "sex", "birth_year", "ethn3_m", "agebirth_m_y", "parity_bin", 
         "areases_tert_preg", "prepreg_psych"), 
      ordered = TRUE)
  )
        
write_csv(table2, path = here("tables", "table2.csv"))


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
# Table S4a: categorical descriptives by cohort  
################################################################################
table4a <- makeTable(
  stats = descriptives,
  cat_vars = tab2_cat.vars, 
  cont_vars = tab2_cont.vars
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
        "sex", "ethn3_m", "parity_bin", "areases_tert_preg"), 
      ordered = TRUE)
  )

write_csv(table4a, path = here("tables", "table4a.csv"))

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

################################################################################
# Table 6: Sample characteristics analysis sample vs full
################################################################################
makeSampleComp <- function(x){
  
  all_cat.vars <- c(
    "ppd", "edu_m_0", "areases_tert_preg", "ethn3_m", "parity_bin", "sex",  
    "greenyn300_preg", "blueyn300_preg", "lden_c_preg")
  
  all_cont.vars <- c(
    "agebirth_m_y", "birth_year", "ndvi300_preg", "no2_preg", "pm25_preg", 
    "pm10_preg", "frichness300_preg", "walkability_mean_preg", "popdens_preg")
  
  tmp_1 <- x$continuous %>%
    filter(variable %in% all_cont.vars & cohort == "combined") %>%
    mutate(value = paste0(perc_50, " (", perc_5, ",", perc_95, ")")) %>%
    select(variable, value, cohort_n) %>%
    mutate(category = NA)
  
  tmp_2 <- x$categorical %>%
    filter(
      variable %in% all_cat.vars & cohort == "combined" &
        category != "missing") %>%
    mutate(value = paste0(value, " (", perc_valid, ")")) %>%
    select(variable, category, value, cohort_n)
  
  out <- bind_rows(tmp_1, tmp_2)
  
}

a_sample.desc <- makeSampleComp(descriptives) %>% mutate(sample = "analysis")
f_sample.desc <- makeSampleComp(descriptives_full) %>% mutate(sample = "full")

sample_comp <- bind_rows(a_sample.desc, f_sample.desc) %>%
  mutate(variable = factor(
    variable, 
    levels = c(
      "sex", "birth_year", "ethn3_m", "edu_m_0", "agebirth_m_y", "parity_bin", 
      "areases_tert_preg", "ppd", "ndvi300_preg", "greenyn300_preg", 
      "blueyn300_preg", "no2_preg", "pm25_preg", "pm10_preg", 
      "lden_c_preg", "frichness300_preg", "walkability_mean_preg", 
      "popdens_preg")
  )) %>%
  arrange(sample, variable)

write_csv(sample_comp, here("tables", "sample-comparison.csv"))

################################################################################
# Table S7: Ns for complete cases  
################################################################################
cc.tab <- miss.descriptives$categorical %>%
  mutate(variable = str_remove(variable, "_ppd_m_fact")) %>%
  dplyr::filter(cohort == "combined" & category == "1") %>%
  mutate(
    n_perc = paste0(value, " (", perc_valid, ")"),
    variable = factor(
      x = variable, 
      levels = c(
        "ndvi300_preg", "greenyn300_preg", "blueyn300_preg", "no2_preg", 
        "pm25_preg", "pm10_preg", "lden_c_preg", "frichness300_preg", 
        "walkability_mean_preg", "popdens_preg")
    )) %>%
  dplyr::select(variable, n_perc) %>%
  arrange(variable)

write_csv(cc.tab, here("tables", "complete_cases.csv"))  

### Next fix analysis df to include non-missing ppd

