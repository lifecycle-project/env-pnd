################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Conduct analyses 
## Date: 3rd August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(here)
library(readr)
install_github("timcadman/datashield-tim", ref = "main")
library(dsTim)
library(dsHelper)

#remove.packages("dsTim")

conns <- datashield.login(logindata, restore = "env_pnd_16")

################################################################################
# 1. Descriptive statistics   
################################################################################

## ---- Define variables -------------------------------------------------------
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

## ---- Descriptives for analysis dataset --------------------------------------
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

descriptives$categorical %>% 
  filter(variable %in% c("prepreg_psych", "prepreg_dep")) %>%
  print(n = Inf)

## ---- Descriptives for full sample -------------------------------------------
descriptives_full <- dh.getStats(
  df = "env_pnd", 
  vars = all.vars
)

save.image()

ds.colnames("analysis_df")
################################################################################
# 2. Regression model definitions  
################################################################################
cohorts <- names(conns)

cov_adj <- c("sex", "edu_m_0", "areases_tert", "agebirth_m_y", "parity_bin", 
          "birth_year", "prepreg_psych")

# Currently excluding birth_month as unavailable for moba

exp_ref <- tibble(
  name = c("ndvi", "green", "blue", "no2", "pm25", "pm10", "lden", "fac_rich", 
           "walk", "pop_dens"), 
  exposure = factor(
    c("ndvi300_preg_iqr_p", "greenyn300_preg", "blueyn300_preg", 
      "no2_preg_iqr_p", "pm25_preg_iqr_p", "pm10_preg_iqr_p", "lden_c_preg", 
      "frichness300_preg_iqr_p", "walkability_mean_preg_iqr_p", 
      "popdens_preg_iqr_p"), ordered = TRUE), 
  full_name = c("NDVI", "Major green space within 300m", 
                "Major blue space within 300m", "N02", "PM2.5", "PM10", "Lden", 
                "Facility richness", "Walkability", "Population density"))

avail_exp <- dh.anyData(df = "analysis_df", vars = exp_ref %>% pull(exposure))
avail_cov <- dh.anyData(df = "analysis_df", vars = cov_adj)

## ---- Create a list of available cohorts for each analysis -------------------
single.mod <- avail_exp %>%
  mutate(variable = factor(
    variable, 
    levels = exp_ref$exposure, 
    ordered = TRUE)) %>%
  group_by(variable) %>%
  group_split %>%
  map(~bind_rows(., avail_cov)) %>%
  map(function(x){
    x %>% 
      select(-variable) %>% 
      dplyr::select(where(function(x) all(x) == TRUE)) %>%
      colnames}) %>%
  set_names(exp_ref$exposure) %>% 
  imap(function(.x, .y){
    out <- list(
      exposure = .y, 
      outcome = "ppd",
      covariates = cov_adj,
      cohorts = .x)}) %>%
  set_names(exp_ref$name)

################################################################################
# 3. Exploring missingness  
################################################################################
################################################################################
# Define models  
################################################################################

## ---- New models for complete cases ------------------------------------------
single_miss.mod <- single.mod %>% map(dt.modToMiss)

################################################################################
# Define complete cases  
################################################################################
single_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dt.defineCompleteCase(
        df = "env_pnd", 
        vars = vars, 
        newobj = name,
        conns = conns[cohorts]
      )
    })
  })

datashield.workspace_save(conns, "env_pnd_miss_1")
conns <- datashield.login(logindata, restore = "env_pnd_miss_1")


################################################################################
# Get stats of these created variables  
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Tibble of variables to join --------------------------------------------
tojoin <- single_miss.mod %>%
  map(function(x){
    out <- tibble(
      var = x$name, 
      cohort = x$cohort)}) %>%
  setNames(., paste0("tmp_", seq(1, length(.), 1))) %>%
  bind_rows %>%
  group_by(cohort) %>%
  group_split


## ---- Join into one dataframe ------------------------------------------------
tojoin %>%
  map(function(x){
    
    ds.dataFrame(
      x = x$var,
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
      input.var.name = paste0("missing$", .), 
      newobj.name = paste0(., "_fact"))
  )

datashield.workspace_save(conns, "env_pnd_miss_4")
conns <- datashield.login(logindata, restore = "env_pnd_miss_4")


## ---- Join back into one dataframe -------------------------------------------
ds.dataFrame(
  x = paste0(ds.colnames("missing")[[1]], "_fact"),
  newobj = "missing")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "env_pnd_miss_5")
conns <- datashield.login(logindata, restore = "env_pnd_miss_5")


################################################################################
# Now extract descriptives  
################################################################################
miss_vars <- ds.colnames("missing")[[1]]

miss.descriptives <- dh.getStats(
  df = "missing", 
  vars = miss_vars
)

save.image()

ds.cor("analysis_df$no2_preg", "analysis_df$ppd")

################################################################################
# Regression models to see whether outcome is associated with missingness  
################################################################################

## ---- Function  --------------------------------------------------------------
dt.makeMissForm <- function(x = single_miss.mod, df = "env_pnd", 
                            outcome = "ppd"){
  
  mod <- list(
    model = paste0(
      paste0(
        x$name,
        "~",  
        df, "$", str_subset(x$vars[[1]], outcome)),
      "+", df, "$sex",
      "+", paste0(paste0(df, "$", x$cohorts[-1]), "_d", collapse = "+")), 
    cohorts = x$cohorts
  )
}

ds.colnames("env_pnd")

## ---- Maternal education ----------------------------------------------------- 
single_miss.fit <- single_miss.mod %>%
  dt.changeForm(
    var = c("eden_poit", "eden_nan", "inma_sab", "inma_gip"), 
    type = "remove", 
    category = "cohorts") %>%
  map(dt.makeMissForm) %>%
  map(dt.glmWrap, type = "ipd", df = NULL)

save.image()

## Being a bit lazy: excluding subcohorts across the board that break it 
## for some models only. Revisit. 

################################################################################
# 4. Main models  
################################################################################
conns <- datashield.login(logindata, restore = "env_pnd_16")

## ---- Single exposure --------------------------------------------------------
single_works.mod <- single.mod %>%
  dt.changeForm(
    elements = "pop_dens", 
    vars = "inma_gip", 
    category = "cohorts", 
    type = "remove") %>%
  dt.changeForm(
    elements = names(single.mod), 
    vars = "dnbc", 
    category = "cohorts", 
    type = "remove")

single.fit = list(
  ipd = single_works.mod %>%
    map(dt.makeGlmForm, type = "ipd", dummy_suff = "_d") %>%
    map(dt.glmWrap, type = "ipd", family = "binomial"), 
  slma = single_works.mod %>%
    map(dt.makeGlmForm, type = "slma") %>%
    map(dt.glmWrap, type = "slma", family = "binomial")) 

save.image()


################################################################################
# 5. Diagnostics  
################################################################################

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



