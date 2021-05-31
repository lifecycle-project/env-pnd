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

save(exposures.desc, file = here("data", "descriptives.RData"))
save(exposures.desc, file = here("data", "descriptives.RData"))



