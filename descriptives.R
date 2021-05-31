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

source("/home/tim/useful-code-r/code/themes/theme-std.R")
source("/home/tim/useful-code-r/code/themes/lc-names-neat.R")

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

out.vars <- "ppd"

################################################################################
# 2. Extract stats  
################################################################################
descriptives <- dh.getStats(
  df = "analysis_df", 
  vars = c(sep.vars, pol.vars, nat.vars, lu.vars, out.vars)
)

################################################################################
# 3. Write descriptives  
################################################################################

# We do it like this because we can't make markdown files in the analysis 
# server so instead we do it locally.

write.csv(descriptives, file = "")

################################################################################
# 3. Arrange data  
################################################################################
time_labs <- c("Birth to 12 months", "Pregnancy")

desc_clean <- descriptives %>%
   map(
    ~separate(, 
      data = .,
      col = variable, 
      sep = "_(?=[^_]*$)", 
      into = c("variable", "time"))
  ) %>%
  map(
    ~mutate(
      .data = ., 
      time = factor(time, labels = time_labs, ordered = TRUE))
    ) %>%
  map(
    ~mutate(
      .data = ., 
      time = fct_relevel(time, rev(time_labs)))
  ) %>%
  map(
    ~left_join(., names_neat, by = "cohort")
  ) %>%  
  map(
    ~mutate(
      .data = .,
      cohort_neat = fct_relevel(
        cohort_neat, 
        c("MoBa", "GENR", "ALSPAC", "EDEN", "NINFEA", "INMA")))
  )


################################################################################
# 4. Plot natural space exposures  
################################################################################
theme_bar <- theme(
  legend.position = "none",
  panel.grid.major.y=element_line(colour="grey"), 
  panel.grid.major.x=element_line(colour="white"),
)

## ---- NDVI -------------------------------------------------------------------
ndvi.plot <- desc_clean$continuous %>%
  filter(variable == "ndvi300" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("NDVI") +
  ggtitle("Mean NDVI values") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

     
## ---- Green distance ---------------------------------------------------------
green_dist.plot <- desc_clean$continuous %>%
  filter(
    variable == "green_dist" & !cohort %in% c("genr", "combined")) %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("Distance (m)") +
  ggtitle("Mean distance to nearest green space > 5000 m2") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

desc_clean$continuous %>%
  filter(variable == "green_dist" & cohort == "genr") %>%
  select(cohort, time, mean, std.dev)

## ---- Blue distance ---------------------------------------------------------
blue_dist.plot <- desc_clean$continuous %>%
  filter(
    variable == "blue_dist" & !cohort %in% c("genr", "combined")) %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("Distance (m)") +
  ggtitle("Mean distance to nearest blue space > 5000 m2") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2000)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

desc_clean$continuous %>%
  filter(variable == "blue_dist" & cohort == "genr") %>%
  select(cohort, time, mean, std.dev)


################################################################################
# 5. Plot polution exposures  
################################################################################

## ---- N02 pregnancy ----------------------------------------------------------
no2.plot <- desc_clean$continuous %>%
  filter(variable == "no2" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  xlab("Cohort") +
  ylab("N02 (microgram / m3)") +
  ggtitle("Mean N02 values in pregnancy") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

## ---- PM2.5 pregnancy --------------------------------------------------------
no2.plot <- desc_clean$continuous %>%
  filter(variable == "pm25" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  xlab("Cohort") +
  ylab("PM2.5 (microgram / m3)") +
  ggtitle("Mean PM2.5 values in pregnancy") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar


################################################################################
# 6. Plot noise exposure  
################################################################################
blue_dist.plot <- desc_clean$continuous %>%
  filter(
    variable == "lden" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("Noise (dBA)") +
  ggtitle("Mean day-evening-night traffic noise") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar


################################################################################
# 7. Plot land use exposures  
################################################################################

## ---- Building density -------------------------------------------------------
bdens.plot <- desc_clean$continuous %>%
  filter(variable == "bdens300" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("Density (m2 / km2)") +
  ggtitle("Mean building density") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 750000)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

## ---- Urban green land use ---------------------------------------------------
lu_green.plot <- desc_clean$continuous %>%
  filter(variable == "natgr" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("%") +
  ggtitle("Percentage of urban green land use") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

## ---- Forest land use --------------------------------------------------------        
lu_foest.plot <- desc_clean$continuous %>%
  filter(variable == "agrgr" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("%") +
  ggtitle("Percentage of forest land use") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

## ---- Walkability index ------------------------------------------------------
walkability.plot <- desc_clean$continuous %>%
  filter(variable == "walkability_mean" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("Decile") +
  ggtitle("Walkability Index") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.6)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

## ---- Land use Shannon's evenness index --------------------------------------
lu_shannon.plot <- desc_clean$continuous %>%
  filter(variable == "landuseshan300" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ggtitle("Shannon's evenness Index") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.6)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

## ---- Facilities richness ----------------------------------------------------
lu_fac_rich.plot <- desc_clean$continuous %>%
  filter(variable == "frichness300" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("N facility types / maximum facility types") +
  ggtitle("Facility richness") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.20)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

## ---- Facilities density -----------------------------------------------------
lu_fac_dens.plot <- desc_clean$continuous %>%
  filter(variable == "frichness300" & cohort != "combined") %>%
  ggplot(aes(x = cohort_neat, y = mean, fill = cohort_neat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~time, ncol = 1) +
  xlab("Cohort") +
  ylab("Number of facilities present within 300m buffer") +
  ggtitle("Facility density") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.20)) +
  scale_fill_manual(values = palette_std) + 
  theme_std + 
  theme_bar

################################################################################
# 7. Postnatal depression  
################################################################################
desc_clean$categorical %>% print(n = Inf)






