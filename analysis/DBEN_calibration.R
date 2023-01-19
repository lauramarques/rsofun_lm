# This script calibrates the model for the target variables selected

# load packages
library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(multidplyr)
library(tidyverse)

# Get ddf_obs ####

# benchmark
regrowth_dynamics <- read.csv("~/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/benchmark_regrowth_and_eq_dynamics.csv")

unique(regrowth_dynamics$Biome)
# FIN
regrowth_dynamics_FIN <- regrowth_dynamics %>% filter(Biome=="Boreal")
ggplot(regrowth_dynamics_FIN,aes(x=bin_num,y=AGB_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGB_kgCm2_10, ymax=AGB_kgCm2_90), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,35)) + geom_hline(yintercept=10, col="grey")
# BIA
regrowth_dynamics_BIA <- regrowth_dynamics %>% filter(Biome=="Temperate")
ggplot(regrowth_dynamics_BIA,aes(x=bin_num,y=AGB_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGB_kgCm2_10, ymax=AGB_kgCm2_90), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,35)) + geom_hline(yintercept=10, col="grey")
# BCI
regrowth_dynamics_BCI <- regrowth_dynamics %>% filter(Biome=="Tropics")
ggplot(regrowth_dynamics_BCI,aes(x=bin_num,y=AGB_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGB_kgCm2_10, ymax=AGB_kgCm2_90), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,35)) + geom_hline(yintercept=10, col="grey")

stand_structure <- read.csv("~/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/benchmark_stand_structure.csv")
str(stand_structure)
# FIN
stand_structure_FIN <- stand_structure %>% filter(site=="FI")
ggplot(stand_structure_FIN,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,500)) 
ggplot(stand_structure_FIN,aes(x=dbh_classes_num,y=cwood_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=cwood_size_lower_kgCm.2, ymax=cwood_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,6.5)) 
# BIA
stand_structure_BIA <- stand_structure %>% filter(site=="BIA")
ggplot(stand_structure_BIA,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,350)) 
ggplot(stand_structure_BIA,aes(x=dbh_classes_num,y=cwood_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=cwood_size_lower_kgCm.2, ymax=cwood_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,6.5)) 
# BCI
stand_structure_BCI <- stand_structure %>% filter(site=="BCI")
ggplot(stand_structure_BCI,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,350)) 
ggplot(stand_structure_BCI,aes(x=dbh_classes_num,y=cwood_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=cwood_size_lower_kgCm.2, ymax=cwood_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,6.5)) 

# FIN ####
## Target 1. Regrowth ####
regrowth_dynamics_FIN # kg C m-2

## Target 2. AGB - Biomass dynamics ####
regrowth_dynamics_FIN # kg C m-2

## Target 3. N stems per ha ####
nstem_FIN <- stand_structure_FIN %>% select(dbh_classes_num,nstem_size_ha.1) %>% rename(nstem_ha=nstem_size_ha.1)

## Target 3. N stems per ha ####
nstem_FIN <- stand_structure_FIN %>% select(dbh_classes_num,nstem_size_ha.1) %>% rename(nstem_ha=nstem_size_ha.1)
cwood_FIN <- stand_structure_FIN %>% select(dbh_classes_num,nstem_size_ha.1) %>% rename(nstem_ha=nstem_size_ha.1)


# Prepare the observed target variables: ddf_obs ####
ddf_obs <- data.frame(
  variables = c("regrowth","biomass","stem_c1","stem_c2","stem_c3","stem_c4","stem_c5","stem_c6","stem_c7",
                "stem_c8","stem_c9","stem_c10","stem_c11","stem_c12","stem_c13","stem_c14","stem_c15","stem_c16",
                "cwood_c1","cwood_c2","cwood_c3","cwood_c4","cwood_c5","cwood_c6","cwood_c7",
                "cwood_c8","cwood_c9","cwood_c10","cwood_c11","cwood_c12","cwood_c13","cwood_c14","cwood_c15","cwood_c16"),
  targets_obs = c(XXXXXXX, XXXXXXX,
                  nstem_FIN$nstem_ha[1],nstem_FIN$nstem_ha[2],nstem_FIN$nstem_ha[3],nstem_FIN$nstem_ha[4],
                  nstem_FIN$nstem_ha[5],nstem_FIN$nstem_ha[6],nstem_FIN$nstem_ha[7],nstem_FIN$nstem_ha[8],
                  nstem_FIN$nstem_ha[9],nstem_FIN$nstem_ha[10],nstem_FIN$nstem_ha[11],nstem_FIN$nstem_ha[12],
                  nstem_FIN$nstem_ha[13],nstem_FIN$nstem_ha[14],nstem_FIN$nstem_ha[15],nstem_FIN$nstem_ha[16],
                  nstem_FIN$nstem_ha[1],nstem_FIN$nstem_ha[2],nstem_FIN$nstem_ha[3],nstem_FIN$nstem_ha[4],
                  nstem_FIN$nstem_ha[5],nstem_FIN$nstem_ha[6],nstem_FIN$nstem_ha[7],nstem_FIN$nstem_ha[8],
                  nstem_FIN$nstem_ha[9],nstem_FIN$nstem_ha[10],nstem_FIN$nstem_ha[11],nstem_FIN$nstem_ha[12],
                  nstem_FIN$nstem_ha[13],nstem_FIN$nstem_ha[14],nstem_FIN$nstem_ha[15],nstem_FIN$nstem_ha[16])
) 
save(ddf_obs, file = "~/Documents/Collaborations/DBEN/inputs_mod/ddf_obs.RData")
