# This script calibrates the model for the target variables selected

# load packages
library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(multidplyr)

# benchmark ####

# FIN ####

## Regrowth curves ####
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_FIN <- benchmark_regrowth %>% filter(Biome=="Boreal")
plot_regrowth_bench <- ggplot(regrowth_dynamics_FIN,aes(x=bin_num,y=AGcwood_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_point(col="blue") + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(0,210)) + scale_y_continuous(lim=c(0,18)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_regrowth_bench

plot_regrowth_out <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGB = NSC+leafC+SapwoodC+WoodC) %>%
  select(year, AGB)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGB)) + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(31,100)) + scale_y_continuous(lim=c(0,18)) +
  theme_classic() + ggtitle("Model output")
plot_regrowth_out

plot_regrowth_out + plot_regrowth_bench

## Biomass ####
# AGcwood (aboveground woody carbon) 
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_FIN <- benchmark_biomass %>% filter(site=="FI")
plot_biomass_bench <- ggplot(benchmark_biomass_FIN,aes(x=Year,y=AGB_kgCm2)) + 
  geom_errorbar(aes(ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_point(col="blue") + geom_hline(yintercept=10, col="grey") +
  scale_y_continuous(lim=c(4,15)) + theme_classic() + ggtitle("Benchmark target")
plot_biomass_bench

plot_biomass_out <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGB = SapwoodC+WoodC) %>%
  select(year, AGB)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGB)) + geom_hline(yintercept=10, col="grey") +
  scale_x_continuous(lim=c(200,450)) + scale_y_continuous(lim=c(4,15)) + 
  theme_classic() + ggtitle("Model output")
plot_biomass_out

plot_biomass_out + plot_biomass_bench

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_FIN <- stand_structure %>% filter(site=="FI")
### N stems size ####
plot_stems_bench <- ggplot(stand_structure_FIN,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point(col="blue") + scale_x_continuous(lim=c(0,100),breaks = seq(0,100,10)) +
  scale_y_continuous(lim=c(0,750)) +
  theme_classic() + ggtitle("Benchmark target")
plot_stems_bench

plot_stems_out <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>%
  group_by(dbh_bins) %>%
  summarise(nstem_size=mean(nstem_size)) %>% mutate(dbh_class_num = c(1,5,10,20,30,40,50,60)) %>%
  ggplot() + 
  geom_point(aes(x = dbh_class_num, y = nstem_size)) + 
  scale_x_continuous(lim=c(0,60),breaks = seq(0,60,10)) +
  scale_y_continuous(lim=c(0,750)) + 
  theme_classic() + ggtitle("Model output")
plot_stems_out

plot_stems_out + plot_stems_bench

### AGcwood size ####
# AGcwood (aboveground woody carbon) 
plot_cwood_bench <- ggplot(stand_structure_FIN,aes(x=dbh_classes_num,y=AGB_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point(col="blue") + scale_y_continuous(limits = c(0,6.5)) + 
  theme_classic() + ggtitle("Benchmark target")
plot_cwood_bench

plot_cwood_out <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>%
  group_by(dbh_bins) %>%
  summarise(cwood_size=mean(cwood_size)) %>% mutate(dbh_class_num = c(1,5,10,20,30,40,50,60)) %>%
  ggplot() + 
  geom_point(aes(x = dbh_class_num, y = cwood_size)) + 
  scale_y_continuous(limits = c(0,6.5)) + 
  theme_classic() + ggtitle("Model output")
plot_cwood_out

plot_cwood_out + plot_cwood_bench

# BIA ####

## Regrowth curves ####
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_BIA <- benchmark_regrowth %>% filter(Biome=="Temperate")
ggplot(regrowth_dynamics_BIA,aes(x=bin_num,y=AGcwood_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_line(col="blue") + scale_y_continuous(limits = c(0,35)) + geom_hline(yintercept=10, col="grey")

## Biomass ####
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_BIA <- benchmark_biomass %>% filter(site=="BIA")
ggplot(benchmark_biomass_BIA,aes(x=Year,y=AGB_kgCm2)) + 
  geom_errorbar(aes(ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,35)) + geom_hline(yintercept=10, col="grey")

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_BIA <- stand_structure %>% filter(site=="BIA")
ggplot(stand_structure_BIA,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,600)) 
ggplot(stand_structure_BIA,aes(x=dbh_classes_num,y=log(nstem_size_ha.1))) + 
  geom_errorbar(aes(ymin=log(nstem_size_lower_ha.1), ymax=log(nstem_size_upper_ha.1)), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,10))
ggplot(stand_structure_BIA,aes(x=dbh_classes_num,y=AGB_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,6.5)) 

# BCI ####

## Regrowth curves ####
benchmark_regrowth <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_regrowth_curves.csv")
benchmark_regrowth_BCI <- benchmark_regrowth %>% filter(Biome=="Tropics")
ggplot(regrowth_dynamics_BCI,aes(x=bin_num,y=AGcwood_kgCm2_med)) + 
  geom_errorbar(aes(ymin=AGcwood_kgCm2_10, ymax=AGcwood_kgCm2_90), width=.2, col="blue") + 
  geom_line(col="blue") + scale_y_continuous(limits = c(0,35)) + geom_hline(yintercept=10, col="grey")

## Biomass ####
benchmark_biomass <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_eq_dynamics.csv")
benchmark_biomass_BCI <- benchmark_biomass %>% filter(site=="BCI")
ggplot(benchmark_biomass_BCI,aes(x=Year,y=AGB_kgCm2)) + 
  geom_errorbar(aes(ymin=AGB_lower_kgCm2, ymax=AGB_upper_kgCm2), width=.2, col="blue") + 
  geom_point() + scale_y_continuous(limits = c(0,35)) + geom_hline(yintercept=10, col="grey")

## Stand structure ####
stand_structure <- read.csv("/home/laura/Documents/Collaborations/DBEN/DBEN_site_simulations_perspective_paper/Benchmarking_datasets/updated/benchmark_stand_structure.csv")
stand_structure_BCI <- stand_structure %>% filter(site=="BCI")
ggplot(stand_structure_BCI,aes(x=dbh_classes_num,y=nstem_size_ha.1)) + 
  geom_errorbar(aes(ymin=nstem_size_lower_ha.1, ymax=nstem_size_upper_ha.1), width=.2, col="blue") + 
  geom_point() 
ggplot(stand_structure_BCI,aes(x=dbh_classes_num,y=log(nstem_size_ha.1))) + 
  geom_errorbar(aes(ymin=log(nstem_size_lower_ha.1), ymax=log(nstem_size_upper_ha.1)), width=.2, col="blue") + 
  geom_point() 
ggplot(stand_structure_BCI,aes(x=dbh_classes_num,y=AGB_size_kgCm.2)) + 
  geom_errorbar(aes(ymin=AGB_size_lower_kgCm.2, ymax=AGB_size_upper_kgCm.2), width=.2, col="blue") + 
  geom_point() 







# Calibration ####
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
