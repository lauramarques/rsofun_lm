# This script plots the outputs from the model simulations

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4) 
library(lmerTest) 
library(effects) 
library(MuMIn)
library(sjPlot)
library(ggeffects)
library(patchwork)

# Outputs ####

# BiomeE_P0_FIN_aCO2 (412 ppm) ####
BiomeE_P0_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_P0_FIN_aCO2_annual_tile.csv")
BiomeE_P0_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_P0_FIN_aCO2_annual_cohorts.csv")

BiomeE_PS6_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS6_FIN_aCO2_annual_tile.csv")
BiomeE_PS6_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS6_FIN_aCO2_annual_cohorts.csv")

# Carbon mass in vegetation by PFT ####
# cveg
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time

cveg <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(wood=sum(wood*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = wood,col=PFT))
