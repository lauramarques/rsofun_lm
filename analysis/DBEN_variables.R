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
library(ncdf4)
library(ncdf4.helpers)

# Outputs ####

# BiomeE_P0_FIN_aCO2 (412 ppm) ####
BiomeE_P0_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_P0_FIN_aCO2_annual_tile.csv")
BiomeE_P0_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_P0_FIN_aCO2_annual_cohorts.csv")

BiomeE_PS6_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS6_FIN_aCO2_annual_tile.csv")
BiomeE_PS6_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS6_FIN_aCO2_annual_cohorts.csv")

# Ensheng simulations ####
p0_FIN_co2A_out_annual_tile    <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_P0_FIN_aCO2_Ecosystem_yearly.csv")
p0_FIN_co2A_out_annual_cohorts <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_P0_FIN_aCO2_Cohort_yearly.csv")

# POOLS ####
# Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cveg,col=PFT))

# Aboveground biomass ####
# AGB
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGB <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGB = NSC+leafC+SapwoodC+WoodC) %>%
  select(year, AGB)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGB))

# Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) 

# Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) 

# Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) 

# Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) 

# Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) 

# Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) 

# 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) 

# FLUXES ####
# Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) 

# Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) 

# Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C ha-1 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) 

# Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count ha-1 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, pft
# cohort output
stemmort <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(stemmort=sum(n_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) 

# Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510)

# Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510)

# Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) 

# Convert to netCDF file ####

ex_output <- nc_open("/home/laura/Documents/Collaborations/DBEN/example_output_for_netcdf_formatting/LPJGUESS_BA_S1_FIN2_fullcolumn.nc")
ex_output <- nc_open("/home/laura/Documents/Collaborations/DBEN/example_output_for_netcdf_formatting/LPJGUESS_nbp_S1_FIN2_fullcolumn.nc")
ex_output <- nc_open("/home/laura/Documents/Collaborations/DBEN/example_output_for_netcdf_formatting/LPJGUESS_cveg_S1_FIN2_fullcolumn.nc")
lat <- ncvar_get(ex_output, varid = "BA")

attributes(ex_output$var)
attributes(ex_output$dim)
Time <- ncvar_get(ex_output, "Time")
pft <- ncvar_get(ex_output, "pft")







library(ncdf4)
#Collect individual columns
xvals <- df$long
yvals <- df$lat
time <- df$time

#Create dimension definition
lon_dim <- ncdim_def("longitude", "degrees_east", xvals)
lat_dim <- ncdim_def("latitude", "degrees_north", yvals)
time_dim <- ncdim_def("time","h",unique(time))

#Define new variable and append with 0
mv =  1.e30
frp <- ncvar_def("frp", "frp", list(lon1,lat2,time_dim), mv)
frp

#Create a netCDF file
ncnew = nc_create("C:\\Users\\Desktop\\my_nc.nc", frp)
ncnew