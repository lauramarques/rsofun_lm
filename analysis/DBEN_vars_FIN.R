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
library(tidyverse)

# Outputs ####

# BiomeE_P0_FIN_aCO2 (412 ppm) ####
BiomeE_P0_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_tile.csv")
BiomeE_P0_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_cohorts.csv")

BiomeE_PS6_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_FIN_aCO2_annual_tile.csv")
BiomeE_PS6_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_FIN_aCO2_annual_cohorts.csv")

# Ensheng simulations ####
p0_FIN_co2A_out_annual_tile    <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_P0_FIN_aCO2_Ecosystem_yearly.csv")
p0_FIN_co2A_out_annual_cohorts <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_P0_FIN_aCO2_Cohort_yearly.csv")

PFT_species <- c("Pinus_sylvestris","Picea_abies","Betula_pendula","Grasses") 
PFT_reorder <- c(1,2,3,4)
PFT <- c(4,3,2,1)
PFT_FIN <- tibble(PFT_species,PFT_reorder,PFT)

# POOLS ####
# 1. Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
cveg_wid <- cveg %>% select(c(year,cveg,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_cveg_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in vegetation by PFT",prec="single")
# create netCDF file and put arrays
cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
# put variables
#cveg_array <- array(-999, dim=c(length(time),length(pft)))
cveg_array <- simplify2array(cveg_wid)
ncvar_put(cveg_ncout,cveg_var,cveg_array)
# Get a summary of the created file
cveg_ncout
# close the file, writing data to disk
nc_close(cveg_ncout)

# 2. Aboveground biomass ####
# AGB
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
AGB <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGB = NSC+leafC+(SapwoodC+WoodC)*0.75) %>%
  select(year, AGB) 
AGB_wid <- AGB %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_AGB_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGB_var <- ncvar_def("AGB","kg C m-2",list(time_dim),-999,
                      "Aboveground biomass",prec="single")
# create netCDF file and put arrays
AGB_ncout <- nc_create(ncfname,list(AGB_var),force_v4=TRUE)
# put variables
AGB_array <- simplify2array(AGB_wid)
ncvar_put(AGB_ncout,AGB_var,AGB_array)
# Get a summary of the created file
AGB_ncout
# close the file, writing data to disk
nc_close(AGB_ncout)

# 3. Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
cwood_wid <- cwood %>% select(c(year,cwood,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_cwood_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                      "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)

# 4. Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) 

# 5. Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) 

# 6. Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.double(PFT)) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
lai_wid <- lai %>% select(c(year,lai,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_lai_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                       "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)

# 7. Crown area ####
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
CA_wid <- CA %>% select(c(year,CA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_CA_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                     "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)

# 8. Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
BA_wid <- BA %>% select(c(year,BA,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_BA_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)

# 9. 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
height_wid <- height %>% select(c(year,height,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_height_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                    "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)

# FLUXES ####
# 10. Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
WBgrowth_wid <- WBgrowth %>% select(c(year,WBgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_WBgrowth_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                        "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)

# 11. Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
BAgrowth_wid <- BAgrowth %>% select(c(year,BAgrowth,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_BAgrowth_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
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

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count ha-1 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(stemmort=sum(n_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) 

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
gpp_wid <- gpp %>% select(c(year,gpp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_gpp_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.double(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>% left_join(PFT_FIN) %>% ungroup()
npp_wid <- npp %>% select(c(year,npp,PFT_reorder)) %>% rename(PFT=PFT_reorder) %>%
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) %>% arrange(year) %>%
  relocate(`1`,.after =year) %>% relocate(`2`,.after =`1`) %>% relocate(`3`,.after =`2`) %>%
  select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_npp_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
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
nbp_wid <- nbp %>% select(-year)
# create the netCDF filename 
ncfname <- paste("~/rsofun/data/outputs_mod/nc_files/412ppm/FIN/", 
                 "BiomeEP_nbp_P0_FIN", ".nc", sep="")
# define dimensions
time <- as.array(seq(1,450,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)





# Convert to netCDF file ####

library(ncdf4)
# open a netCDF file
ex_output <- nc_open("/home/laura/Documents/Collaborations/DBEN/example_output_for_netcdf_formatting/LPJGUESS_BA_S1_FIN2_fullcolumn.nc")
print(ex_output)
attributes(ex_output$var)
attributes(ex_output$dim)

# get dimensions
Time <- ncvar_get(ex_output, "Time")
pft <- ncvar_get(ex_output, "pft")

# get variables
ba_var <- ncvar_get(ex_output,"BA")
dlname <- ncatt_get(ex_output,"BA","long_name")
dunits <- ncatt_get(ex_output,"BA","units")
fillvalue <- ncatt_get(ex_output,"BA","_FillValue")
dim(tmp_array)


library(ncdf4)

cveg_wid
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time

# create the netCDF filename ####
# path and file name, set dname
ncpath <- "~/rsofun/data/outputs_mod/nc_files/"
ncname <- "BA_ncdf4"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)

# Define the contents of the file ####
# create and write the netCDF file -- ncdf4 version
# define dimensions
time <- as.array(seq(1901,2015,1))
length(time)
pft <- as.array(seq(1,4,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 

# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,"Basal area",prec="single")

# create netCDF file and put arrays
ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)

# put variables
fillvalue <- -999
BA_array <- array(-999, dim=c(length(time),length(pft)))
ncvar_put(ncout,BA_var,BA_array)

BA



# Get a summary of the created file:
ncout

# close the file, writing data to disk
nc_close(ncout)



# open a netCDF file
ex_output <- nc_open("/home/laura/rsofun/data/outputs_mod/nc_files/cveg_nc.nc")
print(ex_output)
attributes(ex_output$var)
attributes(ex_output$dim)

# get dimensions
Time <- ncvar_get(ex_output, "time")
pft <- ncvar_get(ex_output, "pft")

# get variables
ba_var <- ncvar_get(ex_output,"cveg")
dlname <- ncatt_get(ex_output,"BA","long_name")
dunits <- ncatt_get(ex_output,"BA","units")
fillvalue <- ncatt_get(ex_output,"cveg","_FillValue")

# get variables
var <- ncvar_get(ncout,"cveg")
dlname <- ncatt_get(ex_output,"BA","long_name")
dunits <- ncatt_get(ex_output,"BA","units")
fillvalue <- ncatt_get(ex_output,"BA","_FillValue")
dim(tmp_array)
