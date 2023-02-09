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

# BiomeE_PS_FIN_aCO2 (412 ppm) ####
BiomeE_PS1_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS1_FIN_aCO2_annual_tile.csv")
BiomeE_PS1_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS1_FIN_aCO2_annual_cohorts.csv")

BiomeE_PS2_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS2_FIN_aCO2_annual_tile.csv")
BiomeE_PS2_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS2_FIN_aCO2_annual_cohorts.csv")

BiomeE_PS3_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS3_FIN_aCO2_annual_tile.csv")
BiomeE_PS3_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS3_FIN_aCO2_annual_cohorts.csv")

BiomeE_PS4_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS4_FIN_aCO2_annual_tile.csv")
BiomeE_PS4_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS4_FIN_aCO2_annual_cohorts.csv")

BiomeE_PS5_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS5_FIN_aCO2_annual_tile.csv")
BiomeE_PS5_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS5_FIN_aCO2_annual_cohorts.csv")

BiomeE_PS6_FIN_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS6_FIN_aCO2_annual_tile.csv")
BiomeE_PS6_FIN_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/BiomeE_PS6_FIN_aCO2_annual_cohorts.csv")

#RColorBrewer::brewer.pal(8, "Set1")

## Plant C (Biomass) ####
fig1a <- BiomeE_P0_FIN_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fig1a

## GPP ####
fig1b <- BiomeE_P0_FIN_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=GPP),col="#377EB8") + 
  labs(x = "year", y = expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fig1b

## Rauto ####
fig1c <- BiomeE_P0_FIN_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=Rauto),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Rauto (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fig1c

## Soil C ####
fig1d <- BiomeE_P0_FIN_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=soilC),col="#377EB8") + 
  labs(x = "year", y =expression(paste("Soil C (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fig1d

(fig1a + fig1b)/(fig1c + fig1d)

# POOLS ####
## Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
figcveg <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cveg,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass in vegetation (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figcveg

## Aboveground biomass ####
# AGB
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
figAGB <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGB = NSC+leafC+SapwoodC+WoodC) %>%
  select(year, AGB)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGB)) +
  labs(x = "year", y = expression(paste("Aboveground biomass (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
figAGB

## Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
figcwood <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cwood,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass in wood (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figcwood

## Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
figcwood_size <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>%
  filter(dbh_bins!="(0,1]"&dbh_bins!="(1,5]") %>%
  ggplot() + 
  geom_line(aes(x = year, y = cwood_size,col=dbh_bins)) +
  labs(x = "year", y = expression(paste("Carbon mass in wood (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
figcwood_size

## Stem number by size class ####
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
fignstem_size <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,20,30,40,50,60,70,80,90,100,150,200))) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% filter(dbh_bins!="(0,1]"&dbh_bins!="(1,5]") %>%
  ggplot() + 
  geom_line(aes(x = year, y = nstem_size,col=dbh_bins)) +
  labs(x = "year", y = expression(paste("Stem number (count ", ha^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fignstem_size

(figcveg + figcwood)/(figcwood_size + fignstem_size) + plot_layout(guides = "collect")

## Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
figlai <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = lai,col=PFT)) +
  labs(x = "year", y = expression(paste("Leaf area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figlai

## Crown area ####
# CA
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
figCA <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  labs(x = "year", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figCA

## Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
figBA <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = BA,col=PFT)) +
  labs(x = "year", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figBA

## 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
figheight <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = height,col=PFT)) +
  labs(x = "year", y = "95th Height (m)") + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figheight

(figlai + figCA)/(figBA + figheight) + plot_layout(guides = "collect")

# FLUXES ####
## Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
figWBgrowth <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=WBgrowth,col=PFT)) +
  labs(x = "year", y = expression(paste("Woody biomass growth (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figWBgrowth

## Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
figBAgrowth <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=BAgrowth,col=PFT)) +
  labs(x = "year", y = expression(paste("Basal area growth (", m^2, " ",ha^-1, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figBAgrowth

## Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C ha-1 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
figcmort <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cmort,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass flux lost (kg C ", ha^-1, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figcmort

## Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count ha-1 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, pft
# cohort output
figstemmort <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(stemmort=sum(n_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = stemmort,col=PFT)) +
  labs(x = "year", y = expression(paste("Stem number flux lost (count ", ha^-1, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figstemmort

(figWBgrowth + figBAgrowth)/(figcmort + figstemmort) + plot_layout(guides = "collect")

## GPP - Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
figgpp <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = gpp,col=PFT)) +
  labs(x = "year", y = expression(paste("Gross Primary Production (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figgpp

## NPP - Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
fignpp <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = npp,col=PFT)) +
  labs(x = "year", y = expression(paste("Net Primary Production (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
fignpp

## Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
fignbp <- BiomeE_P0_FIN_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) %>%
  ggplot() + 
  geom_line(aes(x = year, y = nbp),col="#377EB8") +
  labs(x = "year", y = expression(paste("Net Biospheric Production (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fignbp

figgpp + fignpp + fignbp + plot_layout(guides = "collect")






# Ensheng simulations ####
p0_FIN_co2A_out_annual_tile    <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_P0_FIN_aCO2_Ecosystem_yearly.csv")
p0_FIN_co2A_out_annual_cohorts <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_P0_FIN_aCO2_Cohort_yearly.csv")

pS_FIN_co2A_01_out_annual_tile    <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_PS_FIN_aCO2_01_Ecosystem_yearly.csv")
pS_FIN_co2A_01_out_annual_cohorts <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_PS_FIN_aCO2_01_Cohort_yearly.csv")

pS_FIN_co2A_02_out_annual_tile    <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_PS_FIN_aCO2_02_Ecosystem_yearly.csv")
pS_FIN_co2A_02_out_annual_cohorts <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_PS_FIN_aCO2_02_Cohort_yearly.csv")

pS_FIN_co2A_20_out_annual_tile    <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_PS_FIN_aCO2_20_Ecosystem_yearly.csv")
pS_FIN_co2A_20_out_annual_cohorts <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_PS_FIN_aCO2_20_Cohort_yearly.csv")

pS_FIN_co2A_40_out_annual_tile    <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_PS_FIN_aCO2_40_Ecosystem_yearly.csv")
pS_FIN_co2A_40_out_annual_cohorts <- read.csv("~/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE_FIN/BiomeE_PS_FIN_aCO2_40_Cohort_yearly.csv")

#RColorBrewer::brewer.pal(8, "Set1")

# Plant C (Biomass) ####
fig1a <- pS_FIN_co2A_40_out_annual_tile %>% #filter(year>510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1a

# GPP ####
fig1b <- pS_FIN_co2A_01_out_annual_tile %>% #filter(year>510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=GPP),col="#377EB8") + 
  labs(x = "t", y = expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1b

# Rauto ####
fig1c <- p0_FIN_co2A_out_annual_tile %>% #filter(year>510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=Rauto),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Rauto (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1c

# Soil C ####
fig1d <- pS_FIN_co2A_01_out_annual_tile %>% #filter(year>510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=soilC),col="#377EB8") + 
  labs(x = "t", y =expression(paste("Soil C (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1d

# Basal area ####
fig2a <- pS_FIN_co2A_40_out_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,yr) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*Density)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = sumBA,col=PFT)) +
  #geom_smooth(aes(x=year, y=sumBA, color=PFT),se=F,size=.5) + 
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .20),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2a

# Crown area ####
fig2b <- p0_FIN_co2A_out_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,yr) %>%
  summarise(CrownArea=sum(Acrown*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = CrownArea,col=PFT)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .80),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2b

# Leaf area ####
fig2c <- p0_FIN_co2A_out_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,yr) %>%
  summarise(LeafArea=sum(Aleaf*Density/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = LeafArea,col=PFT)) +
  labs(x = "t", y = expression(paste("LEaf area (", m^-2, " ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .80),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2c

# Biomass ####
fig2d <- p0_FIN_co2A_out_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,yr) %>%
  summarise(biomass=sum((bl*Density + br*Density + bSW*Density + bHW*Density
                         + seed*Density + nsc*Density)/10000)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = biomass ,col=PFT)) +
  labs(x = "t", y = expression(paste("Biomass (Kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .20),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2d

# Stems_ha ####
fig3a <- p0_FIN_co2A_out_annual_cohorts %>% 
  mutate(dbh_bins = cut(dbh, breaks = c(0.0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.5))) %>%
  group_by(dbh_bins) %>%
  summarise(stems=sum(Density)) %>% filter(dbh_bins!="(0,0.1]") %>%
  ggplot() + 
  geom_point(aes(x = dbh_bins, y = stems)) +
  labs(x = "t", y = expression(paste("Biomass (Kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .20),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Needleleaf1","Needleleaf2","Broadleaf")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig3a

# Figures from variables ####

  
  
  