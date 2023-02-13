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

# 412 ppm ####

# P0 - Baseline run BIA  ####
BiomeE_P0_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_tile.csv")
BiomeE_P0_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_cohorts.csv")

#RColorBrewer::brewer.pal(8, "Set1")

## Plant C (Biomass) ####
fig1a <- BiomeE_P0_BIA_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fig1a

## GPP ####
fig1b <- BiomeE_P0_BIA_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=GPP),col="#377EB8") + 
  labs(x = "year", y = expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fig1b

## Rauto ####
fig1c <- BiomeE_P0_BIA_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=Rauto),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Rauto (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fig1c

## Soil C ####
fig1d <- BiomeE_P0_BIA_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=soilC),col="#377EB8") + 
  labs(x = "year", y =expression(paste("Soil C (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fig1d

(fig1a + fig1b)/(fig1c + fig1d) 
#ggsave("~/rsofun/data/figures/BiomeEP_P0_BIA_412ppm_EcosystemC.png", width = 8, height = 5.5, dpi=300)
ggsave("~/rsofun/data/figures/BiomeEP_P0_BIA_412ppm_EcosystemC.pdf", width = 8, height = 5.5, dpi=300)

# POOLS ####
## Carbon mass in vegetation by PFT ####
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
figcveg <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cveg,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass in vegetation (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figcveg

## Aboveground biomass ####
# AGB
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
figAGB <- BiomeE_P0_BIA_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, AGB = NSC+leafC+(SapwoodC+WoodC)*0.75) %>%
  select(year, AGB)  %>%
  ggplot() + 
  geom_line(aes(x = year, y = AGB),col="#377EB8") +
  labs(x = "year", y = expression(paste("Aboveground biomass (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
figAGB

## Carbon mass in wood by PFT ####
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
figcwood <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cwood=sum((sapwC+woodC)*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cwood,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass in wood (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figcwood

## Carbon mass in wood by size class ####
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
figcwood_size <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) %>%
  filter(dbh_bins!="[0,1)"&dbh_bins!="[1,5)") %>%
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
fignstem_size <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(nstem_size=sum(density)) %>% filter(dbh_bins!="[0,1)"&dbh_bins!="[1,5)") %>%
  ggplot() + 
  geom_line(aes(x = year, y = nstem_size,col=dbh_bins)) +
  labs(x = "year", y = expression(paste("Stem number (count ", ha^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
fignstem_size

(figcveg + figcwood)/(figcwood_size + fignstem_size) + 
  plot_layout(guides = "collect") & theme(legend.position = 'right')
ggsave("~/rsofun/data/figures/BiomeEP_P0_BIA_412ppm_PoolsSize.pdf", width = 8, height = 5.5, dpi=300)

## Leaf area index ####
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
figlai <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(lai=sum(Aleaf*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = lai,col=PFT)) +
  labs(x = "year", y = expression(paste("Leaf area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figlai

## Crown area ####
# CA
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
figCA <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(CA=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  labs(x = "year", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figCA

## Basal area ####
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
figBA <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
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
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figBA

## 95th percentile of tree height ####
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
figheight <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(height=quantile(height, probs = 0.95)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = height,col=PFT)) +
  labs(x = "year", y = "95th Height (m)") + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figheight

(figlai + figCA)/(figBA + figheight) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_BIA_412ppm_PoolsStructure.pdf", width = 8, height = 5.5, dpi=300)

# FLUXES ####
## Woody biomass growth ####
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
figWBgrowth <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=WBgrowth,col=PFT)) +
  labs(x = "year", y = expression(paste("Woody biomass growth (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figWBgrowth

## Basal area growth ####
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
figBAgrowth <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) %>%
  #summarise(BAgrowth=sum(dBA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x=year, y=BAgrowth,col=PFT)) +
  labs(x = "year", y = expression(paste("Basal area growth (", m^2, " ",ha^-1, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figBAgrowth

## Carbon Mass Flux lost from live wood due to mortality or other turnover process ####
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
figcmort <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cmort,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass flux lost (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figcmort

figcmort_size <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(cmort=sum(c_deadtrees)) %>% filter(dbh_bins!="[0,1)"&dbh_bins!="[1,5)") %>%
  ggplot() + 
  geom_line(aes(x = year, y = cmort,col=dbh_bins)) +
  labs(x = "year", y = expression(paste("Carbon mass flux lost (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
figcmort_size

## Stem number Flux lost from vegetation due to mortality or other turnover process ####
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, pft
# cohort output
figstemmort <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  group_by(PFT,year) %>%
  #summarise(stemmort=sum(n_deadtrees)) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = stemmort,col=PFT)) +
  labs(x = "year", y = expression(paste("Stem number flux lost (count ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figstemmort

figstemmort_size <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) %>%
  filter(year>510) %>%
  mutate(year = year-510) %>%
  group_by(dbh_bins,year) %>%
  summarise(stemmort=sum(deathrate*density/10000)) %>% filter(dbh_bins!="[0,1)"&dbh_bins!="[1,5)") %>%
  ggplot() + 
  geom_line(aes(x = year, y = stemmort,col=dbh_bins)) +
  labs(x = "year", y = expression(paste("Stem number flux lost (count ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
figstemmort_size

(figWBgrowth + figBAgrowth)/(figcmort + figstemmort) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_BIA_412ppm_FluxesdBAMortality.pdf", width = 8, height = 5.5, dpi=300)

## GPP - Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ####
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
figgpp <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(gpp=sum(GPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = gpp,col=PFT)) +
  labs(x = "year", y = expression(paste("Gross Primary Production (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
figgpp

## NPP - Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ####
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
fignpp <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = npp,col=PFT)) +
  labs(x = "year", y = expression(paste("Net Primary Production (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_discrete(labels = c("Grasses","Tilia cordata","Betula pendula","Picea abies"))
fignpp

## Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ####
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
fignbp <- BiomeE_P0_BIA_aCO2_annual_tile %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450, nbp = GPP-Rauto-Rh) %>%
  select(year, nbp) %>%
  ggplot() + 
  geom_line(aes(x = year, y = nbp),col="#377EB8") +
  labs(x = "year", y = expression(paste("Net Biospheric Production (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
fignbp

figgpp + fignpp + fignbp + plot_layout(guides = "collect") + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/rsofun/data/figures/BiomeEP_P0_BIA_412ppm_FluxesGrowth.pdf", width = 8, height = 3.5, dpi=300)

# PS - Sensitivity runs BIA ####
BiomeE_PS1_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_annual_tile.csv")
BiomeE_PS1_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_annual_cohorts.csv")

BiomeE_PS2_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_annual_tile.csv")
BiomeE_PS2_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_annual_cohorts.csv")

BiomeE_PS3_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_annual_tile.csv")
BiomeE_PS3_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_annual_cohorts.csv")

BiomeE_PS4_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_annual_tile.csv")
BiomeE_PS4_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_annual_cohorts.csv")

BiomeE_PS5_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_annual_tile.csv")
BiomeE_PS5_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_annual_cohorts.csv")

BiomeE_PS6_BIA_aCO2_annual_tile    <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_annual_tile.csv")
BiomeE_PS6_BIA_aCO2_annual_cohorts <- read.csv("~/rsofun/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_annual_cohorts.csv")


figBio_P0 <- BiomeE_P0_BIA_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_BIA_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  #theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_blank()) 
figBio_P0

figBA_P0 <- BiomeE_P0_BIA_aCO2_annual_cohorts %>% 
  mutate(PFT=as.factor(PFT)) %>%
  group_by(PFT,year) %>%
  #summarise(BA=sum(dbh*dbh*pi/4*density/10000)) %>%
  summarise(BA=sum(BA*density)) %>% 
  filter(year>510) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = BA,col=PFT)) +
  labs(x = "year", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  #theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_blank()) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))
figBA_P0

figure_freq <- figBio_P0 + figBA_P0 + figBio_PS1 + figBA_PS1 + figBio_PS2 + figBA_PS2 +
  figBio_PS3 + figBA_PS3 +  figBio_PS4 + figBA_PS4 +  figBio_PS5 + figBA_PS5 +
  figBio_PS6 + figBA_PS6 +
  plot_layout(ncol = 2) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
figure_freq
ggsave("~/rsofun/data/figures/figure_freq.png", width = 6, height = 9, dpi=300)

