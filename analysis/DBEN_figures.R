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
fig1a <- BiomeE_PS6_FIN_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_PS6_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
  #scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
  #scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1a

## GPP ####
fig1b <- BiomeE_P0_FIN_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=GPP),col="#377EB8") + 
  labs(x = "t", y = expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1b

## Rauto ####
fig1c <- BiomeE_P0_FIN_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=Rauto),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Rauto (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1c

## Soil C ####
fig1d <- BiomeE_P0_FIN_aCO2_annual_tile %>% #filter(year>510) %>%
  slice(510+1:nrow(BiomeE_P0_FIN_aCO2_annual_tile)) %>% 
  mutate(year = 1:450) %>%
  ggplot() + 
  geom_line(aes(x=year, y=soilC),col="#377EB8") + 
  labs(x = "t", y =expression(paste("Soil C (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10)) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig1d

## Basal area ####
fig2a <- BiomeE_PS6_FIN_aCO2_annual_cohorts%>% #filter(year>510) %>%
  group_by(PFT,year) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = sumBA,col=PFT)) +
  #geom_smooth(aes(x=year, y=sumBA, color=PFT),se=F,size=.5) + 
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_classic() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2a

## Crown area ####
fig2b <- BiomeE_PS6_FIN_aCO2_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,year) %>%
  summarise(CrownArea=sum(Acrown*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = CrownArea,col=PFT)) +
  labs(x = "t", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
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
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2b

## Wood biomass ####
fig2c <- BiomeE_P0_FIN_aCO2_annual_cohorts %>% #filter(year>510) %>%
  group_by(PFT,year) %>%
  summarise(wood=sum(wood*density/10000)) %>% 
  filter(year>510) %>%
  mutate(PFT=as.factor(PFT)) %>%
  mutate(year = year-510) %>%
  ggplot() + 
  geom_line(aes(x = year, y = wood,col=PFT)) +
  labs(x = "t", y = expression(paste("Wood biomass (Kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2")) #+
#scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
#scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2c

# Save plots ####
fig1a/fig2a/fig2b 



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


  
  
  