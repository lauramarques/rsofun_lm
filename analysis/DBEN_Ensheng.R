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

# Ensheng simulations ####
p0_FIN_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_P0_FIN_aCO2_Ecosystem_yearly.csv")
p0_FIN_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_FIN/BiomeE_P0_FIN_aCO2_Cohort_yearly.csv")

p0_BIA_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_BIA/BiomeE_P0_BIA_aCO2_Ecosystem_yearly.csv")
p0_BIA_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_BIA/BiomeE_P0_BIA_aCO2_Cohort_yearly.csv")

p0_BCI_co2A_out_annual_tile    <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_BCI/BiomeE_P0_BCI_aCO2_Ecosystem_yearly.csv")
p0_BCI_co2A_out_annual_cohorts <- read.csv("/home/laura/Documents/Collaborations/DBEN/Ensheng/DBEN-BiomeE/BiomeE_BCI/BiomeE_P0_BCI_aCO2_Cohort_yearly.csv")

#RColorBrewer::brewer.pal(8, "Set1")

# Plant C (Biomass) ####
p0_FIN_co2A_out_annual_tile %>% 
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() 

p0_BIA_co2A_out_annual_tile %>% 
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() 

p0_BCI_co2A_out_annual_tile %>% 
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "t", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme_classic() 

# Basal area ####
p0_FIN_co2A_out_annual_cohorts %>% #
  group_by(PFT,yr) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*Density)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = sumBA,col=PFT)) +
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_classic()

p0_BIA_co2A_out_annual_cohorts %>% #
  group_by(PFT,yr) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*Density)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = sumBA,col=PFT)) +
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_classic()

p0_BCI_co2A_out_annual_cohorts %>% #
  group_by(PFT,yr) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*Density)) %>% 
  mutate(PFT=as.factor(PFT)) %>%
  ggplot() + 
  geom_line(aes(x = yr, y = sumBA,col=PFT)) +
  labs(x = "t", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  theme_classic()

# All plots ####
# Plant C (Biomass) ####
fig1a <- p0_FIN_co2A_out_annual_tile %>% #filter(year>510) %>%
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
fig1b <- p0_FIN_co2A_out_annual_tile %>% #filter(year>510) %>%
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
fig1d <- p0_FIN_co2A_out_annual_tile %>% #filter(year>510) %>%
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
fig2a <- p0_FIN_co2A_out_annual_cohorts %>% #filter(year>510) %>%
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





