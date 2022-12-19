library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(patchwork)
library(multidplyr)

if(!require(devtools)){install.packages(devtools)}
devtools::install_github("computationales/ingestr")
library(ingestr)

# Precipitation (kg/m2 = mm)
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/forcing_sel_sites_precip.RData")
precip
precip <- precip %>% rownames_to_column(var="date") %>% 
  
# Relative humidity ()
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/forcing_sel_sites_rhum.RData")
rhum
# Air temperature (K)
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/forcing_sel_sites_temp.RData")
temp
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/forcing_sel_sites_tswrf.RData")
# Total surface downwelling radiation (W/m2)
tswrf 
# Wind speed (m/s)
load("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/forcing_sel_sites_wind.RData")
wind
# CO2


# Order of years:
order_years <- c(2014 ,1996, 2010, 1998, 1991, 2002, 2012, 2016, 2017, 1999, 2008, 2001, 2015, 2019, 
                 2006, 1993, 1994, 2020, 2000, 2004, 1992, 1995, 2018, 2011, 2005, 2007, 2013, 2009, 1997, 2003)
