##################################
### LASSA VACCINE HOUSEKEEPING ###
##################################

################
### PACKAGES ###
################

library(tidyverse)
library(readxl)
library(scales)
library(ggsci)
library(cowplot)
library(ggpubr)
library(meta)
library(hesim)
library(metafor)
library(boot)
library(fitdistrplus)
library(purrr)
library(mgcv)
library(officer)
library(flextable)
library(epiR)
library(this.path)
library(metR)
library(bcaboot)
library(zoo)


#############################
### NUMBER OF SIMULATIONS ###
#############################

n_draws_stoch = 500

GID_1_final = c("NGA.5_1", "NGA.7_1", "NGA.10_1", "NGA.11_1", "NGA.12_1", "NGA.14_1", 
                "NGA.16_1", "NGA.19_1", "NGA.23_1", "NGA.26_1", "NGA.29_1", "NGA.31_1", 
                "NGA.32_1", "NGA.35_1", "LBR.2_1", "LBR.5_1", "LBR.12_1", "GIN.8_1", 
                "SLE.1_1")


###########################
### COLOURS AND FACTORS ###
###########################

vec_cols_cases = c('#f7fcf0','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081')
#vec_cols_snhl = c('#fff7f3','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')
vec_cols_snhl = c('#f7fcfd','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6','#8c6bb1','#88419d','#810f7c')
vec_cols_deaths = c('#fff7ec','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
vec_cols_dalys = c('#d53e4f','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#084081')
vec_cols_dalysPerDose = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e')
vec_cols_turnover = c('#bebada', '#8dd3c7', '#fdb462')
vec_cols_attrition = c('#b3e2cd','#fdcdac','#cbd5e8')
vec_cols_VE = c('#f4cae4','#e6f5c9','#fff2ae')


cols_states = pal_simpsons()(15)

### STATE LABELS
vec_endemicStates = c("Bauchi", "Benue", "Delta", "Ebonyi", "Edo", "Enugu", "Gombe", "Kaduna", "Kogi", "Nasarawa", 
               "Ondo", "Oyo", "Plateau", "Taraba")

### VACCINE CHARACTERISTIC LABELS 
vec_labels_risk = c("1% annual outbreak risk", "5% annual outbreak risk", "10% annual outbreak risk", "25% annual outbreak risk", "50% annual outbreak risk")

vec_levels_VE = c(0.5, 0.7, 0.9)
vec_labels_VE = c("50%", "70%", "90%")

vec_labels_turnover = c("5% annual turnover", "15% annual turnover", "25% annual turnover")



#######################
### HANDY FUNCTIONS ###
#######################

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}




