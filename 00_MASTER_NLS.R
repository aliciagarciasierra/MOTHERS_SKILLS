

##############################################################
### Skills Beget Skills: Addressing the Role of ##############
######## Mothers’ Occupation-Specific Skills ################# 
########## on Children’s Developmental Process ###############
##############################################################

# Author: Alicia García-Sierra 

# Data sets needed:

# 1. NLSY79 Child and Young Adult
# Available to download freely from the NLS investigator.
# You can find all the relevant variables to download in the file
# NLS_tagset.

# 2. O*NET dataset
# Available to download freely from https://www.onetonline.org/ 

# Date of preparation of this script: July 2024

#######################################################
#########   PREPARE THE ENVIRONMENT ################
######################################################

# CLEAN

rm(list=ls()) 

# LOAD PACKAGES

library(haven)
library(plyr)
library(psych)
library(sjlabelled)
library(tidyverse)
library(Hmisc)
library(plm)
library(fixest)
library(dplyr)
library(panelr)
library(scales)
library(WeightIt)
library(clubSandwich)
library(stargazer)
library(ggplot2)
library(readxl)
library(pilot)
library(ggpubr)
library(ggfixest)
library(grid)
library(readxl)
library(data.table)


# SET WD 

setwd("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/THIRD PAPER/Data/data")

# SET THEME FOR GRAPHS

set_pilot_family("Avenir Next Medium", title_family = "Avenir Next Demi Bold")

# SET SIGNIFICANCE LEVELS MARKS

signif_codes <- c( "***" = 0.01, "**" = 0.05, "*" = 0.1)