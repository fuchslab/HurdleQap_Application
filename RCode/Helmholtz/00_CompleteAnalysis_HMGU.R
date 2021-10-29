#########################################
#          HurdleQap Analysis           #
###        Helmholtz Data            ####
#          Hannah Marchi                #
#########################################

# Please follow the instructions and run the explained code files in the given order

# 1) if needed install the following packages and load them
library(bibliometrix)
library(dplyr)
library(RISmed)
library(stringr)
library(readxl)
library(tidyverse)
library(igraph)
library(stringr)
library(DescTools)
library(rmarkdown)

#-----------------------------------------------------------------
# Data preparation 1
source("RCode/Helmholtz/01_BibliometrixAnalysis_HMGU.R", echo = TRUE)

# Distance Matrix
source("RCode/Helmholtz/02_DistanceMatrices_HMGU.R", echo = TRUE)

# Cluster Analysis
source("RCode/Helmholtz/03_ClusterAnalysis_HMGU.R", echo = TRUE)

# Data preparation 2 
source("RCode/Helmholtz/04_DataPreparation2_HMGU.R", echo = TRUE)

# Descriptive Network Analysis 
source("RCode/Helmholtz/05_NetworkAnalysis_HMGU.R", echo = TRUE)
save.image("Workspaces/CompleteAnalysis_HMGU.Rdata")


# Network Modeling: preparation for QapGamGlm calculations
source("RCode/Helmholtz/Server/QapGamGlm_preparation_HMGU.R", echo = TRUE)

# for calculating all models, run the code files in the folder "Server".

