#########################################
#          HurdleQap Analysis           #
###        Bielefeld Data            ####
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
#### Database data

# Data preparation 1
source("RCode/Bielefeld/01_BibliometrixAnalysis_Bielefeld.R", echo = TRUE)

# Distance Matrix
source("RCode/Bielefeld/02_DistanceMatrices_Bielefeld.R", echo = TRUE)

# Cluster Analysis
source("RCode/Bielefeld/03_ClusterAnalysis_Bielefeld.R", echo = TRUE)

# Data preparation 2 
source("RCode/Bielefeld/04_DataPreparation2_Bielefeld.R", echo = TRUE)

# Descriptive Network Analysis 
source("RCode/Bielefeld/05_NetworkAnalysis_Bielefeld.R", echo = TRUE)
save.image("Workspaces/CompleteAnalysis_Bielefeld.Rdata")

# Network Modeling: preparation for QapGamGlm calculations
source("RCode/Bielefeld/Server/QapGamGlm_preparation_Bielefeld.R", echo = TRUE)


# for calculating all models, run the code files in the folder "Server".



