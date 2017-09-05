###########################################################################
## PROJECT: L+I predictive modeling
##
## SCRIPT PURPOSE: Compile scripts for separate project components
##    - Sequence scripts in appropriate order
##
## DATE: 30 August 2017
## AUTHOR: Simon Kassel
###########################################################################


# Helper functions
source("R/helper-functions.R")

# Data wrangling and feature engineering
source("R/data-wrangling-and-feature-engineering.R")

# Feature selection
source("R/feature-selection.R")

# Model selection
source("R/model-selection.R")

# Data visualization
source("R/data-vizualization.R")