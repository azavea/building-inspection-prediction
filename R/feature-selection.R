###########################################################################
## PROJECT: L+I predictive modelimg
##
## SCRIPT PURPOSE: Variable importance
##    - use boruta algorithm on all candidate predictor variables to
##      select important predictors
##
## DATE: 18 July 2017
## AUTHOR: Simon Kassel
###########################################################################

# helper functions 
source("R/helper-functions.R")

# source('LNI-analysis-feature-engineering.R')
load("data/full_dataset_for_modeling.Rdata")

# packages
packages(
  c(
    "tidyverse",
    "caret",
    "Boruta"
  )
)

# boruta ------------------------------------------------------------------

# extract just the predictor variable candidates
dd <- ds %>%
  select(one_of(mod_vars))

# partition dataset for boruta
set.seed(12345)
inTrain <- createDataPartition(dd$o.failed.n, p = 0.6, list = FALSE)
boruta.train.set <- dd[inTrain, -c(1,2)]

# run iterative random forrest variable importance test
boruta.train <- Boruta(o.failed ~ ., data = boruta.train.set, doTrace = 2, maxRuns = 50)
# get final decision
fd <- boruta.train$finalDecision

# generate a list of non-useful variables
rejected_vars <- fd[which(fd != "Confirmed")] %>% names

# save output
save(boruta.train.set, rejected_vars, file = "data/boruta.train.Rdata")
