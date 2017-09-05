###########################################################################
## PROJECT: L+I predictive modeling
##
## SCRIPT PURPOSE: Feature engineering
##    - combine violation and inspection datasets
##    - measure before and after outcomes for each inspections
##    - engineer features from license and inspection dataset
##    - create new variables from additional city datasets
##    - remove unnecessary fields
##    - export final dataset for model building
##
## DATE: 18 July 2017
## AUTHOR: Simon Kassel
###########################################################################

# helper functions 
source("R/helper-functions.R")

# packages
packages(
  c("tidyverse",
    "data.table",
    "sp",
    "rgdal",
    "lubridate",
    "plyr",
    "forcats",
    "caret",
    "spatstat",
    "RCurl",
    "jsonlite"
    )
  )


# LOAD AND WRANGLE DATA ---------------------------------------------------

# load spatial datasets
load("data/lni_data.Rdata")

# get coordinates as attributes
insp <- getCoords(insp)
viol <- getCoords(viol)

# join violations and inspections dataset
full_dat <- joinSPdfs(viol, insp, "casenumber")

# clean full data frame:
#   rename fields, creteUID, add a violation year field   
full_dat <- cleanData(full_dat)

# filter the data frame
#   remove some NA fields, closed inspections
filtered_dat <- filterData(full_dat)

# associate each inspection with other inspections in its case
#   measure outcomes of the same case before and after the inspection 
#     in question
filtered_dat <- measureAllFails(filtered_dat)

# REMOVE
# to obtain this dataset dataset:
load("output/data/lni_15to17_withRecent_2017-07-18.Rdata")

# remove the initial inspection instances
df <- filtered_dat %>%
  filter(rec.inspStatus != "none") %>%
  filter(as.Date(violDte) < as.Date(inspCompl)) %>%
  data.table() %>%
  .[, c("organizati", "unit", "apdesc", "aptype") := NULL] %>%
  na.omit()


# FEATURE ENGINEERING -----------------------------------------------------

# create additional variables
df$zipShort <- substr(df$zip, 1, 5) %>% as.factor()
df$failed <- ifelse(df$inspStatus == "Failed", 1, 0)
df$before <- as.factor(df$before)

# add 3 variables indicating durations from previous events in lifecycle of
#   building case
df <- durationVars(df)

# extract 'violation type' variable and remove levels ocurring < 0.02% 
#   of the time
df_violTpe  <- df %>%
  select(apinspkey, violDesc) %>%
  mutate(violTpe = fct_lump(violDesc, p = 0.02)) %>%
  dplyr::rename(id.apinspkey = apinspkey)

# generate a lookup table to get the names of violation types (for later 
#   use)
violType_lookup <- getFactorLevelLookup(df_violTpe$violTpe, "violTpe")

# encode violation-type categorical feature as a series of binary
#   numeric dummy variables
df_violTpe <- castAndSumm(df_violTpe, "id.apinspkey", "violTpe", binary = FALSE)


# SUMMARISE TO THE INSPECTION LEVEL ---------------------------------------

# collapse dataset to the inspection level, summarising 
ds <- summariseInspections(df)

# measure for spatial autocorrelation among repeat violators
ds <- endogMain(ds, nlist = c(5, 10))

# remove infrequently occuring categorical variables
ds$f.inspDesc <- fct_lump(ds$f.inspDesc, prop = 0.02)
ds$f.recStatus <- fct_lump(ds$f.recStatus, prop = 0.02)

# miscellenous feature engineering
ds <- ds %>%
  mutate(f.numFailTypes = as.factor(squishToRange(e.numFailTypes))) %>%
  mutate(f.numViol = as.factor(squishToRange(e.numViol))) %>%
  mutate(f.prevFail = as.factor(squishToRange(e.prevFail))) %>%
  select(-e.numFailTypes, -e.numViol, -e.prevFail) %>%
  filter(is.finite(e.avgDists.repOfAll.n5))


# EXTERNAL DATA SOURCES ---------------------------------------------------

# vacant property indicators
# read dataset
vpi <- read.csv("http://data.phl.opendata.arcgis.com/datasets/f7ed68293c5\
                e40d58f1de9c8435c3e84_0.csv", stringsAsFactors = TRUE)

# look for address matches
ds$f.vpi.isBldgAddkey <- ifelse(
  ds$l.addresskey %in% vpi$LNIADDRESSKEY, 1, 0
  ) %>% as.factor()

# look for owner name matches
ds$f.vpi.isOwner <- ifelse(
  ds$l.owner %in% vpi$OWNER1, 1, ifelse(ds$l.owner %in% vpi$OWNER2, 1, 0)
  ) %>%
  as.factor()


# tax delinquency dataset
# encode and format api call
url <- URLencode("https://data.phila.gov/carto/api/v2/sql?q=SELECT owner,\
                 total FROM real_estate_tax_balances WHERE total > 0 AND \
                 tax_period > 2015 AND owner != ''")
url <- gsub("22", "27", url)

# get tax delinquency data
delinquent <- getURL(url)

# parse delinquency data from JSON
del <- fromJSON(delinquent)$rows %>%
  group_by(owner) %>%
  dplyr::summarise(s.ownerTaxBalance = mean(total)) %>%
  dplyr::rename(l.owner = owner)

# extract a dataset of tax balance data for each property
ds.temp <- ds %>%
  mutate(l.owner = as.character(l.owner)) %>%
  left_join(del) %>%
  select(s.ownerTaxBalance) %>%
  mutate(
    s.ownerTaxBalance = ifelse(
      is.na(s.ownerTaxBalance), 0, s.ownerTaxBalance)) %>%
  mutate(f.ownerDelinquent = as.factor(ifelse(s.ownerTaxBalance > 5, 1, 0)))

# and bind it to the master dataset
ds <- cbind(ds, ds.temp)



# ONE-HOT ENCODE CATEGORICAL VARIABLES ------------------------------------

# get list of factor variable names
names <- ds %>%
  select(starts_with("f.")) %>%
  names %>%
  as.list

# encode each of these as  a series of binary dummy variables
factor_variable_list <- llply(names, castFactorVars, dat = ds, 
                              .progress = "text")

# join all categorical data variable dfs together
ds <- plyr::join_all(factor_variable_list, by = "id.apinspkey", type = "left", match = "all") %>%
  left_join(df_violTpe, "id.apinspkey") %>%
  select(-id.apinspkey) %>%
  cbind(ds,. )

# generate two different response variables, factor and numeric
#   for different model types
ds <- ds %>%
  dplyr::rename(o.failed.n = o.failed) %>%
  mutate(o.failed.f = as.factor(o.failed.n)) %>%
  select(o.failed.n, o.failed.f, 2:ncol(.))

# create a vector of variable names that will be used in the models
# and one that won't
mod_vars <- ds %>% select(
  -o.numFails, -starts_with("i."),
  -starts_with("l."), -starts_with("f.")) %>%
  names
non_mod_vars <- setdiff(names(ds), mod_vars)


# FACTOR LOOK-UP TABLES ---------------------------------------------------

# create a series of reference variable lookup tables for later use
lookup <- rbind(
  violType_lookup,
  getFactorLevelLookup(ds$f.inspDesc, "inspDesc"),
  getFactorLevelLookup(ds$f.recStatus, "recStatus"),
  getFactorLevelLookup(ds$f.priorityde, "priorityde")
)


# SAVE --------------------------------------------------------------------

# save objects for use in following scripts
save(ds, mod_vars, non_mod_vars, lookup, file = "data/full_dataset_for_modeling.Rdata")
save(df, file = "data/failure_level_dataset.Rdata")

