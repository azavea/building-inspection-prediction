###########################################################################
## PROJECT: L+I predictive modelimg
##
## SCRIPT PURPOSE: Model building
##    - establish h2o cluster
##    - partition dataset
##    - train models on training sets with repeated cross-validation
##    - parameter grid tuning
##    - tust multiple models
##    - valid on test sets
##
## DATE: 18 July 2017
## AUTHOR: Simon Kassel
###########################################################################

# helper functions 
source("R/helper-functions.R")

# source('R/data-wrangling-and-feature-engineering.R')
load("data/full_dataset_for_modeling.Rdata")

# source('R/feature-selection.R')
load("data/boruta.train.Rdata")

# packages
packages(
  c(
    "tidyverse",
    "data.table",
    "sp",
    "caret",
    "plyr",
    "pROC",
    "statmod",
    "h2o",
    "Boruta"
  )
)

# baseline accuracy
max(table(ds$o.failed.n)) / nrow(ds)
# 0.57223

# SUBSET DATA -------------------------------------------------------------

# remove rejected vars
d <- ds %>% 
  select(-one_of(rejected_vars))

# partition dataset
set.seed(6789)
inTrain <- createDataPartition(d$o.failed.n, p = 0.6, list = FALSE)
train <- d[inTrain, ]
valid <- d[-inTrain, ]

# list of predictor variables
pred_vars <- setdiff(names(d), c(non_mod_vars, "o.failed.n", "o.failed.f"))


# H2O ---------------------------------------------------------------------

# initialize h2o cluster
localhost <- h2o.init(nthreads = -1, max_mem_size = "8G")

# open flow in web browser
paste0("start http://localhost:", localhost@port) %>%
  shell()

# load data into h2o
d.h2o <- as.h2o(d)
train.h2o <- as.h2o(train)
valid.h2o <- as.h2o(valid)

# define parameter search criteria grid
search.criteria <- list(
  strategy = "RandomDiscrete", 
  max_runtime_secs = 900,
  stopping_rounds = 5, 
  stopping_metric = "AUTO"
)

# GLM ---------------------------------------------------------------------

# subset variables into different categories
fact_vars <- getVarNames(names(train.h2o), "f.")
vi_vars <- getVarNames(names(train.h2o), "vi")
cont_vars <- getVarNames(pred_vars, c("s."))

# extract only one spatial var to avoid multicollinearity
glm_pred_vars <- c(fact_vars, cont_vars, vi_vars, "e.avgDists.rep.n10")

# specify alpha values
alphas <- seq(0, 1, 0.1)

# Parameter tuning:
glm.tune.grid <- h2o.grid(
  algorithm = "glm", 
  hyper_params = list(alpha = alphas),
  y = "o.failed.f", 
  x = glm_pred_vars,
  grid_id = "glm.tune.grid",
  training_frame = train.h2o,
  nfolds = 10,
  lambda_search = TRUE,
  family = "binomial", 
  standardize = TRUE
)

# get results/predicitons from fina glm model
glm_results <- get_from_grid("glm.tune.grid", valid.h2o)

# save results
glm_mod <- glm_results$fit.glm.tuned.h2o
save(glm_mod, file = "output/glm_final_mod.Rdata")


# GLM INTERACTION MODEL ---------------------------------------------------

# get interaction variables for train/test sets
pairwise <- getInteractionVars(train.h2o, realf, bind = FALSE)
int_train <- h2o.cbind(train.h2o, pairwise)
int_valid <- getInteractionVars(valid.h2o, realf)

# glm mods with variable subsets
#   continuous variables
cont_mod <- h2oLogMod(c(cont_vars, vi_vars), "glm_cont")
#   interaction variables
int_mod <- h2oLogMod(names(pairwise), "glm_int", training_frame = int_train)
#   all predictor variables
all_preds <- c(names(pairwise), fact_vars, vi_vars, cont_vars)
all_mod <- h2oLogMod(all_preds, "glm_all", training_frame = int_train)

# generate a list of all glm mods
all_glm_mods <- list(
  list("glm", glm_results$fit.glm.tuned.h2o),
  list("glm_cont", cont_mod),
  list("glm_factor", int_mod),
  list("glm_interaction", all_mod)
)

# predict using each of the four models
all_glm_results <- llply(all_glm_mods, getPredictions, validation_frame = int_valid) %>%
  # append them to each other
  do.call("cbind", .)  %>%
  # calculate additional ensemble variables
  mutate_if(is.factor, factor_to_numeric) %>%
  mutate(result = valid$o.failed.n,
         vote = glm.predict + glm_cont.predict + glm_factor.predict + glm_interaction.predict,
         vote_pred = ifelse(vote > 2, 1, 0),
         correct = ifelse(vote_pred == result, 1, 0),
         mean_vote = (glm.p1 + glm_cont.p1 + glm_factor.p1 + glm_interaction.p1) / 4)



# GBM ---------------------------------------------------------------------

# Tuned parameters:
# define hyperparameter grid
gbm.grid <- list(
  learn_rate = seq(.03,.05,.01), 
  max_depth = c(8:10),
  sample_rate = seq(0.7, 1.0, 0.1), 
  col_sample_rate = seq(0.4, 1.0, 0.1)
)

# search parameter grid
gbm.tune.grid <- h2o.grid(
  algorithm = "gbm", 
  grid_id = "gbm.tune.grid", 
  y = "o.failed.f",
  x = pred_vars,
  hyper_params = gbm.grid,
  training_frame = train.h2o,
  score_each_iteration = TRUE,
  nfolds = 10,
  keep_cross_validation_fold_assignment = TRUE,
  keep_cross_validation_predictions = TRUE,
  search_criteria = search.criteria,
  seed = 1
)

# get results/predictions from final gbm model
gbm_results <- get_from_grid("gbm.tune.grid", valid.h2o)

# save results
gbm_mod <- gbm_results$fit.gbm.tuned.h2o
save(gbm_mod, file = "output/gbm_final_mod.Rdata")

# RANDOM FORREST ----------------------------------------------------------

rf.grid <- list(
  max_depth = seq(15, 30, 45),
  ntrees = c(10, 25, 50)
)

# search parameter grid
rf.tune.grid <- h2o.grid(
  algorithm = "randomForest", 
  y = "o.failed.f",
  x = pred_vars,
  grid = rf.grid,
  training_frame = train.h2o,
  score_each_iteration = TRUE,
  nfolds = 5,
  binomial_double_trees = TRUE,
  balance_classes = TRUE,
  seed = 2
)

# get resuls/predictions from final rf model
rf_results <- get_from_grid("rf.tune.grid", valid.h2o)

# save results
rf_mod <- rf_results$fit.rf.tuned.h2o
save(rf_mod, file = "output/rf_final_mod.Rdata")

# ENSEMBLE MODEL ----------------------------------------------------------

# Combine prediction results from each algorithm with original dataset
results <- valid %>% 
  cbind(all_glm_results, rf_results$pred.rf, gbm_results$pred.gbm)

# add ensemble predictions/probabilities/correct tage
results <- results %>%
  mutate(ensemble.predict = ifelse(
    rf.predict + gbm.predict + glm_interaction.p1 > 1, 1, 0)) %>%
  mutate(ensemble.p1 = (glm_interaction.p1 + rf.p1 + gbm.p1) / 3) %>%
  mutate(ensemble.correct = ifelse(ensemble.predict == o.failed.n, 1, 0))

# report accuracy rates
for (i in names(results)[grepl(".correct", names(results))]) {
  paste0(
    gsub(".correct", " accuracy rate:  % ", i), 
    round(mean(results[,i]) * 100, 2)
  ) %>%
    print
}

# save results
save(results, file = "data/prediction_results.Rdata")

# H2O ---------------------------------------------------------------------

h2o.shutdown()






