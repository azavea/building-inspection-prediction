###########################################################################
## PROJECT: L+I predictive modeling
##
## SCRIPT PURPOSE: Data Visualzation
##    - generate data visualizations showing model input data, features
##      and results
##
## DATE: 17 August 2017
## AUTHOR: Simon Kassel
###########################################################################

# helper functions 
source("R/helper-functions.R")

# source('LNI-analysis-feature-engineering.R')
load("data/full_dataset_for_modeling.Rdata")

# source('R/feature_importance.R')
load("data/boruta.train.Rdata")

# source('R/model-selection.R')
load("data/prediction_results.Rdata")

# Additional datasets
#   Failure-level dataset:
load("data/failure_level_dataset.Rdata")
#   Variable labels
var_names <- read.csv("data/variable_names.csv")
load("output/data/gbm_final_mod.Rdata")
load("output/data/rf_final_mod.Rdata")
load("output/data/glm_final_mod.Rdata")

# packages
packages(
  c(
    "h2o",
    "tidyverse",
    "sf",
    "ggjoy",
    "plotROC",
    "ggpubr",
    "pROC",
    "waffle",
    "svglite"
    )
)

# BAR CHART INDICATING LEVELS OF L+I DATA ---------------------------------

# Create data frame with # of records at each level of data
data_levels_bar <- df %>%
  select(apfailkey, apinspkey, casenumber, addresskey) %>%
  summarise_all(funs(n_distinct)) %>%
  gather %>%
  mutate(outline = ifelse(key == "apinspkey", "y", "n"))

vals <- (data_levels_bar$value / 1000) %>%
  round(1) %>%
  paste0("k")


data_levels_bar <- data_levels_bar %>%
  mutate(label = c(
    paste0(vals[1], " total\nviolations"),
    paste0("from ", vals[2], "\nbuilding inspections"),
    paste0("collected from\n", vals[3], " L+I cases"),
    paste0("at ", vals[4], "\nunique locations")
  ))

# generate graphic
ggplot(data_levels_bar) + 
  geom_bar(aes(x = reorder(key, value), y = value, color = outline), 
           fill = pal[4], size = 1, stat = "identity") +
  geom_text(aes(x = key, y = value / 2, label = label), 
            color = pal[2]) +
  scale_color_manual(values = c("white", pal[2])) +
  labs(
    title = "Building Inspection Data",
    subtitle = "Analysis used inspection-level data from 55,000 building inspections (2014-2017)"
  ) +
  coord_flip() +
  scale_y_discrete(expand = c(0,0)) +
  plotTheme(map = TRUE) +
  theme(
    legend.position = "none", 
    axis.title = element_blank()) 

ggsave("output/plots/fig.x-Levels-of-data-bar-charts.svg", device = "svg")


# LOOK AT THE CASE LEVEL --------------------------------------------------

# create sf points object of all cases
cases <- ds %>% group_by(i.casenumber) %>% 
  dplyr::summarise(
    total_fails = max(as.numeric(as.character(f.before))),
    long = first(l.long),
    lat = first(l.lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant") %>%
  st_transform(2272)

# read in philadelphia boundaries shapefile
phila <- st_read("data/City_Limits.shp") %>%
  st_transform(2272)

# create a .25 mi square grid, clipped to city limits
phila_grid <- st_make_grid(phila, cellsize = c(1320, 1320)) %>%
  st_intersection(phila, .) %>%
  mutate(fid = c(1:nrow(.)))  

# join grid to points
grids <- st_join(phila_grid, cases) %>%
  dplyr::select(fid, total_fails) %>%
  mutate(
    failed_once = ifelse(total_fails < 2, 1, 0),
    failed_more_than_once = ifelse(total_fails >= 2, 1, 0)
  ) %>%
  dplyr::select(-total_fails) %>%
  dplyr::group_by(fid) %>%
  dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  tidyr::gather(label, count, -fid, -geometry) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(label2 = ifelse(
    label == "failed_once", "One-time violations", "Repeat violations"))

# plot repeators and non-repeators
ggplot(grids) + 
  geom_sf(aes(fill = count), color = "white", size = 0.1) +
  facet_wrap(~label2) +
  scale_fill_gradient(
    "Total\nViolations",
    low = pal[4], 
    high = pal[5]) +
  labs(
    title = "Followup Building Inspections Results",
    subtitle = "Relative densities of buildings that passed after initially failing an inspection (left) and those that failed (right)",
    caption = "Source: Azavea, Data: Philadelphia Department of Licenses and Inspections") +
  plotTheme(title_color = 5, map = TRUE, text_color = 5) +
  theme(axis.text = element_blank())
  
ggsave("output/plots/fig.x-Map_of_fails_and_passes.svg", device = "svg")


# GGJOY CONTINUOS VARS ----------------------------------------------------

# generate data frame of useful vars
joy <- ds %>%
  select(o.failed.f, s.sinceLastInsp, s.sinceViolation) %>%
  gather(var, value, -o.failed.f)

# create joy plot
ggplot(joy, aes(x = value, y = var)) + 
  geom_joy(
    aes(fill = o.failed.f), 
    alpha = 0.25, 
    color = NA,
    scale = 1.1) +
  geom_joy(
    aes(color = o.failed.f), 
    size = 1, 
    fill = NA,
    scale = 1.1) +
  labs(
    title = "Predicting outcomes using time variables",
    subtitle = "Distributions of multiple predictor variables between response categories (full dataset)",
    x = "Days") +
  scale_color_manual(
    "Follow-up\nInspection\nOutcome", 
    values = c(pal[5], pal[1]),
    labels = c("pass", "fail")) +
  scale_fill_manual(
    "Follow-up\nInspection\nOutcome", 
    values = c(pal[5], pal[1]),
    labels = c("pass", "fail")) +
  plotTheme() +
  scale_x_continuous(
    limits = c(0, 200),
    expand = c(0, 0)) +
  scale_y_discrete(labels = c("since\nviolation" ,"since last\ninspection")) + 
  theme(axis.title.y = element_blank())

ggsave("output/plots/fig.x-Joy_plots.svg")

# VARIABLE IMPORTANCE PLOT ------------------------------------------------

# extract varible importance 
#   gbm model
gbm_varImp <- h2o.varimp(gbm_mod) %>%
  as.data.frame() %>%
  mutate(model = "gbm")
#   rf model
rf_varImp <- h2o.varimp(rf_mod) %>%
  as.data.frame() %>%
  mutate(model = "rf")

# combine and arrange
vi <- rbind(gbm_varImp, rf_varImp) %>%
  left_join(var_names) %>%
  group_by(group, model) %>%
  dplyr::summarise(importance = sum(percentage)) %>%
  arrange(desc(importance)) 

# plat variable importance dot plot
ggdotchart(
  vi, 
  x = "group", 
  y = "importance",
  color = "model",                               
  sorting = "descending",                       
  rotate = TRUE,                                
  dot.size = 3,
  alpha = 0.5,
  ggtheme = theme_pubr()) + 
  scale_color_manual("Model Type", values = c(pal[1] ,pal[5])) +
  labs(
    title = "Variable importance",
    subtitle = "Scaled variable importance for final Gradient Boosting Machine and Random Forest models",
    y = "Scaled variable importance",
    y = "Predictor variable"
  ) +
  theme(
    text = element_text(
      color = pal[2],
      size = 10),
    legend.position = "right",
    legend.title = element_text(
      size = 8,
      face = "italic",
      color = "gray20"),
    legend.text = element_text(color = "gray20"),
    axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    plot.title = element_text(
      hjust = 0, 
      face = "bold", 
      margin = margin(b = 3),
      size = 11),
    axis.title = element_text(
      hjust = 1,
      face = "italic",
      color = "gray20"),
    axis.text = element_text(
      size = 8,
      color = "gray20"),
    legend.title.align = 0,
    axis.text.y = element_text(vjust = 0.25),
    axis.text.x = element_text(vjust = -50),
    plot.subtitle = element_text(
      hjust = 0, 
      face = "italic",
      margin = margin(b = 10))
    ) +
  theme_cleveland() 

ggsave("output/plots/fig.x-Variable_importance_plot.svg", device = "svg")

  

# ROC  --------------------------------------------------------------------

# get relevant results
res_roc <- results %>%
  select(o.failed.n, ends_with(".p1"), ends_with("pred")) %>%
  gather(model, probability, -o.failed.n) %>%
  filter(model %in% c("glm_interaction.p1", "gbm.p1", "rf.p1"))

# get a data frame of auc values by model
labels <- res_roc %>% 
  group_by(model) %>%
  dplyr::summarise(auc = round(roc(o.failed.n, probability)$auc, 3)) %>%
  mutate(x = 0.6, 
         y = c(0.5, 0.4, 0.3), 
         label = paste0(c("gbm auc: ", "glm auc: ", "rf auc: "), auc))

# plot roc
ggplot() + 
  geom_roc(
    data = res_roc,
    aes(d = o.failed.n, m = probability, color = model),
    linealpha = 0.75,
    labels = FALSE,
    pointsize = 0
  ) +
  labs(
    title = "ROC Curve",
    subtitle = "Comparative fit for multiple models",
    x = "False positive rate",
    y = "True positive rate"
  ) +
  geom_label(
    data = labels,
    aes(x = x, y = y, fill = model, label = label),
    alpha = 0.5,
    color = "white",
    size = 5,
    hjust = 0,
    label.padding = unit(5, "pt"),
    label.r = unit(5, "pt"),
    show.legend = FALSE) +
  scale_fill_manual(values = c(pal[1], pal[3], pal[5])) +
  scale_color_manual(
    "Model type",
    values = c(pal[1], pal[3], pal[5]),
    labels = c("gbm", "glm", "rf")) +
  theme_minimal() +
  plotTheme()

ggsave("output/plots/fig.x-ROC_curves.svg")


# CONFUSION MATRIX MAPS ---------------------------------------------------

# get confusion matrix fields
tcm <- tidy_confusion_matrix(
  results, 
  response_var = "o.failed.n", 
  pred_var = "gbm.predict")

# convert to sf object
st_matrix <- st_as_sf(
  tcm, 
  coords = c("long", "lat"), 
  crs = 4326, 
  agr = "constant") %>%
  st_transform(2272)

# transform into 4 sf objects, joining to grid
grid_mat <- st_join(phila_grid, st_matrix) %>%
  select(fid, true_positive, true_negative, false_positive, false_negative) %>%
  group_by(fid) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  tidyr::gather(label, count, -fid, -geometry) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(pred = factor(
    ifelse(grepl("positive", label), "Predict: Fail", "Predict: Pass"), 
    levels = c("Predict: Pass", "Predict: Fail")),
         result = ifelse(grepl("true", label), "CORRECT", "INCORRECT"),
         response = ifelse(label %in% c("true_positive", "false_negative"), 
                           "Actual: Fail", "Actual: Pass"))

# plot map-based confusion matrix
ggplot(grid_mat) + 
  geom_sf(aes(fill = count), color = NA) +
  facet_grid(pred~response, switch = "both") +
  scale_fill_gradient(
    "Count",
    low = pal[4], 
    high = pal[5]) +
  labs(title = "Mapped Confusion Matrix Results",
       subtitle = "Spatial patterns in test-set prediction results") + 
  plotTheme(map = TRUE, title_color = 5)

ggsave("output/plots/fig.x-Confusion_matrix_maps.svg")


# CONFUSION MATRIX --------------------------------------------------------

# get a confusion matrix data frame
conf_matrix <- grid_mat %>%
  select(label, count) %>%
  group_by(label) %>%
  dplyr::summarise(total = sum(count)) %>%
  mutate(
    label = c("False Negative", "False Positive", "True Negative", "True Positive"),
    pct = round((total / sum(total)) * 100, 2),
    text = paste0(label, ": ", pct, "% "))

# named vector for waffle plot
waff <- conf_matrix$pct
names(waff) <- conf_matrix$text

# generate waffle plot
waffle(
  round(waff, 0), 
  rows = 5,
  colors = waffle_pal) +
  labs(
    title = "Confusion Matrix: Predictive accuracy on validation set",
    subtitle = paste0("Accuracy Rate: ", sum(waff[3:4]), "%")
  ) +
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.5, "cm"),
    plot.title = element_text(
      hjust = 0, 
      face = "bold", 
      margin = margin(b = 3),
      size = 11,
      color = pal[5]),
    plot.subtitle = element_text(
      hjust = 0, 
      face = "italic",
      margin = margin(b = 10),
      color = pal[5]),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  )

ggsave("output/plots/fig.x-Confusion_matrix_waffle.svg")

