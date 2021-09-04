
library(readr)
library(tidymodels)
library(modeltime)
library(baguette)
library(survival)
library(rules)

library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)

cl <- makePSOCKcluster(all_cores)


# SPLIT DATA

set.seed(4595)

data_split <- initial_split(VAL_terremotos_mundo, strata = "inten", prop = 0.75)

train_data <- training(data_split)

test_data <- testing(data_split)


train_data <- recipe(inten~., data = train_data) %>% 
  prep() %>% 
  juice()


k_folds_data <- vfold_cv(train_data, strata = inten,v = 10)


hash_rec <- recipe(inten~., data = train_data)


#-------------------------------------------------------------------------------



# Tune Models


model_control <- control_grid(save_pred = TRUE)
model_metrics <- metric_set(rmse, rsq)



# Tune hash models-----------------------------------------------------


# RAND FOREST


rand_forest_model <- rand_forest(trees = tune(),
                                 min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rand_forest_grid <- grid_regular(parameters(rand_forest_model))

registerDoParallel(cl)

rand_forest_hash <- tune_grid(
  rand_forest_model,
  hash_rec,
  grid = rand_forest_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)
autoplot(rand_forest_hash)


rand_forest_hash %>% show_best("rsq")




# BAGS MARS


bag_mars_model <- bag_mars(num_terms = tune(),
                           prod_degree = tune(),
                           prune_method = tune())  %>%
  set_engine("earth") %>%
  set_mode("regression")


bag_mars_grid <- grid_regular(parameters(bag_mars_model))

bag_mars_hash <- tune_grid(
  bag_mars_model,
  hash_rec,
  grid = bag_mars_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)

autoplot(bag_mars_hash)

bag_mars_hash %>% show_best("rsq")


#  BAGGED DECISION TREE



bagged_decision_tree_model <- bag_tree(cost_complexity = tune(),
                           tree_depth = tune(),
                           min_n = tune())  %>%
  set_engine("rpart") %>%
  set_mode("regression")


bagged_decision_tree_grid <- grid_regular(parameters(bagged_decision_tree_model))

bagged_decision_tree_hash <- tune_grid(
  bagged_decision_tree_model,
  hash_rec,
  grid = bagged_decision_tree_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)

autoplot(bagged_decision_tree_hash)

bagged_decision_tree_hash %>% show_best("rsq")



# BOOST TREE



boost_tree_model <- boost_tree(trees = tune(),
                                min_n = tune(),
                                tree_depth = tune(),
                                learn_rate = tune(),
                                loss_reduction = tune())  %>%
  set_engine("xgboost") %>%
  set_mode("regression")


boost_tree_model_grid <- grid_regular(parameters(boost_tree_model))

registerDoParallel(cl)

boost_tree_model_hash <- tune_grid(
  boost_tree_model,
  hash_rec,
  grid = boost_tree_model_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)

autoplot(boost_tree_model_hash)

boost_tree_model_hash %>% show_best("rsq")




# SVM PLOY MODEL



svm_poly_model <- svm_poly(cost = tune(),
                           degree = tune(),
                           scale_factor = tune(),
                           margin = tune())  %>%
  set_engine("kernlab") %>%
  set_mode("regression")




svm_poly_grid <- grid_regular(parameters(svm_poly_model))


registerDoParallel(cl)
svm_poly_hash <- tune_grid(
  svm_poly_model,
  hash_rec,
  grid = svm_poly_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)

autoplot(svm_poly_hash)

svm_poly_hash %>% show_best("rsq")








# SVM RBF MODEL



svm_rbf_model <- svm_rbf( cost = tune(),
                          rbf_sigma = tune(),
                          margin = tune())  %>%
  set_engine("kernlab") %>%
  set_mode("regression")




svm_rbf_grid <- grid_regular(parameters(svm_rbf_model))


registerDoParallel(cl)

svm_rbf_hash <- tune_grid(
  svm_rbf_model,
  hash_rec,
  grid = svm_rbf_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)

autoplot(svm_rbf_hash)

svm_rbf_hash %>% show_best("rsq")




# KNN 




nearest_neighbor_model <- nearest_neighbor( neighbors = tune(),
                                   weight_func =  tune(),
                                   dist_power =  tune())  %>%
  set_engine("kknn") %>%
  set_mode("regression")



nearest_neighbor_grid <- grid_regular(parameters(nearest_neighbor_model))


registerDoParallel(cl)

nearest_neighbor_hash <- tune_grid(
  nearest_neighbor_model,
  hash_rec,
  grid = nearest_neighbor_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)

autoplot(nearest_neighbor_hash)

nearest_neighbor_hash %>% show_best("rsq")




# CUBIST RULES





cubis_model <- cubist_rules(committees = tune(),
                            neighbors = tune(),
                            max_rules = tune(),)%>%
  set_engine("Cubist") %>%
  set_mode("regression")



cubis_grid <- grid_regular(parameters(cubis_model), levels = 4)


registerDoParallel(cl)

cubis_hash <- tune_grid(
  cubis_model,
  hash_rec,
  grid = cubis_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)

autoplot(cubis_hash)

cubis_hash %>% show_best("rsq")









#------------------------------------------


