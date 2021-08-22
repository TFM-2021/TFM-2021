
library(readr)
library(tidymodels)
library(modeltime)
library(baguette)
library(survival)
library(rules)
library(bimba)
library(discrim)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)

cl <- makePSOCKcluster(all_cores)


#VAL_terremotos_mundo_MMI <- read_delim("data/data_VAL/VAL_terremotos_mundo_MMI.csv", 
                                       #";", escape_double = FALSE, trim_ws = TRUE)


#VAL_terremotos_mundo_MMI <- VAL_terremotos_mundo_MMI %>%
#  mutate(MMI_inten = replace(MMI_inten, MMI_inten %in% c(1,2,3), "<IV"),
#        MMI_inten = replace(MMI_inten, MMI_inten == 4, "IV"),
#        MMI_inten = replace(MMI_inten, MMI_inten == 5, "V"),
#         MMI_inten = replace(MMI_inten, MMI_inten == 6, "VI"),
#        MMI_inten = replace(MMI_inten, MMI_inten == 7, "VII")) %>%
# filter(MMI_inten %in% c("IV", "V","<IV","VI", "VII")) 

#VAL_terremotos_mundo_MMI$inten <- VAL_terremotos_mundo_MMI$MMI_inten

#VAL_terremotos_mundo_MMI$MMI_inten <- NULL

#VAL_terremotos_mundo <- VAL_terremotos_mundo_MMI

#VAL_terremotos_mundo$inten <- as.factor(VAL_terremotos_mundo$inten)


#VAL_terremotos_mundo %>%
#  ggplot(aes(log(prof_km)   ,mag , color= inten)) +
# geom_point()


#recipe(~., VAL_terremotos_mundo) %>%
# step_upsample(inten, over_ratio = 0.5) %>%
# prep() %>%
# bake(new_data = NULL) %>%
# ggplot(aes(log(prof_km)   ,mag , color= inten)) +
# geom_point()

VAL_terremotos_modelo_intensidad <- recipe(~., VAL_terremotos_modelo_intensidad) %>%
 step_upsample(inten, over_ratio = 0.15) %>%
 prep() %>%
 bake(new_data = NULL)



VAL_terremotos_modelo_intensidad <- readRDS(file = "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")




# IMPUTACION




#VAL_terremotos_mundo$prof_km[is.na(VAL_terremotos_mundo$prof_km)]<-median(VAL_terremotos_mundo$prof_km,na.rm=TRUE)


#VAL_terremotos_mundo$mag[is.na(VAL_terremotos_mundo$mag)]<-median(VAL_terremotos_mundo$mag,na.rm=TRUE)



# SPLIT DATA

set.seed(455)

data_split <- initial_split(VAL_terremotos_modelo_intensidad, strata = "inten", prop = 0.75)

train_data <- training(data_split)

test_data <- testing(data_split)


train_data <- recipe(inten~., data = train_data) %>% 
  prep() %>% 
  juice()


k_folds_data <- vfold_cv(train_data, strata = inten,v = 5)


hash_rec <- recipe(inten~., data = train_data)


#-------------------------------------------------------------------------------



# Tune Models


f_score_terremotos <- function(data, truth, estimate, na_rm = TRUE, ...) {
  f_meas(
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    beta = 2,
    na_rm = na_rm,
    ...
  )
}


f_score_terremotos <- new_class_metric(f_score_terremotos,"maximize")

model_control <- control_grid(save_pred = TRUE)
model_metrics <- metric_set(recall,precision,f_score_terremotos,roc_auc)

# Tune hash models-----------------------------------------------------




# RAND FOREST


rand_forest_model <- rand_forest(trees = tune(),
                                 min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rand_forest_grid <- grid_regular(parameters(rand_forest_model))
rand_forest_grid

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

rand_forest_hash %>% show_best("recall")

final_param <- rand_forest_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)


# LDA ---------------------------------------------------------------------


LDA_model <- discrim_linear(penalty = tune()) %>%
  set_engine("mda") %>%
  set_mode("classification")



LDA_grid <- grid_regular(parameters(LDA_model), filter = c(penalty>1),levels = 5000)
LDA_grid <- tibble("penalty" = c(1,1.1,1.5,1.6,1.9))

registerDoParallel(cl)

LDA_hash <- tune_grid(
  LDA_model,
  hash_rec,
  grid = LDA_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)
autoplot(LDA_hash)

LDA_hash %>% show_best("roc_auc")

final_param <- LDA_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(penalty)


# Regularized discriminant analysis--------------------------------------



rdm_model <- discrim_regularized(
                                mode = "classification",
                                engine = "klaR",
                                frac_common_cov =  tune(),
                                frac_identity =  tune())





rdm_grid <- grid_regular(parameters(rdm_model))
rdm_grid

registerDoParallel(cl)

rdm_hash <- tune_grid(
  rdm_model,
  hash_rec,
  grid = rdm_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)
autoplot(rdm_hash)

rdm_hash %>% show_best("recall")

final_param <- rdm_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1) %>%
  select(trees, min_n)




# NAIVE BAYES-------------------------------------------------------------------



naive_Bayes_model <-naive_Bayes(
  mode = "classification",
  engine = "klaR",
  smoothness = tune(),
  Laplace = tune()
)


naive_Bayes_grid <- grid_regular(parameters(naive_Bayes_model))
naive_Bayes_grid

registerDoParallel(cl)

naive_Bayes_hash <- tune_grid(
  naive_Bayes_model,
  hash_rec,
  grid = naive_Bayes_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)
autoplot(naive_Bayes_hash)

naive_Bayes_hash %>% show_best("recall")

final_param <- naive_Bayes_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



# REGERSION MULTINOMIAL--------------------------------------------------------


multinom_reg_model <- multinom_reg(
  mode = "classification",
  engine = "glmnet",
  penalty = tune(),
  mixture = tune()
)



multinom_reg_grid <- grid_regular(parameters(multinom_reg_model))
multinom_reg_grid

registerDoParallel(cl)

multinom_reg_hash <- tune_grid(
  multinom_reg_model,
  hash_rec,
  grid = multinom_reg_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)
autoplot(multinom_reg_hash)

multinom_reg_hash %>% show_best("recall")

final_param <- multinom_reg_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



# C5.0 Rule-Based---------------------------------------------------------------

c5_rules_model <- C5_rules(mode = "classification", 
         trees = tune(), 
         engine = "C5.0")



c5_rules_grid <- grid_regular(parameters(c5_rules_model) )
c5_rules_grid

registerDoParallel(cl)

c5_rules_hash <- tune_grid(
  c5_rules_model,
  hash_rec,
  grid = c5_rules_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)



autoplot(c5_rules_hash)

c5_rules_hash %>% show_best("roc_auc")

final_param <- c5_rules_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



# BAGS MARS-----------------------------------------------------------------------


bag_mars_model <- bag_mars(num_terms = tune(),
                           prod_degree = tune(),
                           prune_method = tune())  %>%
  set_engine("earth") %>%
  set_mode("classification")


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

bag_mars_hash %>% show_best("roc_auc")





#  BAGGED DECISION TREE--------------------------------------------------------



bagged_decision_tree_model <- bag_tree(cost_complexity = tune(),
                                       tree_depth = tune(),
                                       min_n = tune())  %>%
  set_engine("rpart") %>%
  set_mode("classification")


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

bagged_decision_tree_hash %>% show_best("recall")






# BOOST TREE--------------------------------------------------------------------



boost_tree_model <- boost_tree(trees = tune(),
                               min_n = tune(),
                               tree_depth = tune(),
                               learn_rate = tune(),
                               loss_reduction = tune())  %>%
  set_engine("xgboost") %>%
  set_mode("classification")


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

boost_tree_model_hash %>% show_best("roc_auc")




# SVM PLOY MODEL----------------------------------------------------------------



svm_poly_model <- svm_poly(cost = tune(),
                           degree = tune(),
                           scale_factor = tune(),
                           margin = tune())  %>%
  set_engine("kernlab") %>%
  set_mode("classification")




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

svm_poly_hash %>% show_best("roc_auc")








# SVM RBF MODEL-----------------------------------------------------------------



svm_rbf_model <- svm_rbf( cost = tune(),
                          rbf_sigma = tune(),
                          margin = tune())  %>%
  set_engine("kernlab") %>%
  set_mode("classification")




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

svm_rbf_hash %>% show_best("roc_auc")




# KNN ----------------------------------------------------------------------------




nearest_neighbor_model <- nearest_neighbor( neighbors = tune(),
                                            weight_func =  tune(),
                                            dist_power =  tune())  %>%
  set_engine("kknn") %>%
  set_mode("classification")



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

nearest_neighbor_hash %>% show_best("roc_auc")









# Evaluate Confusion Matrix


LDA_hash %>% 
  collect_predictions() %>% 
  inner_join(final_param) %>% 
  conf_mat(truth = inten, estimate = .pred_class)




final_model <- workflow() %>% 
  add_model(LDA_model) %>% 
  add_recipe(hash_rec)

final_model <- finalize_workflow(final_model, final_param)



final_res <- last_fit(final_model, data_split)

final_res %>% collect_metrics()

final_res %>% 
  collect_predictions() %>% 
  conf_mat(truth = inten, estimate = .pred_class)













#------------------------------------------



VAL_terremotos_modelo_intensidad <- readRDS(file = "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")


rand_forest_model_best <- discrim_linear(penalty = 1.5) %>%
  set_engine("mda") %>%
  set_mode("classification")



VAL_terremotos_modelo_intensidad

fit <- rand_forest_model_best %>%
  fit(inten~., data= train_data)

rf_testing_pred <- 
  predict(fit, VAL_terremotos_modelo_intensidad) %>% 
  bind_cols(predict(fit, VAL_terremotos_modelo_intensidad, type = "prob")) %>% 
  bind_cols(VAL_terremotos_modelo_intensidad %>% select(inten))



rf_testing_pred %>%
  roc_curve(truth = inten, `.pred_<IV`:.pred_VII) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path() +
  coord_equal() +
  labs(color = NULL)


rf_testing_pred %>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VII)

rf_testing_pred %>%
  conf_mat(truth = inten, estimate = .pred_class)







