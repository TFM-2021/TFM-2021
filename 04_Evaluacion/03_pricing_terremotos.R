library(tidyverse)
library(tidymodels)

VAL_terremotos_modelo_intensidad <- readRDS(file = "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")

VAL_terremotos_modelo_intensidad$inten <- as.factor(VAL_terremotos_modelo_intensidad$inten)

unique(VAL_terremotos_modelo_intensidad$inten)




# RAND FOREST

autoplot(rand_forest_hash)

rand_forest_hash %>% show_best("recall")

final_param <- rand_forest_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



# Evaluate Confusion Matrix


rand_forest_model <- rand_forest(trees = 1000,
                                 min_n = 2) %>%
  set_engine("ranger") %>%
  set_mode("classification")



fit <- rand_forest_model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)





# LDA ---------------------------------------------------------------------


autoplot(LDA_hash)

LDA_hash %>% show_best("recall")

final_param <- LDA_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(penalty)


model <- discrim_linear(penalty = 1) %>%
  set_engine("mda") %>%
  set_mode("classification")






fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)





# Regularized discriminant analysis--------------------------------------



autoplot(rdm_hash)

rdm_hash %>% show_best("recall")

final_param <- rdm_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1) %>%
  select(trees, min_n)



model <- discrim_regularized(
  mode = "classification",
  engine = "klaR",
  frac_common_cov =  0,
  frac_identity =  0)



fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)



# NAIVE BAYES-------------------------------------------------------------------


autoplot(naive_Bayes_hash)

naive_Bayes_hash %>% show_best("recall")

final_param <- naive_Bayes_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)


model <-naive_Bayes(
  mode = "classification",
  engine = "klaR",
  smoothness = 0.5,
  Laplace = 0)


fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)



# REGERSION MULTINOMIAL--------------------------------------------------------

autoplot(multinom_reg_hash)

multinom_reg_hash %>% show_best("recall")

final_param <- multinom_reg_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(penalty)


model <- multinom_reg(
  mode = "classification",
  engine = "nnet",
  penalty = .0769  
)

fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)



# C5.0 Rule-Based---------------------------------------------------------------

autoplot(c5_rules_hash)

c5_rules_hash %>% show_best("recall")

final_param <- c5_rules_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)


model <- C5_rules(mode = "classification", 
                  trees = 3, 
                  engine = "C5.0")


fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)



#  BAGGED DECISION TREE--------------------------------------------------------


autoplot(bagged_decision_tree_hash)

bagged_decision_tree_hash %>% show_best("recall")


final_param <- bag_mars_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)

model <- bag_tree(cost_complexity = 0.1, tree_depth = 15,
                  min_n = 2)  %>%
  set_engine("rpart") %>%
  set_mode("classification")



fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)



# BOOST TREE--------------------------------------------------------------------


autoplot(boost_tree_model_hash)

boost_tree_model_hash %>% show_best("recall")

final_param <- boost_tree_model_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)

model <- boost_tree(trees = 9)  %>%
  set_engine("C5.0") %>%
  set_mode("classification")


fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)



# SVM PLOY MODEL----------------------------------------------------------------



autoplot(svm_poly_hash)

svm_poly_hash %>% show_best("recall")


final_param <- svm_poly_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



model <- svm_poly(cost = 32,
                  degree = 3,
                  scale_factor =0.1,
                  margin = 0)  %>%
  set_engine("kernlab") %>%
  set_mode("classification")


fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)



# SVM RBF MODEL-----------------------------------------------------------------



autoplot(svm_rbf_hash)

svm_rbf_hash %>% show_best("recall")

final_param <- svm_rbf_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)


model <- svm_rbf( cost = 32,
                  rbf_sigma = 1,
                  margin = 0.1)  %>%
  set_engine("kernlab") %>%
  set_mode("classification")

fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)



# KNN ----------------------------------------------------------------------------
library(kknn)


autoplot(nearest_neighbor_hash)

nearest_neighbor_hash %>% show_best("recall")


final_param <- nearest_neighbor_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(neighbors, weight_func,dist_power)



model <- nearest_neighbor( neighbors = 8,
                           weight_func =  "triangular",
                           dist_power =  2)  %>%
  set_engine("kknn") %>%
  set_mode("classification")


fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))


rf_training_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_training_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)


rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))

rf_testing_pred%>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::precision(truth = inten, .pred_class)

rf_testing_pred%>%
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VI)




# Evaluate Confusion Matrix


rand_forest_hash %>% 
  collect_predictions() %>% 
  inner_join(final_param) %>% 
  conf_mat(truth = inten, estimate = .pred_class)




final_model <- workflow() %>% 
  add_model(rand_forest_model) %>% 
  add_recipe(hash_rec)

final_model <- finalize_workflow(final_model, final_param)



final_res <- last_fit(final_model, data_split)

final_res %>% collect_metrics()

final_res %>% 
  collect_predictions() %>% 
  conf_mat(truth = inten, estimate = .pred_class)













#------------------------------------------



VAL_terremotos_modelo_intensidad <- readRDS(file = "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")


model <- bag_tree(cost_complexity = 0.02, tree_depth = 8,
                  min_n = 21)  %>%
  set_engine("rpart") %>%
  set_mode("classification")





fit <- model %>%
  fit(inten~., data= train_data)

rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))


rf_testing_pred <- 
  predict(fit, VAL_terremotos_modelo_intensidad) %>% 
  bind_cols(predict(fit, VAL_terremotos_modelo_intensidad, type = "prob")) %>% 
  bind_cols(VAL_terremotos_modelo_intensidad %>% select(inten))



rf_testing_pred %>%
  roc_curve(truth = inten, `<IV`:VI) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path() +
  labs(color = NULL)+
  labs(title = "Curva ROC en testing",
       subtitle = "Modelo árbol de decisión") 

rf_testing_pred %>%
  yardstick::recall(truth = inten, predict)

rf_testing_pred%>%
  yardstick::precision(truth = inten, predict)


rf_testing_pred %>%
  conf_mat(truth = inten, estimate = predict)





rf_training_pred %>%
  yardstick::recall(truth = inten, predict)

rf_training_pred%>%
  yardstick::precision(truth = inten, predict)


rf_training_pred %>%
  conf_mat(truth = inten, estimate = predict)



#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------











bagged_decision_tree_hash %>% show_best("recall")



fit <-rpart::rpart(inten~., data= train_data, method = "class",
                   control = rpart::rpart.control(cp = 0.02,minsplit = 21,maxdepth = 8))

rpart.plot::rpart.plot(fit)

rf_training_pred <- 
  cbind("predict"=predict(fit,train_data, type = "class"),
        predict(fit, train_data, type = "prob"),
        train_data %>% select(inten))


rf_testing_pred <- 
  cbind("predict"=predict(fit,test_data, type = "class"),
        predict(fit, test_data, type = "prob"),
        test_data %>% select(inten))



rf_testing_pred%>%
  roc_curve(truth = inten, `<IV`:VI) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(lty = 2, color = "gray80", size = 2) +
  theme_minimal()+
  geom_path()+
  labs(title = "Curva ROC",
       subtitle = "Modelo bagged tree")

rpart.plot::rpart.plot(fit,type = 0,
                       main = "Árbol modelo elegido")



rf_testing_pred %>%
  yardstick::recall(truth = inten, predict)


library("DALEX")
exp_rf <- DALEX::explain(fit, data = test_data)

plot(fit)
library("shapper")

muestra <-tibble("prof_km"=300,
                 "inten"=as.factor("VII"),
                 "mag"=10,
                 "placa_tectonica"=as.factor(0)
)

muestra$mag <- (muestra$mag - 2.850961)/0.9478287
muestra$prof_km <- log(muestra$prof_km)


ive_rf <- shap(exp_rf, new_observation = muestra)
ive_rf

a <- plot(ive_rf)+
  labs(title = "Deep earthquake")








muestra <-tibble("prof_km"=1,
                 "inten"=as.factor("VII"),
                 "mag"=5,
                 "placa_tectonica"=as.factor(0)
)

muestra$mag <- (muestra$mag - 2.850961)/0.9478287
muestra$prof_km <- log(muestra$prof_km)


ive_rf <- shap(exp_rf, new_observation = muestra)
ive_rf

plot(ive_rf)+
  labs(title = "Terremoto superficial, magnitud media")


predict(fit,muestra, type = "class")
gridExtra::grid.arrange(a,b)





saveRDS(fit, "05_Deployment/models/arbol_intensidad_terremotos.rds")



