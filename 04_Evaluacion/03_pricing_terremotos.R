library(tidyverse)
library(tidymodels)

VAL_terremotos_modelo_intensidad <- readRDS(file = "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")

VAL_terremotos_modelo_intensidad$inten <- as.factor(VAL_terremotos_modelo_intensidad$inten)




VAL_terremotos_modelo_intensidad <- VAL_terremotos_modelo_intensidad %>%
  mutate(prof_km = if_else(is.na(prof_km) & inten=="<IV",11,
                           ifelse(is.na(prof_km) & inten=="IV",10.5,
                           ifelse(is.na(prof_km) & inten=="V", 10,
                           ifelse(is.na(prof_km) & inten=="VI", 6, prof_km))) ))


VAL_terremotos_modelo_intensidad <- recipe(~., VAL_terremotos_modelo_intensidad) %>%
  step_upsample(inten, over_ratio = 0.11) %>%
  prep() %>%
  bake(new_data = NULL)


VAL_terremotos_modelo_intensidad$mag <- (VAL_terremotos_modelo_intensidad$mag - 2.850961)/0.9478287
VAL_terremotos_modelo_intensidad$prof_km <- log(VAL_terremotos_modelo_intensidad$prof_km)



rand_forest_hash <- readRDS( "03_Modeling/hash/rand_forest_hash.rds")
LDA_hash <- readRDS( "03_Modeling/hash/LDA_hash.rds")
rdm_hash <- readRDS( "03_Modeling/hash/rdm_hash.rds")
naive_Bayes_hash <- readRDS( "03_Modeling/hash/naive_Bayes_hash.rds")
multinom_reg_hash <- readRDS( "03_Modeling/hash/multinom_reg_hash.rds")
c5_rules_hash <- readRDS( "03_Modeling/hash/c5_rules_hash.rds")
bagged_decision_tree_hash <- readRDS( "03_Modeling/hash/bagged_decision_tree_hash.rds")
boost_tree_model_hash <- readRDS( "03_Modeling/hash/boost_tree_model_hash.rds")
svm_poly_hash <- readRDS( "03_Modeling/hash/svm_poly_hash.rds")
svm_rbf_hash <- readRDS( "03_Modeling/hash/svm_rbf_hash.rds")
nearest_neighbor_hash <- readRDS( "03_Modeling/hash/nearest_neighbor_hash.rds")



# SPLIT DATA

set.seed(12345)

data_split <- initial_split(VAL_terremotos_modelo_intensidad, strata = "inten", prop = 0.75)

train_data <- training(data_split)

test_data <- testing(data_split)


train_data <- recipe(inten~., data = train_data) %>% 
  prep() %>% 
  juice()



# RAND FOREST

autoplot(rand_forest_hash)

rand_forest_hash %>% show_best("recall")

final_param <- rand_forest_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1)%>%
  select(trees, min_n)
final_param


rand_forest_model <- rand_forest(trees = 2000,
                                 min_n = 2) %>%
  set_engine("ranger") %>%
  set_mode("classification")



fit <- rand_forest_model %>%
  fit(inten~., data= train_data)

saveRDS(fit, "04_Evaluacion/trained_models_terremotos/bag_mars_trained.rds")


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
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/discrim_linear_trained.rds")



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
  select(frac_common_cov , frac_identity )



model <- discrim_regularized(
  mode = "classification",
  engine = "klaR",
  frac_common_cov =  0,
  frac_identity =  0)



fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/discrim_regularized_trained.rds")




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
  select(smoothness, Laplace)


model <-naive_Bayes(
  mode = "classification",
  engine = "klaR",
  smoothness = 0.5,
  Laplace = 3)


fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/naive_Bayes_trained.rds")




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
  penalty = 0.00000000214  
)

fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/multinom_reg_trained.rds")




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
  select(trees)


model <- C5_rules(mode = "classification", 
                  trees = 3, 
                  engine = "C5.0")


fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/C5_rules_trained.rds")




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


final_param <- bagged_decision_tree_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(tree_depth , min_n)

model <- bag_tree(tree_depth = 15,
                  min_n = 40)  %>%
  set_engine("rpart") %>%
  set_mode("classification")



fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/bag_tree_trained.rds")




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
  select(trees)

model <- boost_tree(trees = 97)  %>%
  set_engine("C5.0") %>%
  set_mode("classification")


fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/boost_tree_model_hash_trained.rds")




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
  select(degree , margin )



model <- svm_poly(degree = 3,
                  margin = 0)  %>%
  set_engine("kernlab") %>%
  set_mode("classification")


fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/svm_poly_trained.rds")




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
  select(cost, rbf_sigma,margin)


model <- svm_rbf( cost = 32,
                  rbf_sigma = 1,
                  margin = 0)  %>%
  set_engine("kernlab") %>%
  set_mode("classification")

fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/svm_rbf_trained.rds")




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
  select(neighbors,dist_power)



model <- nearest_neighbor( neighbors = 8,
                           dist_power =  1.05)  %>%
  set_engine("kknn") %>%
  set_mode("classification")


fit <- model %>%
  fit(inten~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_terremotos/nearest_neighbor_trained.rds")




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
  roc_curve(truth = inten, `.pred_<IV`:.pred_VI) %>%
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

#------------------------------------------------------------------------

library("DALEX")
exp_rf <- DALEX::explain(fit, data = test_data)

plot(fit)
library("shapper")

muestra <-tibble("prof_km"=300,
                 "inten"=as.factor("na"),
                 "mag"=10,
                 "placa_tectonica"=as.factor(0)
)

muestra$mag <- (muestra$mag - 2.850961)/0.9478287
muestra$prof_km <- log(muestra$prof_km)


ive_rf <- shap(exp_rf, new_observation = muestra)


a <- plot(ive_rf)+
  labs(title = "Deep earthquake")

a






muestra2 <-tibble("prof_km"=1,
                 "inten"=as.factor("na"),
                 "mag"=6.1,
                 "placa_tectonica"=as.factor(1)
)

muestra2$mag <- (muestra2$mag - 2.850961)/0.9478287
muestra2$prof_km <- log(muestra2$prof_km)


ive_rf <- shap(exp_rf, new_observation = muestra2)
ive_rf

plot(ive_rf)+
  labs(title = "Terremoto superficial, magnitud media")


predict(fit,muestra, type = "class")
gridExtra::grid.arrange(a,b)





saveRDS(fit, "05_Deployment/models/arbol_intensidad_terremotos.rds")



