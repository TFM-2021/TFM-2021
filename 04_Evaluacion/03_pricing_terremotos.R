

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

model <- bag_tree(cost_complexity = .00000316 ,
                  tree_depth = 15,
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


rand_forest_model_best <- rand_forest(trees = 2000,
                                 min_n = 2) %>%
  set_engine("ranger") %>%
  set_mode("classification")





fit <- rand_forest_model_best %>%
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
  roc_curve(truth = inten, `.pred_<IV`:.pred_VII) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path() +
  coord_equal() +
  labs(color = NULL)
View(rf_testing_pred)

rf_testing_pred %>%
  yardstick::recall(truth = inten, .pred_class)

rf_testing_pred %>%
  conf_mat(truth = inten, estimate = .pred_class)





#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------











bagged_decision_tree_hash %>% show_best("recall")



model <- bag_tree(cost_complexity = .00000316 ,
                  tree_depth = 15,
                  min_n = 2)  %>%
  set_engine("rpart") %>%
  set_mode("classification")



fit <- model %>%
  fit(inten~., data= train_data)


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(predict(fit, train_data, type = "prob")) %>% 
  bind_cols(train_data %>% select(inten))



rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(predict(fit, test_data, type = "prob")) %>% 
  bind_cols(test_data %>% select(inten))







rf_testing_pred %>%
  roc_curve(truth = inten, `.pred_<IV`:.pred_VI) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(lty = 2, color = "gray80", size = 2) +
  theme_minimal()+
  geom_path()+
  labs(title = "Curva ROC",
       subtitle = "Modelo bagged tree")





library("DALEXtra")

wflow <- workflow() %>%
  add_recipe( recipe(inten ~ ., data = train_data)) %>%
  add_model(model) %>%
  fit(train_data)




a <- explain_tidymodels(wflow, data = train_data, y = train_data$inten)
a$y_hat

pdp_time <- model_profile(
  a,
  variables = "inten",
  N = NULL,
  groups = "type"
)
b <-model_profile(a)
plot(b)



as_tibble(b$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow.")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.5) +
  labs(
    x = "Time to complete track",
    y = "Predicted probability of shortcut",
    title = "Partial dependence plot for Mario Kart world records",
    subtitle = "Predictions from a decision tree model"
  )
