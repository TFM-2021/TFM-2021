

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


# LDA ---------------------------------------------------------------------


autoplot(LDA_hash)

LDA_hash %>% show_best("roc_auc")

final_param <- LDA_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(penalty)


# Regularized discriminant analysis--------------------------------------



autoplot(rdm_hash)

rdm_hash %>% show_best("recall")

final_param <- rdm_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1) %>%
  select(trees, min_n)




# NAIVE BAYES-------------------------------------------------------------------


autoplot(naive_Bayes_hash)

naive_Bayes_hash %>% show_best("recall")

final_param <- naive_Bayes_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



# REGERSION MULTINOMIAL--------------------------------------------------------

autoplot(multinom_reg_hash)

multinom_reg_hash %>% show_best("recall")

final_param <- multinom_reg_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



# C5.0 Rule-Based---------------------------------------------------------------

autoplot(c5_rules_hash)

c5_rules_hash %>% show_best("roc_auc")

final_param <- c5_rules_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



# BAGS MARS-----------------------------------------------------------------------



autoplot(bag_mars_hash)

bag_mars_hash %>% show_best("roc_auc")


final_param <- bag_mars_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



#  BAGGED DECISION TREE--------------------------------------------------------


autoplot(bagged_decision_tree_hash)

bagged_decision_tree_hash %>% show_best("recall")


final_param <- bag_mars_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)




# BOOST TREE--------------------------------------------------------------------


autoplot(boost_tree_model_hash)

boost_tree_model_hash %>% show_best("roc_auc")

final_param <- boost_tree_model_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)



# SVM PLOY MODEL----------------------------------------------------------------



autoplot(svm_poly_hash)

svm_poly_hash %>% show_best("roc_auc")


final_param <- svm_poly_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)





# SVM RBF MODEL-----------------------------------------------------------------



autoplot(svm_rbf_hash)

svm_rbf_hash %>% show_best("roc_auc")

final_param <- svm_rbf_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(trees, min_n)


# KNN ----------------------------------------------------------------------------



autoplot(nearest_neighbor_hash)

nearest_neighbor_hash %>% show_best("roc_auc")


final_param <- nearest_neighbor_hash %>% 
  show_best("recall") %>% 
  dplyr::slice(1 )%>%
  select(neighbors, weight_func,dist_power)






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
  yardstick::roc_auc(truth = inten, `.pred_<IV`:.pred_VII)

rf_testing_pred %>%
  conf_mat(truth = inten, estimate = .pred_class)







