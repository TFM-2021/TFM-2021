library(tidyverse)
library(tidymodels)
library(rules)
library(baguette)

VAL_incendios <- read.csv('data/data_VAL/incendios_clean_final.csv')



VAL_incendios$fecha <- as.Date(VAL_incendios$fecha, format = "%Y-%m-%d")

VAL_incendios$coste <- VAL_incendios$gastos + VAL_incendios$perdidas

VAL_incendios <- VAL_incendios %>%
  mutate(year = as.numeric(format(fecha, format = "%Y")),
         month = as.numeric(format(fecha, format = "%m")),
         day = as.numeric(format(fecha, format = "%d")))


VAL_incendios$CCAA_riesgo <- ifelse(VAL_incendios$comunidad == "Galicia",2,
                                    ifelse(VAL_incendios$comunidad == "Aragón",1,
                                           0))

VAL_incendios$mes_sin <- sin(VAL_incendios$month*pi*2/12)
VAL_incendios$mes_cos <- cos(VAL_incendios$month*pi*2/12)


VAL_incendios <- VAL_incendios %>%
  filter(perdidas>=0)%>%
  select(perdidas, CCAA_riesgo, mes_cos, mes_sin, muertos, heridos, superficie) %>%
  mutate(perdidas = ifelse(perdidas==0,1, perdidas))



VAL_incendios$perdidas <- log(VAL_incendios$perdidas)
VAL_incendios$superficie <-log(VAL_incendios$superficie)^0.5







bag_mars_hash <- readRDS("03_Modeling/hash_incendios/bag_mars_hash.rds")
boost_tree_model_hash <- readRDS("03_Modeling/hash_incendios/boost_tree_model_hash.rds")
cubis_hash <- readRDS("03_Modeling/hash_incendios/cubis_hash.rds")
nearest_neighbor_hash <- readRDS("03_Modeling/hash_incendios/nearest_neighbor_hash.rds")
rand_forest_hash <- readRDS("03_Modeling/hash_incendios/rand_forest_hash.rds")



# SPLIT DATA

set.seed(4595)

data_split <- initial_split(VAL_incendios, strata = "perdidas", prop = 0.75)

train_data <- training(data_split)

test_data <- testing(data_split)


train_data <- recipe(perdidas~., data = train_data) %>% 
  prep() %>% 
  juice()




# BAG MARS


autoplot(bag_mars_hash)

bag_mars_hash %>% show_best("rmse")

final_param <- bag_mars_hash %>% 
  show_best("rmse") %>% 
  dplyr::slice(1)%>%
  select(num_terms , prod_degree ,prune_method )


model <- bag_mars(num_terms = 5,
                  prod_degree = 2)  %>%
  set_engine("earth") %>%
  set_mode("regression")

fit <- model %>%
  fit(perdidas~., data= train_data)

saveRDS(fit, "04_Evaluacion/trained_models_incendios/bag_mars_trained.rds")

rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(train_data %>% select(perdidas))


rf_training_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_training_pred %>%
  yardstick::rsq(perdidas, .pred)




rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(test_data %>% select(perdidas))


rf_testing_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_testing_pred %>%
  yardstick::rsq(perdidas, .pred)




# BOOST TREE



autoplot(boost_tree_model_hash)

boost_tree_model_hash %>% show_best("rmse")

final_param <- boost_tree_model_hash %>% 
  show_best("rmse") %>% 
  dplyr::slice(1)%>%
  select(trees, min_n,tree_depth )

model <- boost_tree(trees = 1000,
                    min_n = 40,
                    tree_depth = 8)  %>%
  set_engine("xgboost") %>%
  set_mode("regression")

fit <- model %>%
  fit(perdidas~., data= train_data)

saveRDS(fit, "04_Evaluacion/trained_models_incendios/boost_tree_trained.rds")


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(train_data %>% select(perdidas))


rf_training_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_training_pred %>%
  yardstick::rsq(perdidas, .pred)




rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(test_data %>% select(perdidas))


rf_testing_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_testing_pred %>%
  yardstick::rsq(perdidas, .pred)



# CUBIS


autoplot(cubis_hash)

cubis_hash %>% show_best("rmse")

final_param <- cubis_hash %>% 
  show_best("rmse") %>% 
  dplyr::slice(1)%>%
  select(committees , neighbors ,max_rules )


model <- cubist_rules(committees = 100,
                       neighbors = 9,
                       max_rules = 167)%>%
  set_engine("Cubist") %>%
  set_mode("regression")




fit <- model %>%
  fit(perdidas~., data= train_data)

saveRDS(fit, "04_Evaluacion/trained_models_incendios/cubist_rules_trained.rds")


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(train_data %>% select(perdidas))


rf_training_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_training_pred %>%
  yardstick::rsq(perdidas, .pred)




rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(test_data %>% select(perdidas))


rf_testing_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_testing_pred %>%
  yardstick::rsq(perdidas, .pred)



# KNN nearest_neighbor

autoplot(nearest_neighbor_hash)

nearest_neighbor_hash %>% show_best("rmse")

final_param <- nearest_neighbor_hash %>% 
  show_best("rmse") %>% 
  dplyr::slice(1)%>%
  select(neighbors , weight_func ,dist_power )

model <- nearest_neighbor( neighbors =15 ,
                            weight_func = "triangular",
                            dist_power =  0.1)  %>%
  set_engine("kknn") %>%
  set_mode("regression")


fit <- model %>%
  fit(perdidas~., data= train_data)

saveRDS(fit, "04_Evaluacion/trained_models_incendios/nearest_neighbor_trained.rds")


rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(train_data %>% select(perdidas))


rf_training_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_training_pred %>%
  yardstick::rsq(perdidas, .pred)




rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(test_data %>% select(perdidas))


rf_testing_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_testing_pred %>%
  yardstick::rsq(perdidas, .pred)



# RAND FORES


autoplot(rand_forest_hash)

rand_forest_hash %>% show_best("rmse")

final_param <- rand_forest_hash %>% 
  show_best("rmse") %>% 
  dplyr::slice(1)%>%
  select(trees)


model <- rand_forest(trees = 2000) %>%
  set_engine("ranger") %>%
  set_mode("regression")



fit <- model %>%
  fit(perdidas~., data= train_data)
saveRDS(fit, "04_Evaluacion/trained_models_incendios/rand_forest_trained.rds")



rf_training_pred <- 
  predict(fit, train_data) %>% 
  bind_cols(train_data %>% select(perdidas))


rf_training_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_training_pred %>%
  yardstick::rsq(perdidas, .pred)




rf_testing_pred <- 
  predict(fit, test_data) %>% 
  bind_cols(test_data %>% select(perdidas))


rf_testing_pred %>%
  yardstick::rmse(perdidas, .pred)

rf_testing_pred %>%
  yardstick::rsq(perdidas, .pred)








rf_testing_pred %>%
  unnest(.pred) %>%
  ggplot(aes(perdidas, .pred)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted",
    color = NULL
  )
