library(readr)
library(tidymodels)
library(modeltime)
library(baguette)
library(survival)
library(rules)



VAL_incendios <- read.csv('data/data_VAL/incendios_clean_final.csv')

VAL_incendios$fecha <- as.Date(VAL_incendios$fecha, format = "%Y-%m-%d")

VAL_incendios$coste <- VAL_incendios$gastos + VAL_incendios$perdidas

VAL_incendios <-VAL_incendios %>%
  mutate(year = as.numeric(format(fecha, format = "%Y")),
         month = as.numeric(format(fecha, format = "%m")),
         day = as.numeric(format(fecha, format = "%d")))



VAL_incendios$CCAA_riesgo <- ifelse(VAL_incendios$comunidad == "Galicia",2,
                                    ifelse(VAL_incendios$comunidad == "Aragón",1,
                                    0))


VAL_incendios$mes_sin <- sin(VAL_incendios$month*pi*2/12)
VAL_incendios$mes_cos <- cos(VAL_incendios$month*pi*2/12)

meses <- unique(data.frame(sin=VAL_incendios$mes_sin,
                    cos=VAL_incendios$mes_cos))



VAL_incendios <- VAL_incendios %>%
  filter(perdidas>=0)%>%
  select(perdidas, CCAA_riesgo, mes_cos, mes_sin, muertos, heridos, superficie) %>%
  mutate(perdidas = ifelse(perdidas==0,1, perdidas))


plot(density(log(VAL_incendios$superficie)^0.5))
plot(density(log(VAL_incendios$perdidas)))

VAL_incendios$perdidas <- log(VAL_incendios$perdidas)
VAL_incendios$superficie <-log(VAL_incendios$superficie)^0.5


e1071::skewness(VAL_incendios$perdidas)

e1071::skewness(VAL_incendios$superficie)




# SPLIT DATA

set.seed(4595)

data_split <- initial_split(VAL_incendios, strata = "perdidas", prop = 0.75)

train_data <- training(data_split)

test_data <- testing(data_split)


train_data <- recipe(perdidas~., data = train_data) %>% 
  prep() %>% 
  juice()



k_folds_data <- vfold_cv(train_data, strata = perdidas,v = 2)


hash_rec <- recipe(perdidas~., data = train_data)



#-------------------------------------------------------------------------------



# Tune Models


model_control <- control_grid(save_pred = TRUE, verbose = TRUE)
model_metrics <- metric_set(rmse, rsq)



# Tune hash models-----------------------------------------------------


# RAND FOREST


rand_forest_model <- rand_forest(trees = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rand_forest_grid <- grid_regular(parameters(rand_forest_model))



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

saveRDS(rand_forest_hash, "03_Modeling/hash_incendios/rand_forest_hash.rds")



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

saveRDS(bag_mars_hash, "03_Modeling/hash_incendios/bag_mars_hash.rds")


bag_mars_hash %>% show_best("rsq")



# BOOST TREE



boost_tree_model <- boost_tree(trees = tune(),
                               min_n = tune(),
                               tree_depth = tune())  %>%
  set_engine("xgboost") %>%
  set_mode("regression")


boost_tree_model_grid <- grid_regular(parameters(boost_tree_model))



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


saveRDS(boost_tree_model_hash, "03_Modeling/hash_incendios/boost_tree_model_hash.rds")




# KNN 




nearest_neighbor_model <- nearest_neighbor( neighbors = tune(),
                                            weight_func =  tune(),
                                            dist_power =  tune())  %>%
  set_engine("kknn") %>%
  set_mode("regression")



nearest_neighbor_grid <- grid_regular(parameters(nearest_neighbor_model))




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


saveRDS(nearest_neighbor_hash, "03_Modeling/hash_incendios/nearest_neighbor_hash.rds")


# CUBIST RULES





cubis_model <- cubist_rules(committees = tune(),
                            neighbors = tune(),
                            max_rules = tune(),)%>%
  set_engine("Cubist") %>%
  set_mode("regression")



cubis_grid <- grid_regular(parameters(cubis_model), levels = 4)




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





saveRDS(cubis_hash, "03_Modeling/hash_incendios/cubis_hash.rds")




#------------------------------------------













































































































