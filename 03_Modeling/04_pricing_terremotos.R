
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

VAL_terremotos_modelo_intensidad <- readRDS(file = "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")





# IMPUTACION


VAL_terremotos_modelo_intensidad %>%
  filter(is.na(prof_km))

VAL_terremotos_modelo_intensidad %>%
  group_by(inten)%>%
  summarise(median(prof_km, na.rm=T))


VAL_terremotos_modelo_intensidad <- VAL_terremotos_modelo_intensidad %>%
  mutate(prof_km = if_else(is.na(prof_km) & inten=="<IV",11,
                          ifelse(is.na(prof_km) & inten=="IV",10.5,
                          ifelse(is.na(prof_km) & inten=="V", 10,
                          ifelse(is.na(prof_km) & inten=="VI", 6, prof_km))) ))




# OVERSAMPLING



VAL_terremotos_modelo_intensidad %>%
  ggplot(aes(inten))+
  geom_bar(aes(fill=inten))+
  theme_minimal()+
  
  labs(title = "Número de valores",
       subtitle = "Variable objetivo") +
  geom_text(stat='count', aes(label=..count..), vjust=1.2)


recipe(~., VAL_terremotos_modelo_intensidad) %>%
  step_upsample(inten, over_ratio = 0.11) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  ggplot(aes(inten))+
  geom_bar(aes(fill=inten))+
  theme_minimal()+
  
  labs(title = "Número de valores - oversampling",
       subtitle = "Variable objetivo") +
  geom_text(stat='count', aes(label=..count..), vjust=1.2)

VAL_terremotos_modelo_intensidad <- recipe(~., VAL_terremotos_modelo_intensidad) %>%
  step_upsample(inten, over_ratio = 0.11) %>%
  prep() %>%
  bake(new_data = NULL)




VAL_terremotos_modelo_intensidad$prof_km <- log(VAL_terremotos_modelo_intensidad$prof_km)

VAL_terremotos_modelo_intensidad[,c(1,3)] <- scale(VAL_terremotos_modelo_intensidad[,c(1,3)])






# SPLIT DATA

set.seed(12345)

data_split <- initial_split(VAL_terremotos_modelo_intensidad, strata = "inten", prop = 0.75)

train_data <- training(data_split)

test_data <- testing(data_split)


train_data <- recipe(inten~., data = train_data) %>% 
  prep() %>% 
  juice()


k_folds_data <- vfold_cv(train_data, strata = inten,v = 5)


hash_rec <- recipe(inten~., data = train_data)




# SHIFT FUNCTION


a <- ggplot()+ 
  geom_density(test_data,mapping = aes(log(prof_km)))+
  geom_density(train_data,mapping = aes(log(prof_km)))+
  theme_minimal()


b <- ggplot()+ 
  geom_density(test_data,mapping = aes(inten ,fill=inten), alpha =0.15)+
  geom_density(train_data,mapping = aes(inten ,fill=inten), alpha =0.15)+
  theme_minimal()

c <- ggplot()+ 
  geom_density(test_data,mapping = aes(mag ))+
  geom_density(train_data,mapping = aes(mag ))+
  theme_minimal()


d <- ggplot()+ 
  geom_density(test_data,mapping = aes(placa_tectonica , color= placa_tectonica ))+
  geom_density(train_data,mapping = aes(placa_tectonica, color= placa_tectonica ))+
  theme_minimal()


gridExtra::grid.arrange(a,b,c,d)







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



model_control <- control_grid(save_pred = TRUE, verbose = T)
model_metrics <- metric_set(recall,precision,roc_auc)

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
#autoplot(rand_forest_hash)

rand_forest_hash %>% show_best("recall")



saveRDS(rand_forest_hash, "03_Modeling/hash/rand_forest_hash.rds")






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
#autoplot(LDA_hash)

LDA_hash %>% show_best("roc_auc")


saveRDS(LDA_hash, "03_Modeling/hash/LDA_hash.rds")





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
#autoplot(rdm_hash)

rdm_hash %>% show_best("recall")


saveRDS(rdm_hash, "03_Modeling/hash/rdm_hash.rds")


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
#autoplot(naive_Bayes_hash)

naive_Bayes_hash %>% show_best("recall")

saveRDS(naive_Bayes_hash, "03_Modeling/hash/naive_Bayes_hash.rds")


# REGERSION MULTINOMIAL--------------------------------------------------------


multinom_reg_model <- multinom_reg(
  mode = "classification",
  engine = "nnet",
  penalty = tune()
)


grid_random(parameters(multinom_reg_model))
multinom_reg_grid <- grid_regular(parameters(multinom_reg_model), 50)
multinom_reg_grid <- grid_random(parameters(multinom_reg_model),size = 50)

registerDoParallel(cl)

multinom_reg_hash <- tune_grid(
  multinom_reg_model,
  hash_rec,
  grid = multinom_reg_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)
#autoplot(multinom_reg_hash)

multinom_reg_hash %>% show_best("recall")


saveRDS(multinom_reg_hash, "03_Modeling/hash/multinom_reg_hash.rds")


# C5.0 Rule-Based---------------------------------------------------------------

c5_rules_model <- C5_rules(mode = "classification", 
         trees = tune(), 
         engine = "C5.0")



c5_rules_grid <- grid_random(parameters(c5_rules_model), size = 10)
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



#autoplot(c5_rules_hash)

c5_rules_hash %>% show_best("roc_auc")

saveRDS(c5_rules_hash, "03_Modeling/hash/c5_rules_hash.rds")



#  BAGGED DECISION TREE--------------------------------------------------------



bagged_decision_tree_model <- bag_tree(cost_complexity = 0.02,
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

#autoplot(bagged_decision_tree_hash)

bagged_decision_tree_hash %>% show_best("recall")




saveRDS(bagged_decision_tree_hash, "03_Modeling/hash/bagged_decision_tree_hash.rds")


# BOOST TREE--------------------------------------------------------------------



boost_tree_model <- boost_tree(trees = tune())  %>%
  set_engine("C5.0") %>%
  set_mode("classification")


boost_tree_model_grid <- grid_regular(parameters(boost_tree_model), levels = 50)

registerDoParallel(cl)

boost_tree_model_hash <- tune_grid(
  boost_tree_model,
  hash_rec,
  grid = boost_tree_model_grid,
  control = model_control,
  metrics = model_metrics,
  resamples = k_folds_data
)

#autoplot(boost_tree_model_hash)

boost_tree_model_hash %>% show_best("roc_auc")

saveRDS(boost_tree_model_hash, "03_Modeling/hash/boost_tree_model_hash.rds")



# SVM PLOY MODEL----------------------------------------------------------------



svm_poly_model <- svm_poly(degree = tune(),
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

#autoplot(svm_poly_hash)

svm_poly_hash %>% show_best("recall")





saveRDS(svm_poly_hash, "03_Modeling/hash/svm_poly_hash.rds")



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

#autoplot(svm_rbf_hash)

svm_rbf_hash %>% show_best("roc_auc")


saveRDS(svm_rbf_hash, "03_Modeling/hash/svm_rbf_hash.rds")


# KNN ----------------------------------------------------------------------------

library(kknn)


nearest_neighbor_model <- nearest_neighbor( neighbors = tune(),
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

#autoplot(nearest_neighbor_hash)

nearest_neighbor_hash %>% show_best("roc_auc")


saveRDS(nearest_neighbor_hash, "03_Modeling/hash/nearest_neighbor_hash.rds")



