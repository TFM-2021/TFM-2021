model <- bag_tree(cost_complexity = 0.02, tree_depth = 15,
                  min_n = 2)  %>%
  set_engine("rpart") %>%
  set_mode("classification")


fit <- model %>%
  fit(inten~., data= train_data)

muestra <-tibble("prof_km"=6,
          "mag"=7,
          "placa_tectonica"=as.factor(1),
          "inten"=as.factor("VII"))

muestra$mag <- (muestra$mag - 2.850961)/0.9478287
muestra$prof_km <- log(muestra$prof_km)

rf_training_pred <- 
  predict(fit, muestra) %>% 
  bind_cols(predict(fit, muestra, type = "prob")) %>% 
  bind_cols(muestra %>% select(inten))

rf_training_pred 



predict(s,muestra)



s <- rpart::rpart(inten~., data= train_data, method = "class",
             control = rpart::rpart.control(cp = 0.02,minsplit = 2,maxdepth = 15))

rpart.plot::rpart.plot(s)
predict(s,muestra)




##############################################################################

muestra <-tibble("prof_km"=50,
                 "inten"=as.factor("VII"),
                 "mag"=6,
                 "placa_tectonica"=as.factor(1)
                 )

muestra$mag <- (muestra$mag - 2.850961)/0.9478287
muestra$prof_km <- log(muestra$prof_km)

muestra
rf_training_pred <- 
  predict(fit, muestra) %>% 
  bind_cols(predict(fit, muestra, type = "prob")) %>% 
  bind_cols(muestra %>% select(inten))

rf_training_pred 


bag
predict(s,muestra, type = "class")

train_data %>%
  filter(prof_km >3)

s <- rpart::rpart(inten~., data= train_data, method = "class",
                  control = rpart::rpart.control(cp = 0.02,minsplit = 2,maxdepth = 15))

rpart.plot::rpart.plot(s)
prediccion_1 <-predict(s,test_data, type = "prob")

library(caret)
confusionMatrix(prediccion_1, test_data$inten)






cbind("predict"=predict(s,test_data, type = "class"),
      predict(s, test_data, type = "prob"),
      test_data %>% select(inten))%>%
  roc_curve(truth = i nten, `<IV`:VI) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(lty = 2, color = "gray80", size = 2) +
  theme_minimal()+
  geom_path()+
  labs(title = "Curva ROC",
       subtitle = "Modelo bagged tree")




cbind("predict"=predict(s,test_data, type = "class"),
      predict(s, test_data, type = "prob"),
      test_data %>% select(inten))%>%
  conf_mat(truth = inten, estimate = predict)

