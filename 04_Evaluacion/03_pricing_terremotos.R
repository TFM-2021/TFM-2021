

cubist <- cubist_rules(committees = 34,
                       neighbors = 9,
                       max_rules = 1)%>%
  set_engine("Cubist") %>%
  set_mode("regression")



cubist_fit <- cubist %>%
  fit(Sepal.Length ~ .,
      data = train_data
  )




results_train <- cubist_fit %>%
  predict(new_data = train_data) %>%
  mutate(
    truth = train_data$Sepal.Length,
    model = "lm"
  ) %>%
  bind_rows(rf_fit %>%
              predict(new_data = train_data) %>%
              mutate(
                truth = train_data$Sepal.Length,
                model = "rf"
              ))

results_test <- lm_fit %>%
  predict(new_data = test_data) %>%
  mutate(
    truth = test_data$Sepal.Length,
    model = "lm"
  ) %>%
  bind_rows(rf_fit %>%
              predict(new_data = test_data) %>%
              mutate(
                truth = test_data$Sepal.Length,
                model = "rf"
              ))

