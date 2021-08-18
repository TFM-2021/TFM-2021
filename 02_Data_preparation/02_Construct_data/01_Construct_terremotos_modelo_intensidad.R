library(tidyverse)

terremotos_modelo_intensidad <- readRDS(file = "02_Data_preparation/01_Clean_data/terremotos_clean_modelo_intensidad.rds")


terremotos_modelo_intensidad <- terremotos_modelo_intensidad %>%
  dplyr::mutate(inten = replace(inten, inten == "I", "<IV"),
                inten = replace(inten, inten == "I-II", "<IV"),
                inten = replace(inten, inten == "II", "<IV"),
                inten = replace(inten, inten == "II-III", "<IV"),
                inten = replace(inten, inten == "III", "<IV"),
                inten = replace(inten, inten == "III-IV", "IV"),
                inten = replace(inten, inten == "IV-V", "V"))%>%
  filter(inten != "VII")


terremotos_modelo_intensidad$inten <- as.factor(terremotos_modelo_intensidad$inten)

terremotos_modelo_intensidad$prof_km <- log(terremotos_modelo_intensidad$prof_km)

library(recipes)
library(modeldata)
library(themis)

terremotos_modelo_intensidad <- recipe(inten ~ ., data = terremotos_modelo_intensidad) %>%
  step_adasyn(inten) %>%
  prep() %>%
  bake(new_data = NULL) #%>%
  ggplot(aes(prof_km    , mag, color = inten)) +
  geom_jitter() +
  labs(title = "With ADASYN")

terremotos_modelo_intensidad %>%
  ggplot(aes(log(prof_km)   , log(mag), color = inten)) +
  geom_point()

library(caret)


set.seed(1123)
# Stratified sampling
TrainingDataIndex <- createDataPartition(terremotos_modelo_intensidad$inten, p=0.5, list = FALSE)
# Create Training Data 
trainingData <- terremotos_modelo_intensidad[TrainingDataIndex,]
testData <- terremotos_modelo_intensidad[-TrainingDataIndex,]
TrainingParameters <- trainControl(method = "repeatedcv", number = 2, repeats=10)

skimr::skim(trainingData)
skimr::skim(testData)
#Naive algorithm
NaiveModel <- train(trainingData, trainingData$inten, 
                    method = "nb",
                    preProcess=c("scale","center"),
                    trControl= TrainingParameters,
                    na.action = na.omit
)

#Predictions
NaivePredictions <-predict(NaiveModel, trainingData, na.action = na.pass)
cmNaive <-confusionMatrix(NaivePredictions, trainingData$inten)
print(cmNaive)


NaivePredictions <-predict(NaiveModel, terremotos_modelo_intensidad, na.action = na.pass)
cmNaive <-confusionMatrix(NaivePredictions, terremotos_modelo_intensidad$inten)
print(cmNaive)
 
unique(terremotos_modelo_intensidad$inten)

tree <- rpart(inten~ mag+ prof_km, data = terremotos_modelo_intensidad, cp=.01)
rpart.plot::rpart.plot(tree,  box.palette="RdBu", shadow.col="gray", nn=TRUE)

