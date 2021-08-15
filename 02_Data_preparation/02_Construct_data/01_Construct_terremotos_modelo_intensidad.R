

terremotos_modelo_intensidad <- readRDS(file = "02_Data_preparation/01_Clean_data/terremotos_clean_modelo_intensidad.rds")


terremotos_modelo_intensidad <- terremotos_modelo_intensidad %>%
  dplyr::mutate(inten = replace(inten, inten == "I", "<IV"),
                inten = replace(inten, inten == "I-II", "<IV"),
                inten = replace(inten, inten == "II", "<IV"),
                inten = replace(inten, inten == "II-III", "<IV"),
                inten = replace(inten, inten == "III", "<IV"),
                inten = replace(inten, inten == "III-IV", "IV"),
                inten = replace(inten, inten == "IV-V", "V"))

unique(terremotos_modelo_intensidad$inten)
terremotos_modelo_intensidad$inten <- as.factor(terremotos_modelo_intensidad$inten)

set.seed(157)
# Stratified sampling
TrainingDataIndex <- createDataPartition(terremotos_modelo_intensidad$inten, p=0.8, list = FALSE)
# Create Training Data 
trainingData <- terremotos_modelo_intensidad[TrainingDataIndex,]
testData <- terremotos_modelo_intensidad[-TrainingDataIndex,]
TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=10)


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

NaivePredictions <-predict(NaiveModel, testData, na.action = na.pass)
cmNaive <-confusionMatrix(NaivePredictions, testData$inten)
print(cmNaive)
