library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(smotefamily)
library(themis)
library(ROSE)

df <- read.csv("diabetes.csv")
str(df)
unique(df$CLASS)

df <- df |>
  mutate(CLASS = trimws(CLASS)) |>
  filter(CLASS != "P") |>
  mutate(CLASS = factor(CLASS),
         Gender = factor(Gender)) |>
  select(!c(ID,No_Pation))

unique(df$CLASS)  


# treino e teste
set.seed(1421)

n <- round(nrow(df)*0.8)
indices <- sample(1:nrow(df), size = n, replace = FALSE)
treino <- df[indices,]
teste <- df[-indices,]

# 1. o modelo knn

## 1.1 primeiro vamos determinar k por cross validation

controle <- trainControl(method = "cv",
                         number = 10,
                         verboseIter = TRUE)

possiveis_k <- expand.grid(k = 1:20) 

modelo_knn <- train(
  CLASS ~. -Gender,
  data = treino,
  method = "knn",
  tuneGrid = possiveis_k,
  preProcess = c("center", "scale")
)

modelo_knn

pred <- predict(ajuste_modelo_knn, newdata = teste)

matriz_confusao_knn <- confusionMatrix(data = pred,
                                       reference = teste$CLASS,
                                       positive = "Y")


## 1.2 vamos considerar um outro cenário: primeiro, vamos balancear os dados.

### 1.2.3 modelo com down sampling

set.seed(1457)

controle_down <- trainControl(
  method = "cv",
  number = 10,
  sampling = "down",
  classProbs = TRUE)

modelo_knn_down <- train(
  CLASS ~ ., 
  data = treino, 
  method = "knn",
  trControl = controle_down,
  tuneGrid = possiveis_k,
  preProcess = c("center", "scale")
)

pred_down <- predict(modelo_knn_down, newdata = teste)
matriz_confusao_knn_down <- confusionMatrix(data = pred_down,
                                       reference = teste$CLASS,
                                       positive = "Y")

### 1.2.3 modelo com smote

# install.packages("smotefamily")

set.seed(1512)
controle_smote <- trainControl(
  method = "cv",
  number = 10,
  sampling = "smote")

modelo_knn_smote <- train(
  CLASS ~ ., 
  data = treino, 
  method = "knn",
  trControl = controle_smote,
  tuneGrid = possiveis_k,
  preProcess = c("center", "scale")
)

pred_smote <- predict(modelo_knn_smote, newdata = teste)

matriz_confusao_knn_smote <- confusionMatrix(data = pred_smote, 
                                             reference = teste$CLASS, 
                                             positive = "Y")

matriz_confusao_knn_smote


# agora, vamos fazer o SVM

library(caret)

# Reutilizando seu trainControl original (sem balanceamento, já que venceu)
controle_svm <- trainControl(
  method = "cv", 
  number = 10
)

# Definindo a grade de busca para C (custo) e sigma (suavidade da fronteira)

grid_svm <- expand.grid(
  sigma = c(0.01, 0.05, 0.1), 
  C = c(0.25, 0.5, 1, 2, 4)
)

set.seed(123)
modelo_svm <- train(
  CLASS ~ ., 
  data = treino, 
  method = "svmRadial",  # SVM com kernel radial
  trControl = controle_svm,
  tuneGrid = grid_svm,
  preProcess = c("center", "scale")
)

pred_svm <- predict(modelo_svm, newdata = teste)
confusionMatrix(pred_svm, teste$CLASS)