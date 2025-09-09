library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(e1071)

diabetes <- read.csv("diabetes.csv")
summary(diabetes)

unique(diabetes$CLASS)

# trabalhando apenas com a previsão de ter ou não ter diabetes e ajustando as respostas da variável CLASS; variaveis CLASS e Gender sofreram mutação para serem categóricas;
diabetes <- diabetes |>
  select(-c(ID, No_Pation)) |>
  filter(CLASS != "P") |>
  mutate(CLASS = case_when(
    CLASS ==  "N " ~ "N",
    CLASS == "Y " ~ "Y",
    TRUE ~ CLASS
  ),
  Gender = case_when(
    Gender == "f" ~ "F",
    TRUE ~ Gender
  )) |>
  mutate(across(c("Gender", "CLASS"), as.factor))

unique(diabetes$CLASS)
summary(diabetes)

# construindo treino e teste
set.seed(2151)
diabetes <- diabetes[sample(nrow(diabetes)),]
n <- round(nrow(diabetes)*0.8)
n
treino <- diabetes[1:n,]  
teste <- diabetes[-(1:n),]  

treino |>
  ggplot(aes(x = CLASS, y = BMI)) +
  geom_boxplot() +
  theme_minimal()

treino |>
  ggplot(aes(x = CLASS, y = HbA1c)) +
  geom_boxplot() +
  theme_minimal()

treino |>
  ggplot(aes(x = HbA1c, fill = CLASS)) +
  geom_density(alpha = 0.6) +
  theme_minimal()

# criando a árvore de decisão, realizando previsões e construindo a matriz de confusão
arvore.diabetes <- rpart(CLASS ~ .,
                         data = treino,
                         method = "class")  
rpart.plot(arvore.diabetes, extra = 101)

previsao.diabetes <- predict(arvore.diabetes, newdata = teste, type = "class")

mean(previsao.diabetes == teste$CLASS)

table(teste$CLASS, previsao.diabetes)