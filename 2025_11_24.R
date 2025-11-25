library(ggplot2)
#instalar dplyr
library(dplyr)
#intalar class
library(class)

dados <- read.csv("penguins.csv", header = TRUE)

# dplyr é um pacote para manipulação dos dados

# filter, select, mutate

machos <- filter(dados, sex == "MALE")
dados[dados$sex == "MALE",]
View(machos)
#vamos fazer para as fêmeas
femeas <- filter(dados, sex == "FEMALE")

machos <- dados |>
  filter(sex == "MALE") 

adelie <- dados |>
  filter(species == "Adelie")

adelie_macho <- dados |>
  filter(sex == "MALE") |>
  filter(species == "Adelie")

#outra forma de fazer isso

adelie_macho2 <- dados |>
  filter(species == "Adelie", sex == "MALE")

adelie_biscoe <- dados |>
  filter(species == "Adelie", island == "Biscoe")

# agora vamos aprender a utilizar o verbo selecionar (select) colunas

dados |>
  select(species, island)

dados |>
  select(species, island) |>
  filter(species == "Adelie")

dados |>
  select(-island)

dados |>
  select(-c(island, species))

#último verbo: fazer uma mutação (mutate) nas colunas

dados <- dados |>
  mutate(peso_kg = body_mass_g/1000)

dados |>
  na.omit()

# remover peso_kg
# remover dados faltantes
# remover strings vazias do sexo
# remover ilha e sexo
# embaralhar as linhas dos dados
# selecionar 80% para treino e 20% para teste

dados_limpos <- dados |>
  select(-peso_kg) |>
  na.omit() |>
  filter(sex != "") |>
  select(-island, -sex)

View(dados_limpos)  

set.seed(1903)
dados_limpos <- dados_limpos[sample(nrow(dados_limpos)),]

n <- round(0.8*nrow(dados_limpos))
n

treino <- dados_limpos[1:n,]
teste <- dados_limpos[-(1:n),]

treino |>
  ggplot(mapping = aes(x = bill_length_mm, y = flipper_length_mm, col = species)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "bottom")

treino |>
  ggplot(mapping = aes(x = species, y = flipper_length_mm)) +
  geom_boxplot() +
  theme_minimal()

?scale
scale(treino)

treino_padronizado <- scale(treino[,-1])
teste_padronizado <- scale(teste[,-1])

sd(treino_padronizado[,4])

treino_padronizado2 <- treino |>
  select(-species) |>
  scale()

?knn

classificacao <- knn(train = treino_padronizado,
                     test = teste_padronizado,
                     cl = treino$species,
                     k = 7)

mean(teste$species == classificacao)
