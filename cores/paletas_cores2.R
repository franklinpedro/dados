# nesta aula, vamos falar sobre paletas de cores e acessibilidade; na primeira parte, exploraremos forma de obter cores da natureza para criação de paletas; depois falaremos de acessibilidade e por fim usaremos cores para contar uma história.

# install.packages("colorfindr")
library(colorfindr) # o pacote colorfindr tem uma função chamada get_colors() que extrai as cores de uma imagem, e depois podemos usar a função make_palette() para criar uma paleta de cores a partir dessas cores extraídas.

# Extraindo paleta de uma imagem
paleta_cerrado <- get_colors("cerrado.jpg") |> 
  make_palette(n = 3)

# poderíamos usar essa paleta para criar um gráfico no ggplot com:
# scale_fill_manual(values = paleta_cerrado)

# agora vamos falar de acessibilidade, e para isso vamos usar o pacote colorBlindness, que tem uma função chamada cvdPlot() que simula como as pessoas com diferentes tipos de daltonismo veem as cores.

# como aplicação, vamos construir um mapa de árvore que mostrará quantos municípios cada estado brasileiro possui além de agrupar os estados por região; depois vamos usar a função cvdPlot() para simular como as pessoas com diferentes tipos de daltonismo veem as cores do nosso gráfico.

library(colorBlindness)
library(ggplot2)
library(dplyr)
library(treemap)
library(treemapify)
library(geobr)
library(sf)

municipios <- read_municipality(year = 2020)

municipios <- municipios |>
  st_drop_geometry()

municipios <- municipios |>
  group_by(name_region, abbrev_state) |>
  summarise(total = n(), .groups = "drop")


meu_treemap <- ggplot(municipios, aes(area = total, fill = name_region, subgroup = name_region,
                                      label = abbrev_state))+
  geom_treemap(colour = "white", size = 1)+
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  geom_treemap_text(colour = "white", place = "topleft", size = 10) +
  labs(fill = "região")

cvdPlot(meu_treemap)

# base

base_treemap <- ggplot(municipios, aes(area = total, fill = name_region, subgroup = name_region,
                                       label = abbrev_state))+
  geom_treemap(colour = "white", size = 1)+
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  geom_treemap_text(colour = "white", place = "topleft", size = 10) + 
  labs(fill = "região")

# paleta wong

# Paleta segura para daltônicos com 5 cores (uma por região)
cores_seguras <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

meu_treemap1 <- base_treemap +
  scale_fill_manual(values = cores_seguras)

cvdPlot(meu_treemap1)  # verificar resultado

# paleta de Okabe e Ito

library(ggthemes)

meu_treemap2 <- base_treemap + 
  scale_fill_colorblind() 

cvdPlot(meu_treemap2)


# a biblioteca viridis
library(viridis)

meu_treemap3 <- base_treemap +
  scale_fill_viridis_d(option = "plasma")   # opções: "viridis", "plasma", "magma", "turbo"

meu_treemap3

cvdPlot(meu_treemap3)

# khroma

library(khroma)

base_treemap + scale_fill_bright()

cvdPlot(base_treemap + scale_fill_vibrant())
