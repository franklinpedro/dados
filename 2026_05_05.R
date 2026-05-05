library(ggplot2)
library(dplyr)
library(dbscan)
library(mlbench)
library(ggforce)
library(palmerpenguins)
library(GGally)
library(plotly)

set.seed(42)

espiral <- mlbench.spirals(n = 500, cycles = 0.5, sd = 0.05)

df <- as.data.frame(espiral$x)

ponto_idx <- sample(1:nrow(df), size = 10)
df_alvo   <- df[ponto_idx, ]
epsilon   <- 0.09

ggplot() +
  geom_point(data = df, aes(x = V1, y = V2), alpha = 0.5) +
  geom_circle(aes(x0 = V1, y0 = V2, r = epsilon),
              data = df_alvo, color = "red", linewidth = 0.5) +
  coord_fixed() +
  theme_minimal()

df_cluster <- dbscan(df, eps = 2, minPts = 4)
df_cluster
df$cluster <- as.factor(df_cluster$cluster)

ggplot(df) +
  geom_point(aes(x = V1, y = V2, color = cluster)) +
  theme_minimal()

kNNdistplot(df, k = 4)