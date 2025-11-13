library(tidyverse)

df <- read.table("aula1.txt", sep = ";",
                 header = TRUE)

glimpse(df)


retangulo1 <- data.frame(x1  = 1.5,
                         x2 = 4,
                         y1 = 0.9,
                         y2 = 1)

retangulos <- data.frame(x1 = c(1.5, 5.2),
                         x2 = c(4, 8.4),
                         y1 = c(0.9, 0.58),
                         y2 = c(1,0.95))

ggplot() +
  geom_point(data = df, mapping = aes(x = fertilidade, y = taxa_sobrevivencia, size = populacao_numerica), alpha = 0.6) +
  geom_rect(data  = retangulos, aes(xmin = x1, ymin = y1,
                                    xmax = x2, ymax = y2),
            alpha = 0, color = "black") +
  scale_size(range = (c(2,20))) +
  scale_x_continuous(breaks = 1:8)+
  scale_y_continuous(breaks = seq(from = 0.5, to = 1, by = 0.1),
                     labels = paste0(seq(from = 0.5, to = 1, by = 0.1)*100, "%")) +
  annotate("text", x = 2.75, y = 0.6, label = "1965", size = 15, alpha = 0.3, vjust = 0) +
  annotate("text", y = 1.008, x = 2.75,
           label = "DESENVOLVIDOS",
           fontface = "bold",
           size = 6,
           color = "darkgrey") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs(x = "Número médio de filhos por mulher",
       y = "Proporção de crianças que sobrevivem até os 5 anos")
  

