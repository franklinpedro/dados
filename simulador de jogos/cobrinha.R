#JOGO DAS COBRINHAS

# 1 JOGO

cobra <- 7
somas_sem_cobra <- c(2:6, 8:12)

meus_resultados <- sum(sample(x = 1:6, size = 2, replace = TRUE))
sem_cobras <- sum(somas_sem_cobra %in% meus_resultados)
cobras <- sum(meus_resultados == 7)
rodadas <- 1

while (sem_cobras < 10 & cobras < 7) {
  soma <- sum(sample(x = 1:6, size = 2, replace = TRUE))
  meus_resultados <- c(meus_resultados, soma)
  sem_cobras <- sum(somas_sem_cobra %in% meus_resultados)
  cobras <- sum(meus_resultados == 7)
  rodadas <- rodadas + 1
}
rodadas
sem_cobras
cobras