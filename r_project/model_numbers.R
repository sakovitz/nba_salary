# cout disttinctpara saber quantos jogadores tenho na amostra:
library(dplyr)


# Filtrando os dados para o ano de 2017 e contando jogadores distintos
num_jogadores_distintos <- season %>%
  filter(Year == 2017) %>%
  summarise(num_distinct_players = n_distinct(Player))


num_jogadores_distintos
