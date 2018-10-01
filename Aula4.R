#instalar pacotes
lista.de.pacotes = c("tidyverse","lubridate") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

#ler pacotes
library(tidyverse)
library(magrittr)
library(lubridate)

#ler arquivo decisões.rds
decisoes <- read_rds("C:/Users/aluno.ENAP/Documents/FGS/D6/D6/decisoes.rds")

#ver resumo do arquivo decisões
glimpse(decisoes)

# Exercício 1

# Criar objeto contendo o tempo medio entre decisoes e registro por juiz. Para isso:

#1. Seleciona todas as colunas que serão utilizadas

juizes_drogas_CL <- decisoes %>%
  select(juiz, municipio, txt_decisao, data_registro, data_decisao) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[íi]na"),
         tempo = dmy(data_registro) - dmy(data_decisao)) %>%
  filter(droga ==T, municipio %in% c("Campinas","Limeira")) %>%
  group_by(juiz) %>%
  summarise(tempo_medio = mean(tempo, na.rm=T))

#salve o objeto resultante em um arquivo chamado "juizes_drogas_CL.rds"

write_rds(juizes_drogas_CL,"C:/Users/aluno.ENAP/Documents/FGS/D6/D6/juizes_drogas_CL.rds")

#Exercício 2
glimpse(decisoes)

#Criar dataframe contendo juiz, n_processo_drogas, n_processos_n_drogas e total_processos

juiz_droga <- decisoes %>%
  filter(!is.na(txt_decisao)) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[íi]na"),
         droga = case_when(
           droga==T ~ "droga"
           droga==F ~ "n_droga"
           )) %>%
  group_by(juiz,droga) %>%
  summarise(n=n()) %>%
  spread(droga,n,fill = 0) %>%
  mutate(total=droga+n_droga,
         proporcao=droga/total) %>%
  arrange(desc(proporcao))



