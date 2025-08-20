# Carregar pacotes
library(tidyverse)

#importação de Dados
fires <- read.csv("data/forestfires.csv")

#carregamento de primeiras linhas
head(fires)

#Estrutura
str(fires)

#Resumo das Estatisticas
summary(fires)

#Conversão de Meses e dias em Categorias
fires$month <- factor(fires$month, levels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
fires$day <- factor(fires$day, levels=c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))

#Vizualização da Frequencia de incendios por mês

ggplot(fires, aes(x=month)) + 
  #Conta quantas vezes o mês aparece
  geom_bar(fill = "red") + 
  labs(title="Frequência de Incendios de cada mês",
       x="Mês", y="Quantidade de Incêndios") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Boxplot da Aréa queimada
#vai exibir apenas a Area que o incendio foi maior que Zero.
ggplot(fires, aes(x= month, y=area)) +
  geom_boxplot(fill= "green") +
  scale_y_log10() + #Reduz a distorção 
  labs(title = "Distribuição das Queimadas",
       x="Mês", y="Área Queimada") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Criação da relação entre temperatura alta e maior area queimada
#Grafico de Dispersão
ggplot(fires, aes(x=temp, y=area)) +
  geom_point(alpha= 0.5, color="orange") +
  scale_y_log10() +
  labs(title ="Temperatura X Área Queimada", 
       x= "Temperatura (ºC)", y="Área Queimada") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
    

ggplot(fires, aes(x=RH, y=area)) +
  geom_point(alpha = 0.5, color="navyblue") +
  scale_y_log10() +
  labs(title = "Umidade X Área Queimada", 
       x= "Umidade (%)", y="Área Queimada (log)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
library(GGally)
ggpairs(fires[,c("temp","RH","wind","rain","area")]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
