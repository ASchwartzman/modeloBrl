rm(list=ls());cat("\014") #limpando variaveis e graficos
graphics.off() #limpando graficos

## baixando os pacotes
pacotes <- c('dplyr','ggplot2','xts','TTR', 'randomForest','keras')
lapply(pacotes, require, character.only = TRUE)

## baixando objetos
objetos <- c('function_Return.R','Series.RData')
lapply(objetos, load, .GlobalEnv)

rm(pacotes,objetos)

