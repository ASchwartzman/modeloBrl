rm(list=ls());cat("\014")
graphics.off()

pacotes <- c('dplyr','ggplot2','xts','Hmisc','TTR', 'randomForest','keras')
lapply(pacotes, require, character.only = TRUE)

objetos <- c('function_Return.R','Series.RData')
lapply(objetos, load, .GlobalEnv)

rm(pacotes,objetos)

