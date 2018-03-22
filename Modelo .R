setwd("/Users/aleschwartzman/Documents/Big Data/Modelo BRL") #estabelecendo o diretório de trab
source('Initialize.R')
#source('BBG download.R)
#source('Charts.R')
source('Data Structure.R')
source('Random Forest m5.R')

# MODELO ESCOLHIDO --------------------------------------------------------

modelo_escolhido <- rf.m5
variaveis_escolhidas <- variaveis
today_input <- tail(df_x[,variaveis_escolhidas],1)

previsao <- predict(modelo_escolhido,today_input)

ifelse(previsao == 'UP','UP - Buy USDBRL',
        ifelse(previsao =='DOWN','DOWN - Sell USDBRL',
       'FLAT - Stay flat USDBRL'))
        
varImpPlot(modelo_escolhido)
round(summary(modelo_escolhido$predicted)/n,2)

base$previsao <- modelo_escolhido$predicted



# BackTest Modelo escolhido -----------------------------------------------

tail(base)
class(rownames(base))
