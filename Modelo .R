setwd("/Users/aleschwartzman/Documents/Big Data/Modelo BRL") #estabelecendo o diretório de trab
source('Initialize.R')
#source('BBG download.R)
source('Data Structure.R')
source('Charts.R')

# PRIMEIROS MODELOS -------------------------------------------------------
variaveis <- c('ST120.BRL','ST60.BRL','ST20.BRL',
               'ST20.SPX','ST60.SPX','ST120.SPX',
               'ST120.VIX','ST120.VOL_BRL',
               'ST120.CCI','ST60.CCI', 'ST20.CCI',
               'ST20.BMAT_SPX','ST60.BMAT_SPX','ST120.BMAT_SPX',
               'ST20.IBX','ST60.IBX','ST120.IBX',
               'ST20.EMFX','ST60.EMFX','ST120.EMFX')
n <- 4*252
base <- tail(base,n)

X <- as.matrix(base[,variaveis])
Y <- as.factor(base[,'move_lead5'])

rf.m5 <- randomForest(x = X, y = Y , mtry = 5, ntree = 300)
rf.m5$confusion

Y <- as.factor(base[,'move_lead1'])
rf.m1 <- randomForest(x = X, y = Y , mtry = 3, ntree = 300)
rf.m1$confusion

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
