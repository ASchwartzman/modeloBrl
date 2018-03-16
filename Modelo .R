setwd("/Users/aleschwartzman/Documents/Big Data/Modelo BRL") #estabelecendo o diretório de trab
source('Initialize.R')
#source('BBG download.R)
source('Charts.R')
source('Data Structure.R')

# ANALISE EXPLORATORIA ----------------------------------------------------

base <- merge(df_y,df_x, by = 'row.names')
base$return_lead1 <- lead(base$return1,1)
base$return_lead5 <- lead(base$return5,5)
base$move_lead1 <- lead(base$move,1)
base$move_lead5 <- lead(base$move5,5)
base$brl <- NULL
base$return1 <- NULL
base$return5 <- NULL
base$move <- NULL
base$move5 <- NULL
base$Row.names <- NULL


dim(base)
tail(base,6)
str(base)

base <- na.omit(base)
cor <- cor(base[,1:44])
sort(cor[,44], decreasing = TRUE)
sort(cor[,43], decreasing = TRUE)


# PRIMEIROS MODELOS -------------------------------------------------------
variaveis <- c('ST20.SPX','ST120.VIX','ST120.CCI','ST20.BMAT_SPX')
X <- as.matrix(base[,variaveis])
Y <- as.matrix(base[,'return_lead5'])
           
lm.r1.v1 <- lm('Y~X') 
summary.lm.v1 <- summary(lm.r1.v1)

plot(Y, type = 'l', main = 'Linear Model')
lines(lm.r1.v1$fitted.values, col = 2)

rf.r1.v1 <- randomForest(x = X, y = Y , mtry = 3, ntree = 300)
plot(Y, type = 'l', main = 'Random Forest')
lines(rf.r1.v1$predicted, col = 2)

#plot(Y,rf.r1.v1)
#plot(Y,rf.r1.v1$predicted)


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
