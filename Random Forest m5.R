# variaveis <- c('ST120.BRL','ST60.BRL','ST20.BRL',
#                'ST20.SPX','ST60.SPX','ST120.SPX',
#                'ST120.VIX','ST120.VOL_BRL',
#                'ST120.CCI','ST60.CCI', 'ST20.CCI',
#                'ST20.BMAT_SPX','ST60.BMAT_SPX','ST120.BMAT_SPX',
#                'ST20.IBX','ST60.IBX','ST120.IBX',
#                'ST20.EMFX','ST60.EMFX','ST120.EMFX')

variaveis <- c('ST20.BRL','ST60.BRL','ST120.BRL',
               'ST20.EMFX','ST60.EMFX','ST120.EMFX',
               'ST120.VIX','ST120.VOL_BRL',
               'ST120.CCI','ST60.CCI', 'ST20.CCI',
               'ST20.BMAT_SPX','ST60.BMAT_SPX','ST120.BMAT_SPX',
               'ST20.IBX','ST60.IBX','ST120.IBX',
               'ST20.FERRO','ST60.FERRO','ST120.FERRO',
               'ST20.GT10','ST60.GT10','ST120.GT10')

X <- as.matrix(xts_x[,variaveis])
Y <- as.factor(xts_m[,'lead_m5'])

X <- X[which(Y != ""),]
Y <- Y[which(Y != "")]
Y <- droplevels(Y)

X <- X[120:nrow(X),]
Y <- Y[120:length(Y)]

## Loop para limpar NAs
for (v in variaveis){ 
  Y <- Y[which(!is.na(X[,v]))]
  X <- X[which(!is.na(X[,v])),]
}

## definindo a janela de teste 't'
t <- 180 #dias
X_train <- X[1:(nrow(X)-t),]
X_test <- X[(nrow(X)-t+1):nrow(X),]

Y_train <- Y[1:(length(Y)-t)]
Y_test <- Y[(length(Y)-t+1):length(Y)]

rf.m5 <- randomForest(x = X_train, y = Y_train , mtry = 5, ntree = 300)
print('Error - TRAIN DATA')
print(rf.m5$confusion)

rf.m5.pred <- predict(rf.m5,newdata = X_test)
confusion.table <- table(prev = rf.m5.pred, test = Y_test)
down_error <- 1-round(confusion.table['DOWN','DOWN']/sum(confusion.table['DOWN',]),2)
flat_error <- 1-round(confusion.table['FLAT','FLAT']/sum(confusion.table['FLAT',]),2)
up_error <- 1-round(confusion.table['UP','UP']/sum(confusion.table['UP',]),2)
test_error <- c(down_error,flat_error,up_error)
test_error <- as.matrix(test_error)
rownames(test_error) <- c('DOWN','FLAT','UP')

print('Error - TEST DATA')
print(confusion.table)
print(test_error)
print(varImpPlot(rf.m5))
