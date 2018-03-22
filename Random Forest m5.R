variaveis <- c('ST20.BRL','ST60.BRL','ST120.BRL',
               'ST20.EMFX','ST60.EMFX','ST120.EMFX',
               'ST20.VIX','ST60.VIX','ST120.VIX',
               'ST20.VOL_BRL','ST60.VOL_BRL','ST120.VOL_BRL',
               'ST120.CCI','ST60.CCI', 'ST20.CCI',
               'ST20.BMAT_SPX','ST60.BMAT_SPX','ST120.BMAT_SPX',
               'ST20.IBX','ST60.IBX','ST120.IBX',
               'ST20.FERRO','ST60.FERRO','ST120.FERRO',
               'ST20.GT10','ST60.GT10','ST120.GT10')

X.pure <- as.matrix(xts_x[,variaveis])
Y.pure <- as.factor(xts_m[,'lead_m5'])

X <- X.pure[which(Y.pure != ""),]
Y <- Y.pure[which(Y.pure != "")]
Y <- droplevels(Y)

X <- X[120:nrow(X),]
Y <- Y[120:length(Y)]

## Loop para limpar NAs
for (v in variaveis){ 
  Y <- Y[which(!is.na(X[,v]))]
  X <- X[which(!is.na(X[,v])),]
}

## definindo a janela de teste 't'
t <- 250 #dias
X_train <- X[1:(nrow(X)-t),]
X_test <- X[(nrow(X)-t+1):nrow(X),]

Y_train <- Y[1:(length(Y)-t)]
Y_test <- Y[(length(Y)-t+1):length(Y)]

rf.m5 <- randomForest(x = X_train, y = Y_train , mtry = round(sqrt(length(variaveis)),0), ntree = 300)
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
varImpPlot(rf.m5)

<<<<<<< HEAD
X_test <- rbind(X_test,X.pure[which(Y.pure == ""),]) 
#X_test <- na.omit(X.pure)
=======
X_test <- rbind(X_test,X.pure[which(Y.pure == ""),])
>>>>>>> 6888b4af2e53ca6ec666db3c3905b607b8f5908e
rf.m5.pred <- predict(rf.m5,newdata = X_test)

#P&L modelo
trade_idea <- ifelse(rf.m5.pred == 'DOWN',-1,
                     ifelse(rf.m5.pred == 'UP',1,0))
trade_idea <- xts(trade_idea, order.by = as.Date(rownames(X_test)))
backtest <- merge(trade_idea, brl , all = c(TRUE,FALSE))
backtest$pnl <- lag(backtest$trade_idea)*diff(brl)*10000
backtest$pnl[1] <- 0
backtest$pnl_accumulated <- 0
backtest$pnl_accumulated <- cumsum(backtest$pnl)
print(tail(backtest,10))

pnl.plot <- plot(backtest$pnl_accumulated, grid.col = 'ghostwhite', main = '')
print(pnl.plot)
title('P&L Acc. - Test Period',cex.main = 1.25)

plot(as.zoo(backtest$brl),
     xlab = '',
     ylab = '')
<<<<<<< HEAD
par(new=TRUE)
plot(as.zoo(backtest$pnl_accumulated),
     col=2,
     bty='n',
     xaxt="n",
     yaxt="n",
=======
par(new=TRUE)               
plot(as.zoo(backtest$pnl_accumulated),
     col=2,
     bty='n',               
     xaxt="n",               
     yaxt="n",              
>>>>>>> 6888b4af2e53ca6ec666db3c3905b607b8f5908e
     xlab="", ylab="")

axis(4, las=1)

<<<<<<< HEAD
legend("topleft",
       legend=c("BRL (L)","P&L(R)"),
       col=1:2,
       lty=1,
=======
legend("topleft",           
       legend=c("BRL (L)","P&L(R)"), 
       col=1:2,
       lty=1,              
>>>>>>> 6888b4af2e53ca6ec666db3c3905b607b8f5908e
       cex=0.65)


rm(confusion.table,down_error,flat_error,up_error,
   X,X_train,Y,Y_train,variaveis,v,t,test_error)

