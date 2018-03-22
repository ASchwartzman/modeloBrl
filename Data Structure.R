## Creating Y xts base (BRL) 
## Remember that all series objects are xts, they only accept same type objects
brl<-series$brl[,1]
r_1 <- round(return(brl,1)*100,2) # BRL 1 day return
r_5 <- round(return(brl,5)*100,2) # BRL 5 day return

threshold_m1 <- 0.35 # BRL 1 day move (factor)
m_1 <- xts(as.factor(ifelse(r_1 < -threshold_m1,'DOWN',  
                              ifelse(r_1 > threshold_m1, 'UP','FLAT'))), 
                 order.by = index(brl))
## 1d move distribution
print(paste('Threshold 1d move:', threshold_m1))
print(round(table(m_1)/nrow(m_1),2))

threshold_m5 <- 0.75 # BRL 5 day move (factor)
m_5 <- xts(as.factor(ifelse(r_5 < -threshold_m5,'DOWN',
                              ifelse(r_5 > threshold_m5, 'UP','FLAT'))),
                 order.by = index(brl))
## 5d move distribution
print(paste('Threshold 5d move:', threshold_m5))
print(round(table(m_5)/nrow(m_5),2))


xts_r <- merge(r_1,r_5) ; colnames(xts_r) <- c('r1','r5')
xts_r$lead_r1 <- Lag(xts_r$r1, shift = -1)
xts_r$lead_r5 <- Lag(xts_r$r5, shift = -5)

             
xts_m <- merge(m_1,m_5) ; colnames(xts_m) <- c('m1','m5')
xts_m$lead_m1 <- Lag(xts_m$m1, shift = -1)
xts_m$lead_m5 <- Lag(xts_m$m5, shift = -5)


## Creating X xts base (ST20 / ST60 / ST120) for all variables 
xts_x <- xts(NULL, order.by = index(series[[1]]))
for (i in 1:length(series)){
  
  serie <- series[[i]]
  name_serie <- toupper(names(series[i]))
  
  xts <- serie[,5:7]
  colnames(xts) <- paste(colnames(xts),name_serie)
  
  xts_x <- merge(xts_x,xts)
  rm(serie,name_serie,xts)
}

### É ESSE o CAMINHO
xts_x <- xts_x[index(xts_m)]

rm(i,threshold_m1,threshold_m5,
   m_1,m_5,r_1,r_5)
