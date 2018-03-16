library(Rblpapi)

blpConnect() 
initial.date <- Sys.Date()-10*365
end.date <- Sys.Date()

ativos = list(brl = 'BRL Curncy',
              cci = 'CCI Index',
              emfx = '.EMERGCUR Index',
              cds = 'BRAZIL CDS 5 YR Curncy',
              ibx = 'IBX Index',
              spx = 'SPX Index',
              gt10 = 'USGG10Y Index',
              wti = 'CL1 Comdty',
              soy = 'S 1 Comdty',
              corn = 'C 1 Comdty',
              ferro = 'TIO3 Comdty',
              bmat_spx = '.BMAT-SPX Index',
              vix = 'VIX Index',
              vol_brl = 'USDBRLV1M Curncy',
              carry_brl = '.CARRYBRL Index')

series <- list()
i <- 1

## Downloading series into a list...
for (ativo in ativos){
  print(paste('Downloading',names(ativos[i]),'...'))
  ts_x <- bdh(ativo,"PX_LAST", start.date = initial.date)
  ts_x <- xts(ts_x[,-1], order.by = ts_x[,1])
  series[[names(ativos[i])]] <- ts_x
  i <- i+1
  rm(ts_x,ativo)
}

## Creating MA's components for each series (remember to insert the function substr in bps cases)
for (i in 1:length(series)){
  serie <- series[[i]]
  MA20 <- SMA(serie[,1],20)
  MA60 <- SMA(serie[,1],60)
  MA120 <- SMA(serie[,1],120)
  serie <- merge(serie, MA20, MA60, MA120)
  colnames(serie) <- c(names(series[i]),'MA20','MA60','MA120')

  ## INSERT THE SUBSTR() HERE!!1
  if (names(series[i]) == 'cds'|| names(series[i]) == 'gt10' || names(series[i]) == 'vix' ||names(series[i]) == 'vol_brl' ){
    ST20 <- serie[,1]-serie$MA20
    ST60 <- serie[,1]-serie$MA60
    ST120 <- serie[,1]-serie$MA120
  } else {
    ST20 <- serie[,1]/serie$MA20
    ST60 <- serie[,1]/serie$MA60
    ST120 <- serie[,1]/serie$MA120
  }
  
  serie <- merge(serie, ST20, ST60, ST120)
  colnames(serie) <- c(names(series[i]),'MA20','MA60','MA120',
                       'ST20','ST60','ST120')
  series[[i]] <- serie
  rm('serie','MA20','MA60','MA120',
     'ST20','ST60','ST120')
}

save(series, file = 'Series.RData')