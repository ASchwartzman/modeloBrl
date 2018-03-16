par(mfrow = c(2,1))
years <- 1.75

for (i in length(series):1){
  
  serie <- series[[i]]
  n <- nrow(serie)
  series_name <- names(series[i])
  
  matriz.na <- matrix(NA, ncol = ncol(serie), nrow = 90) 
  matriz.na <- xts(matriz.na, 
                   order.by = seq.Date(from = 1 + index(serie[n,]) , by = 'days', length.out = 90 ))
  colnames(matriz.na) <- colnames(serie)
  
  serie <- rbind(serie, matriz.na)
  n <- nrow(serie)
  
  print (paste('Ploting',series_name))
  print(plot(serie[(n-years*365):n,1:4], grid.col = 'ghostwhite',
             main = ""))
  title(toupper(series_name),cex.main = 1.5)
  rm(serie,series_name,n,matriz.na)
}

rm(list = c('i','years'))
