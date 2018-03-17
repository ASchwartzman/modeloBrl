
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

