## Minimo entre.....
data$Min <- with(data, pmin(Parm1, Parm2))

## Agrupa
metric = aggregate(remessa[,c(5:7)],
                   by = list(remessa$data_mes_ano,remessa$faixa_peso),
                   FUN = mean)
## Split
library(tidyr)
Data = exdata

Column Name to be splitted = Month_Date
separate(exdata,Month_Date,into=c( “Month” , ”Date” ),sep=”_”)

### remove caracter
rs<-c("copyright @ The Society of mo","I want you to meet me @ the coffeshop")
s<-gsub("@.*","",rs)


#trocar nome de 1 coluna
colnames(trSamp)[2] <- "newname2"


##
df2$ratio[match(df1$year_quarter, df2$year_quarter)] * df1[-1]

df$gear[df$hp > 150] <- mean(df$gear[df$hp > 150])
tabela$minimo[tabela$tarifa!='SP_C1'] <- tabela$minimo[tabela$tarifa!='SP_C1']*1.15

